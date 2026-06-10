library(here)
library(fixest)
library(dplyr)
library(ggplot2)

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
FIG_DIR <- here("output", "figures")
TAB_DIR <- here("output", "tables")

panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))

# ---------------------------------------------------------------------------
# Alternative treatment: prop_apr — share of rural dwellings in the school's
# APR polygon with piped water (red pública). Constructed from CENSO 2024
# Entidades_CPV24 × APR polygon spatial intersection (script 14_censo_coverage.R).
# Time-invariant (single census snapshot).
#
# Three complementary specifications:
#
#  A. Dose-response TWFE: D_cont = treated × prop_apr
#       Effect = impact when prop_apr rises by 1 (full-coverage equivalent).
#       Schools with prop_apr = 0.6 get 60% of the "full" treatment effect.
#
#  B. Interaction TWFE: treated + treated:prop_apr
#       Decomposes into a base effect (prop_apr = 0) and a marginal effect
#       of additional coverage. Main effect of prop_apr absorbed by school FE.
#
#  C. Dose-response event study: i(rel_year_bin, prop_apr, ref = -1)
#       Shows how the coverage gradient evolves pre- and post-installation.
#       Pre-treatment coefficients near 0 = parallel trends by coverage.
#
#  D. Tercile robustness: ATT estimated separately for low/mid/high prop_apr
#       Non-parametric dose-response check using binary treated in each tier.
# ---------------------------------------------------------------------------

panel_cont <- panel |>
  group_by(rbd) |>
  mutate(ever_treated = any(treated == 1, na.rm = TRUE)) |>
  ungroup() |>
  filter(ever_treated | rural_rbd == 1) |>
  mutate(
    log_den_total       = log(n_den_total       + 1),
    log_den_convivencia = log(n_den_convivencia + 1),
    log_den_abuso       = log(n_den_abuso       + 1),
    # Continuous treatment: intensity × post-treatment indicator
    D_cont = treated * prop_apr,
    # Relative year bins for event study (same as 01_twfe.R)
    rel_year = if_else(!is.na(anio_instalacion),
                       year - anio_instalacion, NA_real_),
    rel_year_bin = case_when(
      is.na(rel_year)  ~ NA_real_,
      rel_year <= -5   ~ -5,
      rel_year >= 5    ~  5,
      TRUE             ~  rel_year
    )
  )

OUTCOMES <- c(
  "prom_lect", "prom_mate",
  "tasa_asistencia",
  "tasa_repitencia", "tasa_desercion",
  "log_den_total", "log_den_convivencia", "log_den_abuso"
)

controls <- "dias_calor + deficit_precip + log(n_matricula + 1)"

# ---------------------------------------------------------------------------
# Part A: Dose-response static TWFE
# D_cont = treated * prop_apr
# Both cluster-by-RBD and Conley 50km SEs (matches 01_twfe.R structure)
# ---------------------------------------------------------------------------
run_cont <- function(y, vcov_type) {
  fml <- as.formula(paste(y, "~ D_cont +", controls, "| rbd + year"))
  df  <- panel_cont |> filter(!is.na(.data[[y]]))
  if (vcov_type == "cluster") {
    feols(fml, data = df, cluster = ~rbd, warn = FALSE, notes = FALSE)
  } else {
    feols(fml, data = df,
          vcov = conley(lat = "lat", lon = "lon", cutoff = 50),
          warn = FALSE, notes = FALSE)
  }
}

cont_cluster <- lapply(OUTCOMES, function(y) run_cont(y, "cluster"))
cont_conley  <- lapply(OUTCOMES, function(y) run_cont(y, "conley"))
names(cont_cluster) <- OUTCOMES
names(cont_conley)  <- OUTCOMES

for (y in OUTCOMES) {
  etable(list(cont_cluster[[y]], cont_conley[[y]]),
         headers = c("Cluster RBD", "Conley 50km"),
         file = file.path(TAB_DIR, paste0("cont_static_", y, ".tex")),
         replace = TRUE)
}

# Joint table across all outcomes (cluster only) — main paper table
etable(cont_cluster,
       file = file.path(TAB_DIR, "cont_static_all.tex"),
       replace = TRUE)

# ---------------------------------------------------------------------------
# Part B: Interaction TWFE
# treated + treated:prop_apr | rbd + year
# prop_apr main effect absorbed by school FE (time-invariant)
# treated coef = base effect; treated:prop_apr coef = marginal effect of coverage
# ---------------------------------------------------------------------------
run_interact <- function(y) {
  fml <- as.formula(paste(
    y, "~ treated + treated:prop_apr +", controls, "| rbd + year"
  ))
  feols(fml, data = panel_cont |> filter(!is.na(.data[[y]])),
        cluster = ~rbd, warn = FALSE, notes = FALSE)
}

interact_results <- lapply(OUTCOMES, run_interact)
names(interact_results) <- OUTCOMES

etable(interact_results,
       file = file.path(TAB_DIR, "cont_interact_all.tex"),
       replace = TRUE)

# ---------------------------------------------------------------------------
# Part C: Dose-response event study
# i(rel_year_bin, prop_apr, ref = -1): coefficient at each time bin gives the
# marginal effect of a 1-unit increase in prop_apr at that relative year.
# Pre-treatment coefficients near 0 = coverage does not predict pre-trends.
# ---------------------------------------------------------------------------
run_cont_es <- function(y) {
  fml <- as.formula(paste(
    y, "~ i(rel_year_bin, prop_apr, ref = -1) +", controls, "| rbd + year"
  ))
  feols(fml, data = panel_cont |> filter(!is.na(.data[[y]]), !is.na(rel_year_bin)),
        cluster = ~rbd, warn = FALSE, notes = FALSE)
}

cont_es <- lapply(OUTCOMES, run_cont_es)
names(cont_es) <- OUTCOMES

for (y in OUTCOMES) {
  png(file.path(FIG_DIR, paste0("eventstudy_cont_", y, ".png")),
      width = 800, height = 500)
  iplot(cont_es[[y]],
        main  = paste("Dose-response event study —", y),
        xlab  = "Years since APR installation",
        ylab  = "Marginal effect per unit prop_apr")
  abline(v = -0.5, lty = 2, col = "gray50")
  dev.off()
}

# ---------------------------------------------------------------------------
# Part D: Tercile robustness — non-parametric dose-response
# Split treated schools by prop_apr tercile; estimate binary ATT in each tier
# vs. the same rural control group. Shows the dose-response non-parametrically.
# ---------------------------------------------------------------------------

# Tercile breakpoints based on treated schools only
treated_coverage <- panel_cont |>
  filter(ever_treated) |>
  distinct(rbd, prop_apr) |>
  pull(prop_apr)

q1 <- quantile(treated_coverage, 1/3, na.rm = TRUE)
q2 <- quantile(treated_coverage, 2/3, na.rm = TRUE)

panel_cont <- panel_cont |>
  mutate(
    apr_tier = case_when(
      !ever_treated   ~ "control",
      prop_apr <= q1  ~ "low_cov",
      prop_apr <= q2  ~ "mid_cov",
      TRUE            ~ "high_cov"
    )
  )

message("Coverage tercile breakpoints: q1 = ", round(q1, 3), ", q2 = ", round(q2, 3))
message("Tier sizes (treated schools):")
panel_cont |>
  filter(ever_treated) |>
  distinct(rbd, apr_tier) |>
  count(apr_tier) |>
  print()

run_tercile <- function(y, tier) {
  df <- panel_cont |>
    filter(apr_tier %in% c("control", tier), !is.na(.data[[y]]))
  fml <- as.formula(paste(y, "~ treated +", controls, "| rbd + year"))
  feols(fml, data = df, cluster = ~rbd, warn = FALSE, notes = FALSE)
}

tercile_results <- lapply(OUTCOMES, function(y) {
  list(
    low  = run_tercile(y, "low_cov"),
    mid  = run_tercile(y, "mid_cov"),
    high = run_tercile(y, "high_cov")
  )
})
names(tercile_results) <- OUTCOMES

for (y in OUTCOMES) {
  etable(
    list(tercile_results[[y]]$low,
         tercile_results[[y]]$mid,
         tercile_results[[y]]$high),
    headers = c("Low coverage", "Mid coverage", "High coverage"),
    file = file.path(TAB_DIR, paste0("cont_tercile_", y, ".tex")),
    replace = TRUE)
}

# ---------------------------------------------------------------------------
# Dose-response summary plot: ATT by tercile for key outcomes
# ---------------------------------------------------------------------------
key_outcomes <- c("prom_lect", "prom_mate", "tasa_asistencia", "tasa_desercion")

coef_df <- lapply(key_outcomes, function(y) {
  lapply(c("low_cov", "mid_cov", "high_cov"), function(tier) {
    m <- run_tercile(y, tier)
    data.frame(
      outcome  = y,
      tier     = tier,
      estimate = coef(m)["treated"],
      se       = se(m)["treated"]
    )
  }) |> bind_rows()
}) |> bind_rows() |>
  mutate(
    tier    = factor(tier, levels = c("low_cov", "mid_cov", "high_cov"),
                     labels = c("Low\ncoverage", "Mid\ncoverage", "High\ncoverage")),
    ci_lo   = estimate - 1.96 * se,
    ci_hi   = estimate + 1.96 * se
  )

p_dose <- ggplot(coef_df, aes(x = tier, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2) +
  geom_point(size = 2.5) +
  facet_wrap(~outcome, scales = "free_y", nrow = 1) +
  labs(
    title = "Non-parametric dose-response: ATT by APR coverage tercile",
    x     = "APR coverage tier (prop_apr)",
    y     = "Estimated ATT (cluster SE)"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(FIG_DIR, "dose_response_tercile.png"),
       p_dose, width = 12, height = 4)

saveRDS(
  list(static    = cont_cluster,
       interact  = interact_results,
       eventstudy = cont_es,
       tercile   = tercile_results),
  file.path(TAB_DIR, "continuous_treatment_results.rds")
)

message("Continuous treatment analysis complete.")
message("  Tables → ", TAB_DIR)
message("  Figures → ", FIG_DIR)
