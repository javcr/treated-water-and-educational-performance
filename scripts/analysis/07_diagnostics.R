library(here)
library(fixest)
library(dplyr)
library(ggplot2)
library(bacondecomp)  # install.packages("bacondecomp")
library(HonestDiD)    # install.packages("HonestDiD")

# Purpose: econometric diagnostic tests for the staggered DiD design.
# Not for the informe de avance — for the paper methodology/appendix.
# (1) Goodman-Bacon decomposition: weights of clean vs. dirty TWFE comparisons
# (2) Formal Wald pre-trends test: joint H0 that all pre-treatment coefficients = 0
# (3) Rambachan & Roth (2023) HonestDiD: sensitivity to parallel trends violations

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
FIG_DIR <- here("output", "figures")
TAB_DIR <- here("output", "tables")

panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))

panel_did <- panel |>
  group_by(rbd) |>
  mutate(
    ever_treated = any(treated == 1, na.rm = TRUE),
    gname = if_else(ever_treated, min(year[treated == 1], na.rm = TRUE), 0L)
  ) |>
  ungroup() |>
  filter(ever_treated | rural_rbd == 1) |>
  mutate(rel_year_bin = case_when(
    is.na(gname) | gname == 0 ~ NA_real_,
    year - gname <= -5        ~ -5,
    year - gname >=  5        ~  5,
    TRUE                      ~  as.numeric(year - gname)
  ))

controls <- "dias_calor + deficit_precip + log(n_matricula + 1)"

OUTCOMES <- c("prom_lect", "prom_mate", "tasa_asistencia",
              "tasa_repitencia", "tasa_desercion")

# ---------------------------------------------------------------------------
# 1. Goodman-Bacon decomposition
#    Decomposes the TWFE coefficient into weighted 2x2 DiDs:
#      - Treated vs. never-treated (clean)
#      - Early-treated vs. late-treated (potentially contaminated)
#    A large weight on "earlier vs. later treated" suggests TWFE bias.
# ---------------------------------------------------------------------------
bacon_results <- lapply(OUTCOMES, function(y) {
  df <- panel_did |>
    filter(!is.na(.data[[y]]), !is.na(gname)) |>
    mutate(D = as.integer(treated == 1))

  tryCatch({
    bd <- bacon(as.formula(paste(y, "~ D | rbd + year")),
                data = as.data.frame(df),
                id_var = "rbd", time_var = "year")
    message(y, " — Bacon decomposition done. ",
            "Weight on never-treated: ",
            round(sum(bd$weight[bd$type == "Treated vs Untreated"]), 3))
    bd
  }, error = function(e) {
    message(y, " — Bacon decomposition failed: ", e$message)
    NULL
  })
})
names(bacon_results) <- OUTCOMES

# Plot decomposition for primary outcomes
for (y in c("prom_lect", "tasa_asistencia")) {
  bd <- bacon_results[[y]]
  if (is.null(bd)) next
  p <- ggplot(bd, aes(x = weight, y = estimate, color = type, shape = type)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = paste("Goodman-Bacon decomposition —", y),
         subtitle = "Each point is a 2x2 DiD; size ~ weight in TWFE estimate",
         x = "Weight", y = "2x2 DiD estimate",
         color = "Comparison type", shape = "Comparison type") +
    theme_minimal()
  ggsave(file.path(FIG_DIR, paste0("bacon_", y, ".png")),
         p, width = 8, height = 5)
}

saveRDS(bacon_results, file.path(TAB_DIR, "bacon_results.rds"))

# ---------------------------------------------------------------------------
# 2. Formal Wald pre-trends test
#    Joint H0: all pre-treatment event-study coefficients = 0.
#    Rejection → evidence against parallel trends.
# ---------------------------------------------------------------------------
wald_results <- lapply(OUTCOMES, function(y) {
  fml <- as.formula(paste(
    y, "~ i(rel_year_bin, ref = -1) +", controls, "| rbd + year"
  ))
  m <- feols(fml, data = panel_did |> filter(!is.na(.data[[y]])),
             cluster = ~rbd, warn = FALSE, notes = FALSE)

  # Identify pre-treatment coefficients (rel_year_bin < -1)
  pre_coefs <- grep("rel_year_bin::-[2-9]|rel_year_bin::-[1-9][0-9]",
                    names(coef(m)), value = TRUE)

  if (length(pre_coefs) == 0) {
    message(y, " — No pre-treatment coefficients found")
    return(NULL)
  }

  w <- wald(m, keep = pre_coefs)
  message(y, " — Wald pre-trends test: F = ", round(w$stat, 3),
          ", p = ", round(w$p, 4))
  list(model = m, wald = w, pre_coefs = pre_coefs)
})
names(wald_results) <- OUTCOMES

wald_tab <- lapply(OUTCOMES, function(y) {
  if (is.null(wald_results[[y]])) return(NULL)
  w <- wald_results[[y]]$wald
  data.frame(outcome = y,
             F_stat  = round(w$stat, 3),
             df1     = w$df1,
             df2     = round(w$df2, 1),
             p_value = round(w$p, 4))
}) |> bind_rows()

write.csv(wald_tab, file.path(TAB_DIR, "wald_pretrends.csv"), row.names = FALSE)
message("Wald pre-trends test saved.")

# ---------------------------------------------------------------------------
# 3. Rambachan & Roth (2023) — HonestDiD
#    Sensitivity analysis: how large can pre-trend violations be (relative
#    to observed pre-trends) before the estimated ATT is no longer significant?
#    Uses the "relative magnitudes" restriction (M-type).
# ---------------------------------------------------------------------------
honest_results <- lapply(OUTCOMES, function(y) {
  res <- wald_results[[y]]
  if (is.null(res)) return(NULL)
  m <- res$model

  # Extract event-study coefficients and vcov (excluding reference period -1)
  es_coefs <- coef(m)[grep("rel_year_bin", names(coef(m)))]
  es_vcov  <- vcov(m)[grep("rel_year_bin", rownames(vcov(m))),
                       grep("rel_year_bin", colnames(vcov(m)))]

  # Identify pre vs. post indices (rel_year < -1 = pre; >= 0 = post)
  # rel_year_bin values present (sorted)
  rnames <- names(es_coefs)
  vals   <- as.numeric(gsub(".*::", "", rnames))
  pre_idx  <- which(vals < -1)
  post_idx <- which(vals >= 0)

  if (length(pre_idx) == 0 || length(post_idx) == 0) return(NULL)

  tryCatch({
    sensitivity <- createSensitivityResults_relativeMagnitudes(
      betahat        = es_coefs[post_idx],
      sigma          = es_vcov[post_idx, post_idx],
      numPrePeriods  = length(pre_idx),
      numPostPeriods = length(post_idx),
      Mbarvec        = seq(0.5, 2, by = 0.5)   # M = 0.5 to 2x max pre-trend
    )

    original_cs <- constructOriginalCS(
      betahat        = es_coefs[post_idx],
      sigma          = es_vcov[post_idx, post_idx],
      numPrePeriods  = length(pre_idx),
      numPostPeriods = length(post_idx)
    )

    p <- createSensitivityPlot_relativeMagnitudes(sensitivity, original_cs) +
      labs(title = paste("HonestDiD sensitivity —", y),
           subtitle = "M = max pre-trend violation allowed relative to observed")

    ggsave(file.path(FIG_DIR, paste0("honestdid_", y, ".png")),
           p, width = 8, height = 5)

    message(y, " — HonestDiD done.")
    sensitivity
  }, error = function(e) {
    message(y, " — HonestDiD failed: ", e$message)
    NULL
  })
})
names(honest_results) <- OUTCOMES

saveRDS(list(bacon = bacon_results,
             wald  = wald_tab,
             honestdid = honest_results),
        file.path(TAB_DIR, "diagnostics_results.rds"))

message("=== Diagnostics complete. Outputs → ", TAB_DIR, " | ", FIG_DIR)
