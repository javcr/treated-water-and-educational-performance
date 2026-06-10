library(here)
library(fixest)
library(dplyr)

OUT_DIR  <- here("data", "A_raw", "apr_ddbb")
FIG_DIR  <- here("output", "figures")
TAB_DIR  <- here("output", "tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))

# Keep only schools ever treated or rural untreated
# (schools inside APR coverage area at some point)
# → more homogeneous control group, reduces selection bias
panel_did <- panel |>
  group_by(rbd) |>
  mutate(ever_treated = any(treated == 1, na.rm = TRUE)) |>
  ungroup() |>
  filter(ever_treated | rural_rbd == 1)   # treated + rural untreated

# Years since treatment for event study
panel_did <- panel_did |>
  mutate(
    rel_year = if_else(!is.na(anio_instalacion),
                       year - anio_instalacion,
                       NA_real_)
  )

# Log-transform violence outcomes (right-skewed count variables)
panel_did <- panel_did |>
  mutate(
    log_den_total      = log(n_den_total      + 1),
    log_den_convivencia = log(n_den_convivencia + 1),
    log_den_abuso      = log(n_den_abuso      + 1)
  )

# Outcomes to estimate
OUTCOMES <- c(
  "prom_lect", "prom_mate",                            # SIMCE
  "tasa_asistencia",                                    # attendance
  "tasa_repitencia", "tasa_desercion",                  # grade outcomes
  "log_den_total", "log_den_convivencia", "log_den_abuso" # school violence
)

# --- Static TWFE -------------------------------------------------------------
# Y_it = alpha_i + lambda_t + beta * D_it + controls + eps_it
controls <- "dias_calor + deficit_precip + log(n_matricula + 1)"

# Spec 1: school-clustered standard errors (baseline)
twfe_static <- lapply(OUTCOMES, function(y) {
  fml <- as.formula(paste(y, "~ treated +", controls, "| rbd + year"))
  feols(fml, data = panel_did,
        cluster = ~rbd,
        warn = FALSE, notes = FALSE)
})
names(twfe_static) <- OUTCOMES

# Spec 2: Conley standard errors (spatial autocorrelation)
# cutoff = 50 km — schools within 50 km share unobserved local shocks
# lat/lon from school_panel (columns lat, lon)
twfe_conley <- lapply(OUTCOMES, function(y) {
  fml <- as.formula(paste(y, "~ treated +", controls, "| rbd + year"))
  feols(fml, data = panel_did,
        vcov = conley(lat = "lat", lon = "lon", cutoff = 50),
        warn = FALSE, notes = FALSE)
})
names(twfe_conley) <- OUTCOMES

# Joint table: cluster (col 1) + Conley (col 2) per outcome — goes to methodological appendix
for (y in OUTCOMES) {
  etable(list(cluster = twfe_static[[y]], conley = twfe_conley[[y]]),
         headers = c("Cluster RBD", "Conley 50km"),
         file = file.path(TAB_DIR, paste0("twfe_static_conley_", y, ".tex")),
         replace = TRUE)
}

# Main table: cluster only
etable(twfe_static,
       file = file.path(TAB_DIR, "twfe_static.tex"),
       replace = TRUE)

# --- Dynamic TWFE (event study) ----------------------------------------------
# Omit bin -1 (year before treatment) as reference
# Bin extremes: rel_year <= -5 and rel_year >= 5
panel_did <- panel_did |>
  mutate(rel_year_bin = case_when(
    is.na(rel_year)  ~ NA_real_,
    rel_year <= -5   ~ -5,
    rel_year >= 5    ~  5,
    TRUE             ~  rel_year
  ))

twfe_dynamic <- lapply(OUTCOMES, function(y) {
  fml <- as.formula(paste(
    y, "~ i(rel_year_bin, ref = -1) +", controls, "| rbd + year"
  ))
  feols(fml, data = panel_did,
        cluster = ~rbd,
        warn = FALSE, notes = FALSE)
})
names(twfe_dynamic) <- OUTCOMES

# Plot event studies
for (y in OUTCOMES) {
  png(file.path(FIG_DIR, paste0("eventstudy_twfe_", y, ".png")),
      width = 800, height = 500)
  iplot(twfe_dynamic[[y]],
        main = paste("Event study TWFE —", y),
        xlab = "Years since APR installation",
        ylab = "Estimated effect")
  abline(v = -0.5, lty = 2, col = "gray50")
  dev.off()
}

message("TWFE complete. Tables → ", TAB_DIR, " | Figures → ", FIG_DIR)
