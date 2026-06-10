library(here)
library(DIDmultiplegt)
library(dplyr)

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
FIG_DIR <- here("output", "figures")
TAB_DIR <- here("output", "tables")

panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))

panel_dc <- panel |>
  group_by(rbd) |>
  mutate(
    gname = if_else(any(treated == 1, na.rm = TRUE),
                    min(year[treated == 1], na.rm = TRUE),
                    0L)
  ) |>
  ungroup() |>
  filter(rural_rbd == 1 | gname > 0)

panel_dc <- panel_dc |>
  mutate(
    log_den_total       = log(n_den_total       + 1),
    log_den_convivencia = log(n_den_convivencia + 1),
    log_den_abuso       = log(n_den_abuso       + 1),
    log_n_matricula     = log(n_matricula        + 1)
  )

OUTCOMES <- c(
  "prom_lect", "prom_mate",
  "tasa_asistencia",
  "tasa_repitencia", "tasa_desercion",
  "log_den_total", "log_den_convivencia", "log_den_abuso"
)

# de Chaisemartin & D'Haultfoeuille (2024)
# Robust to treatment effect heterogeneity in staggered adoption
run_dc <- function(y) {
  df <- panel_dc |>
    filter(!is.na(.data[[y]])) |>
    as.data.frame()

  did_multiplegt_dyn(
    df      = df,
    outcome = y,
    group   = "rbd",
    time    = "year",
    treatment = "treated",
    controls  = c("dias_calor", "deficit_precip", "log_n_matricula"),
    effects   = 5,       # dynamic effects up to t+5
    placebo   = 3,       # pre-treatment placebos up to t-3
    cluster   = "rbd"
  )
}

dc_results <- lapply(OUTCOMES, run_dc)
names(dc_results) <- OUTCOMES

saveRDS(dc_results, file.path(TAB_DIR, "dechais_results.rds"))

# Note on standard errors:
# did_multiplegt_dyn() uses bootstrap by default → robust to serial correlation.
# Conley SEs not directly supported. Spatial robustness: compare with TWFE Conley
# from 01_twfe.R. If coefficients are similar despite different SEs, spatial
# inference does not alter conclusions.
message("de Chaisemartin complete.")
