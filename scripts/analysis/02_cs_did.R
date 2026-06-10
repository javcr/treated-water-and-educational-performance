library(here)
library(did)
library(dplyr)
library(ggplot2)

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
FIG_DIR <- here("output", "figures")
TAB_DIR <- here("output", "tables")

panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))

# Callaway & Sant'Anna requires:
#   gname  = first treatment year (0 if never treated)
#   idname = unit identifier
#   tname  = year
panel_cs <- panel |>
  group_by(rbd) |>
  mutate(
    gname = if_else(any(treated == 1, na.rm = TRUE),
                    min(year[treated == 1], na.rm = TRUE),
                    0L)
  ) |>
  ungroup() |>
  filter(rural_rbd == 1 | gname > 0)   # same sample as TWFE

panel_cs <- panel_cs |>
  mutate(
    log_den_total       = log(n_den_total       + 1),
    log_den_convivencia = log(n_den_convivencia + 1),
    log_den_abuso       = log(n_den_abuso       + 1)
  )

OUTCOMES <- c(
  "prom_lect", "prom_mate",
  "tasa_asistencia",
  "tasa_repitencia", "tasa_desercion",
  "log_den_total", "log_den_convivencia", "log_den_abuso"
)

run_cs <- function(y) {
  df <- panel_cs |>
    filter(!is.na(.data[[y]])) |>
    mutate(across(all_of(y), as.numeric))

  att_gt(
    yname         = y,
    tname         = "year",
    idname        = "rbd",
    gname         = "gname",
    xformla       = ~ dias_calor + deficit_precip + log(n_matricula + 1),
    data          = df,
    control_group = "notyettreated",   # control group: not-yet-treated units
    anticipation  = 0,
    clustervars   = "rbd",
    panel         = TRUE
  )
}

cs_results <- lapply(OUTCOMES, run_cs)
names(cs_results) <- OUTCOMES

# Aggregation and event study
for (y in OUTCOMES) {
  # Aggregate ATT
  agg <- aggte(cs_results[[y]], type = "simple")
  message(y, " — Aggregate ATT: ", round(agg$overall.att, 4),
          " (se = ", round(agg$overall.se, 4), ")")

  # Event study plot
  es <- aggte(cs_results[[y]], type = "dynamic", na.rm = TRUE)
  p  <- ggdid(es) +
    labs(title = paste("Callaway & Sant'Anna —", y),
         x = "Years since APR installation",
         y = "Estimated ATT") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    theme_minimal()

  ggsave(file.path(FIG_DIR, paste0("eventstudy_cs_", y, ".png")),
         p, width = 8, height = 5)
}

saveRDS(cs_results, file.path(TAB_DIR, "cs_results.rds"))

# Note on standard errors:
# att_gt() uses bootstrap (1000 reps by default) → SEs are already
# robust to serial correlation. Conley SEs not directly supported.
# Spatial robustness: compare coefficients with TWFE Conley 50km from 01_twfe.R.
message("Callaway & Sant'Anna complete.")
