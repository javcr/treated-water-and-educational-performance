library(here)
library(dplyr)

OUT_DIR  <- here("data", "A_raw", "apr_ddbb")
OUT_FILE <- file.path(OUT_DIR, "school_main_panel.rds")

read_built <- function(name) readRDS(file.path(OUT_DIR, name))

school   <- read_built("school_panel.rds")
apr      <- read_built("school_apr.rds")
simce    <- read_built("simce_panel.rds")
asist    <- read_built("asistencia_panel.rds")
rend     <- read_built("rendimiento_panel.rds")
violence <- read_built("violence_panel.rds")
climate  <- read_built("climate_panel.rds")
# censo_apr_coverage: one row per school, prop_apr = share of rural dwellings
# in the school's APR polygon with piped water (continuous treatment, from 14_censo_coverage.R)
censo_cov <- read_built("censo_apr_coverage.rds")

panel <- school |>
  left_join(apr,       by = c("rbd", "year")) |>
  left_join(simce,     by = c("rbd", "year")) |>
  left_join(asist,     by = c("rbd", "year")) |>
  left_join(rend,      by = c("rbd", "year")) |>
  left_join(violence,  by = c("rbd", "year")) |>
  left_join(climate,   by = c("rbd", "year")) |>
  left_join(censo_cov, by = "rbd") |>           # time-invariant: CENSO 2024 snapshot
  arrange(rbd, year)

saveRDS(panel, OUT_FILE)

message("=== School main panel ===")
message("  Rows:   ", nrow(panel))
message("  RBDs:   ", n_distinct(panel$rbd))
message("  Years:  ", min(panel$year), "–", max(panel$year))
message("  Treated school-years:  ", sum(panel$treated == 1,          na.rm = TRUE))
message("  SIMCE obs:             ", sum(!is.na(panel$prom_lect)))
message("  Asistencia obs:        ", sum(!is.na(panel$tasa_asistencia)))
message("  Rendimiento obs:       ", sum(!is.na(panel$tasa_repitencia)))
message("  Violencia obs:         ", sum(!is.na(panel$n_den_total)))
message("  Clima obs:             ", sum(!is.na(panel$dias_calor)))
message("  prop_apr coverage:     ", sum(!is.na(panel$prop_apr)), " school-years (",
        n_distinct(panel$rbd[!is.na(panel$prop_apr)]), " schools)")
message("Saved → ", OUT_FILE)
