library(here)
library(dplyr)
library(readr)
library(purrr)

RAW_SIMCE <- here("data", "A_raw", "MINEDUC", "SIMCE", "4to_1988-2024")
OUT_FILE  <- here("data", "A_raw", "apr_ddbb", "simce_panel.rds")

# SIMCE 4th grade was administered every year 2005-2013, then 2015-2018
# (not in 2014 due to policy change; not in 2019-2021 due to COVID)
SIMCE_YEARS <- c(2005:2013, 2015:2018)

read_simce_rbd <- function(year) {
  year_dirs <- list.dirs(RAW_SIMCE, recursive = FALSE)
  match     <- year_dirs[grepl(as.character(year), year_dirs)]
  if (length(match) == 0) {
    message("  No folder found for SIMCE year ", year)
    return(NULL)
  }
  txt_dir <- file.path(match[1], "Archivos TXT (Planos)")
  csv <- list.files(txt_dir, pattern = paste0("simce4b", year, "_rbd"), full.names = TRUE)
  if (length(csv) == 0) {
    message("  No RBD CSV found for SIMCE year ", year)
    return(NULL)
  }
  read_delim(csv[1], delim = "|", show_col_types = FALSE,
             locale = locale(encoding = "UTF-8"))
}

simce_raw <- map(SIMCE_YEARS, read_simce_rbd) |> compact() |> list_rbind()

simce_panel <- simce_raw |>
  rename_with(tolower) |>
  select(
    year = agno,
    rbd,
    n_alumnos_4b   = nalu_4b_rbd,
    prom_lect      = prom_lect4b_rbd,
    prom_mate      = prom_mate4b_rbd,
    prom_soc       = prom_soc4b_rbd,
    rural          = cod_rural_rbd,
    cod_reg        = cod_reg_rbd,
    cod_com        = cod_com_rbd
  ) |>
  mutate(across(c(prom_lect, prom_mate, prom_soc, n_alumnos), as.numeric))

saveRDS(simce_panel, OUT_FILE)
message("Saved: ", nrow(simce_panel), " rows, years ",
        min(simce_panel$year), "–", max(simce_panel$year), " → ", OUT_FILE)
