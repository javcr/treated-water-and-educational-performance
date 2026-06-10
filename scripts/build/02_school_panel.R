library(here)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)

RAW_EE       <- here("data", "A_raw", "MINEDUC", "Establecimientos")
STUDENT_FILE <- here("data", "A_raw", "apr_ddbb", "student_panel.rds")
OUT_FILE     <- here("data", "A_raw", "apr_ddbb", "school_panel.rds")

PANEL_YEARS <- 2004:2021

find_ee_dir <- function(year) {
  candidates <- c(
    file.path(RAW_EE, paste0("Directorio-Oficial-EE-", year)),
    file.path(RAW_EE, paste0("Directorio-oficial-EE-",  year))
  )
  found <- candidates[dir.exists(candidates)]
  if (length(found) == 0) return(NULL)
  found[1]
}

read_ee <- function(year) {
  dir <- find_ee_dir(year)
  if (is.null(dir)) return(NULL)
  csv <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)[1]
  enc <- if (year <= 2017) "latin1" else "UTF-8"
  read_delim(csv, delim = ";", show_col_types = FALSE,
             locale = locale(encoding = enc),
             name_repair = "minimal") |>
    rename_with(tolower) |>
    mutate(year_dir = year)
}

ee_years <- 2013:2021
ee_raw   <- map(ee_years, read_ee) |> compact() |> list_rbind()

# Variables from Establecimientos that don't exist in Matricula
ee_base <- ee_raw |>
  select(
    rbd, year_dir,
    nom_rbd,
    lat           = latitud,
    lon           = longitud,
    convenio_pie,
    estado_estab,
    starts_with("ens_"),
    pago_matricula,
    pago_mensual
  ) |>
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )

# School universe: all RBDs ever in Matricula + any in Directorio not in Matricula
student <- readRDS(STUDENT_FILE)
rbd_mat <- unique(student$rbd)
rbd_ee  <- unique(ee_base$rbd)
all_rbd <- union(rbd_mat, rbd_ee)

message("RBDs in Matricula: ", length(rbd_mat))
message("RBDs only in Directorio: ", length(setdiff(rbd_ee, rbd_mat)))
message("Total RBDs in universe: ", length(all_rbd))

# Build balanced panel for the full universe
school_panel <- expand_grid(rbd = all_rbd, year = PANEL_YEARS) |>
  # Join school characteristics from Matricula (available 2004-2021)
  left_join(student, by = c("rbd", "year")) |>
  # Join Establecimientos variables (lat/lon from 2014, rest from 2013)
  left_join(ee_base, by = c("rbd", "year" = "year_dir")) |>
  arrange(rbd, year) |>
  group_by(rbd) |>
  # Fill lat/lon and slow-moving Establecimientos variables
  # (schools don't move; convenio_pie, nom_rbd change rarely)
  fill(nom_rbd, lat, lon, convenio_pie,
       starts_with("ens_"), pago_matricula, pago_mensual,
       .direction = "downup") |>
  # estado_estab: no fill — only meaningful where observed
  ungroup()

saveRDS(school_panel, OUT_FILE)
message("Saved: ", nrow(school_panel), " school-years, ",
        n_distinct(school_panel$rbd), " RBDs → ", OUT_FILE)
