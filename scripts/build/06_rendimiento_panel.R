library(here)
library(data.table)
library(purrr)

RAW_REN  <- here("data", "A_raw", "MINEDUC", "Rendimiento")
OUT_FILE <- here("data", "A_raw", "apr_ddbb", "rendimiento_panel.rds")

PANEL_YEARS <- 2010:2020

COLS <- c("AGNO", "RBD", "MRUN", "COD_GRADO", "COD_ENSE",
          "ASISTENCIA", "PROM_GRAL", "SIT_FIN", "GEN_ALU", "RURAL_RBD")

find_rend_csv <- function(year) {
  dir  <- file.path(RAW_REN, paste0("Rendimiento-", year))
  csvs <- list.files(dir, pattern = "\\.(csv|CSV)$", full.names = TRUE)
  csvs[which.max(file.size(csvs))]
}

read_rend <- function(year) {
  csv <- find_rend_csv(year)
  if (length(csv) == 0 || is.na(csv)) {
    message("  No CSV for Rendimiento year ", year)
    return(NULL)
  }
  message("Reading ", year, " — ", basename(csv))
  available <- names(fread(csv, nrows = 0))
  dt <- fread(csv, sep = ";", encoding = "Latin-1",
              select = intersect(COLS, available),
              na.strings = c("", "NA"))
  dt[, agno := year]
  dt
}

ren_raw <- map(PANEL_YEARS, read_rend) |> rbindlist(fill = TRUE)

# SIT_FIN: P = Promovido, R = Repite, Y = Abandona
# Aggregate to school × year
rendimiento_panel <- ren_raw[!is.na(RBD), .(
  n_alumnos      = .N,
  n_promovidos   = sum(SIT_FIN == "P", na.rm = TRUE),
  n_repiten      = sum(SIT_FIN == "R", na.rm = TRUE),
  n_abandonan    = sum(SIT_FIN == "Y", na.rm = TRUE),
  tasa_repitencia = sum(SIT_FIN == "R", na.rm = TRUE) / .N,
  tasa_desercion  = sum(SIT_FIN == "Y", na.rm = TRUE) / .N,
  tasa_aprobacion = sum(SIT_FIN == "P", na.rm = TRUE) / .N,
  prom_gral_mean  = mean(as.numeric(PROM_GRAL), na.rm = TRUE),
  asistencia_mean = mean(as.numeric(ASISTENCIA), na.rm = TRUE)
), by = .(rbd = RBD, year = agno)]

setorder(rendimiento_panel, rbd, year)

saveRDS(rendimiento_panel, OUT_FILE)
message("Saved: ", nrow(rendimiento_panel), " school-years → ", OUT_FILE)
