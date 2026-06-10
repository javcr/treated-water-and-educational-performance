library(here)
library(data.table)
library(purrr)

RAW_ASIS <- here("data", "A_raw", "MINEDUC", "Asistencia")
OUT_FILE <- here("data", "A_raw", "apr_ddbb", "asistencia_panel.rds")

PANEL_YEARS <- 2011:2020   # intersect with panel window (Asistencia starts 2011)

COLS <- c("AGNO", "MES_ESCOLAR", "RBD", "DIAS_TRABAJADOS", "DIAS_ASISTIDOS")

read_month <- function(csv_path, year) {
  dt <- fread(csv_path, sep = ";", encoding = "Latin-1",
              select = COLS, na.strings = c("", "NA"))
  dt[, agno := year]
  dt
}

read_year <- function(year) {
  year_dir  <- file.path(RAW_ASIS, as.character(year))
  month_dirs <- list.dirs(year_dir, recursive = FALSE)
  csvs <- unlist(lapply(month_dirs, function(d)
    list.files(d, pattern = "\\.(csv|CSV)$", full.names = TRUE)))
  # Exclude documentation files
  csvs <- csvs[file.size(csvs) > 1e6]
  if (length(csvs) == 0) {
    message("  No CSVs for year ", year)
    return(NULL)
  }
  message("Reading ", year, " — ", length(csvs), " monthly files")
  map(csvs, read_month, year = year) |> rbindlist(fill = TRUE)
}

asis_raw <- map(PANEL_YEARS, read_year) |> rbindlist(fill = TRUE)

# Annual attendance rate per school:
# sum of days attended / sum of days in session across all students and months
asistencia_panel <- asis_raw[
  !is.na(RBD) & !is.na(DIAS_TRABAJADOS) & DIAS_TRABAJADOS > 0,
  .(
    dias_trabajados = sum(DIAS_TRABAJADOS, na.rm = TRUE),
    dias_asistidos  = sum(DIAS_ASISTIDOS,  na.rm = TRUE)
  ),
  by = .(rbd = RBD, year = agno)
][, tasa_asistencia := dias_asistidos / dias_trabajados]

setorder(asistencia_panel, rbd, year)

saveRDS(asistencia_panel, OUT_FILE)
message("Saved: ", nrow(asistencia_panel), " school-years → ", OUT_FILE)
