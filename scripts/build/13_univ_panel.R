library(here)
library(data.table)
library(purrr)

# Matricula Educacion Superior 2007-2024 (MINEDUC/SIES).
# One row per student-year enrolled in higher education.
# Key: mrun (same identifier as Rendimiento/Matricula).
# Purpose: build student-level indicator of higher education enrollment,
# to be merged into the student panel as a long-run outcome.

RAW_ES  <- here("data", "A_raw", "MINEDUC", "Matricula-Ed-Superior")
OUT_DIR <- here("data", "A_raw", "apr_ddbb")
OUT_FILE <- file.path(OUT_DIR, "univ_panel.rds")

# Students in the Rendimiento panel (2010-2020) could reach university from 2011 onward.
# Include through 2024 to capture delayed enrollment.
ES_YEARS <- 2011:2024

ES_COLS <- c("cat_periodo", "mrun", "tipo_inst_1", "tipo_inst_2",
             "nivel_global", "nivel_carrera_1", "anio_ing_carr_ori",
             "area_conocimiento", "region_sede")

find_csv <- function(dir) {
  csvs <- list.files(dir, pattern = "\\.(csv|CSV)$", full.names = TRUE)
  if (length(csvs) == 0) return(NA_character_)
  csvs[which.max(file.size(csvs))]
}

read_es <- function(year) {
  dir <- file.path(RAW_ES, paste0("Matricula-Ed-Superior-", year))
  csv <- find_csv(dir)
  if (is.na(csv)) {
    message("  No CSV for ES year ", year, " — skipping")
    return(NULL)
  }
  message("Reading Ed. Superior ", year)
  avail <- names(fread(csv, nrows = 0, sep = ";"))
  dt <- fread(csv, sep = ";", encoding = "Latin-1",
              select = intersect(ES_COLS, avail),
              na.strings = c("", "NA"))
  dt[, year := year]
  dt
}

es_raw <- map(ES_YEARS, read_es) |> rbindlist(fill = TRUE)
setnames(es_raw, names(es_raw), tolower(names(es_raw)))

# Harmonise period column (cat_periodo or year)
if ("cat_periodo" %in% names(es_raw))
  es_raw[!is.na(cat_periodo), year := as.integer(cat_periodo)]

# Indicator: enrolled in any higher education in year t
es_raw[, `:=`(
  univ      = as.integer(grepl("Universidad", tipo_inst_1, ignore.case = TRUE)),
  cft_ip    = as.integer(grepl("CFT|Instituto Profesional", tipo_inst_1,
                                ignore.case = TRUE)),
  cualquier = 1L   # any higher education
)]

# Collapse to one row per MRUN × year (a student may have multiple programs)
univ_panel <- es_raw[!is.na(mrun), .(
  univ      = as.integer(any(univ   == 1L)),
  cft_ip    = as.integer(any(cft_ip == 1L)),
  cualquier = 1L,
  anio_ing_carr_ori = min(anio_ing_carr_ori, na.rm = TRUE)
), by = .(mrun, year)]

# Student-level summary: first year of any higher ed enrollment
first_es <- univ_panel[, .(
  anio_primer_es   = min(year),
  univ_alguna_vez  = as.integer(any(univ   == 1L)),
  cft_ip_alguna_vez = as.integer(any(cft_ip == 1L))
), by = mrun]

setorder(univ_panel, mrun, year)

saveRDS(list(panel = univ_panel, first = first_es), OUT_FILE)

message("=== University panel ===")
message("  Student-years:          ", nrow(univ_panel))
message("  Unique students:        ", uniqueN(univ_panel$mrun))
message("  Ever enrolled (univ):   ", sum(first_es$univ_alguna_vez))
message("  Ever enrolled (CFT/IP): ", sum(first_es$cft_ip_alguna_vez))
message("Saved → ", OUT_FILE)
