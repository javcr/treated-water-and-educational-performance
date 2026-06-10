library(here)
library(data.table)
library(purrr)

OUT_DIR  <- here("data", "A_raw", "apr_ddbb")
RAW_REN  <- here("data", "A_raw", "MINEDUC", "Rendimiento")
RAW_MAT  <- here("data", "A_raw", "MINEDUC", "Matricula")
OUT_FILE <- file.path(OUT_DIR, "student_main_panel.rds")

# Rendimiento available from 2010; Matricula from 2004
# Student-level outcome panel: 2010-2020
PANEL_YEARS <- 2010:2020

# ---------------------------------------------------------------------------
# 1. Rendimiento per student — annual outcomes
# ---------------------------------------------------------------------------
REN_COLS <- c("AGNO", "RBD", "MRUN", "GEN_ALU", "FEC_NAC_ALU",
              "COD_GRADO", "COD_ENSE", "RURAL_RBD",
              "COD_REG_RBD", "COD_COM_RBD",
              "COD_COM_ALU",          # student's municipality of residence
              "PROM_GRAL", "ASISTENCIA", "SIT_FIN")

find_csv <- function(dir) {
  csvs <- list.files(dir, pattern = "\\.(csv|CSV)$", full.names = TRUE)
  csvs[which.max(file.size(csvs))]
}

read_rend <- function(year) {
  csv  <- find_csv(file.path(RAW_REN, paste0("Rendimiento-", year)))
  if (is.na(csv)) return(NULL)
  message("Rendimiento ", year)
  avail <- names(fread(csv, nrows = 0))
  fread(csv, sep = ";", encoding = "Latin-1",
        select = intersect(REN_COLS, avail),
        na.strings = c("", "NA"))[, agno := year]
}

rend <- map(PANEL_YEARS, read_rend) |> rbindlist(fill = TRUE)
setnames(rend, names(rend), tolower(names(rend)))

# --- Duplicate check: MRUN appearing in >1 RBD in same year in Rendimiento ---
ren_dups <- rend[, .(n_rbd = uniqueN(rbd)), by = .(mrun, agno)][n_rbd > 1]
message("=== Rendimiento duplicate check ===")
message("  Student-years with >1 RBD: ", nrow(ren_dups),
        " (", round(100 * nrow(ren_dups) / nrow(rend[, .N, by = .(mrun, agno)]), 2), "%)")
message("  Unique students affected:  ", uniqueN(ren_dups$mrun))
rend[, flag_ren_dup := as.integer(
  paste(mrun, agno) %in% paste(ren_dups$mrun, ren_dups$agno)
)]

# ---------------------------------------------------------------------------
# 2. Rural emigration tracking
#    For each rural student in year t: do they appear in an urban school in t+1?
#    Or disappear from Matricula altogether? → emigra_rural indicator
# ---------------------------------------------------------------------------
MAT_COLS <- c("mrun", "rbd", "rural_rbd", "agno")

read_mat_min <- function(year) {
  dir  <- file.path(RAW_MAT, paste0("Matricula-por-estudiante-", year))
  csv  <- find_csv(dir)
  if (is.na(csv)) return(NULL)
  message("Matricula ", year)
  avail <- names(fread(csv, nrows = 0, sep = ";"))
  fread(csv, sep = ";", encoding = "Latin-1",
        select = intersect(MAT_COLS, avail),
        na.strings = c("", "NA"))[, agno := year]
}

# Read t and t+1 for all panel years
mat_years <- sort(unique(c(PANEL_YEARS, PANEL_YEARS + 1)))
mat <- map(mat_years, read_mat_min) |> rbindlist(fill = TRUE)

# --- Duplicate check: MRUN appearing in >1 RBD in same year in Matricula -----
mat_dups <- mat[, .(n_rbd = uniqueN(rbd)), by = .(mrun, agno)][n_rbd > 1]
message("=== Matricula duplicate check ===")
message("  Student-years with >1 RBD: ", nrow(mat_dups),
        " (", round(100 * nrow(mat_dups) / nrow(mat[, .N, by = .(mrun, agno)]), 2), "%)")
message("  Unique students affected:  ", uniqueN(mat_dups$mrun))

# One row per MRUN/year; flag ambiguous cases (>1 RBD) → emigra_rural = NA later
mat_uniq <- mat[, .(
  rbd        = rbd[1],
  rural_rbd  = rural_rbd[1],
  flag_mat_dup = as.integer(uniqueN(rbd) > 1)
), by = .(mrun, agno)]

# Join t+1 to detect school change
mat_t  <- mat_uniq[agno %in% PANEL_YEARS]
mat_t1 <- mat_uniq[, .(mrun, agno_t1 = agno, rbd_t1 = rbd, rural_t1 = rural_rbd)]
setkey(mat_t,  mrun, agno)
setkey(mat_t1, mrun, agno_t1)

migration <- mat_t[mat_t1, on = .(mrun, agno = agno_t1 - 1L), nomatch = NA]
migration[, emigra_rural := as.integer(
  rural_rbd == 1 &                    # was in rural school at t
  (is.na(rbd_t1) |                    # disappears in t+1
   rural_t1 == 0)                     # or moves to urban school in t+1
)]
# Ambiguous school assignment in t or t+1 → emigra_rural not trustworthy
migration[flag_mat_dup == 1L, emigra_rural := NA_integer_]
migration <- migration[, .(mrun, agno, emigra_rural)]

# ---------------------------------------------------------------------------
# 3. Merge with treatment and controls from school panel
# ---------------------------------------------------------------------------
apr       <- readRDS(file.path(OUT_DIR, "school_apr.rds"))          |> setDT()
school    <- readRDS(file.path(OUT_DIR, "school_panel.rds"))        |> setDT()
climate   <- readRDS(file.path(OUT_DIR, "climate_panel.rds"))       |> setDT()
censo_cov <- readRDS(file.path(OUT_DIR, "censo_apr_coverage.rds"))  |> setDT()

setkey(apr,     rbd, year)
setkey(school,  rbd, year)
setkey(climate, rbd, year)
setkey(censo_cov, rbd)

rend[, year := agno]
setkey(rend, rbd, year)

student_panel <- rend[
  apr[, .(rbd, year, treated, apr_id, anio_instalacion)],
  on = .(rbd, year), nomatch = NA
][
  school[, .(rbd, year, lat, lon, nom_com_rbd, cod_depe, cod_depe2,
             convenio_pie, n_matricula)],
  on = .(rbd, year), nomatch = NA
][
  climate[, .(rbd, year, tmax_anual, precip_anual,
              dias_calor, deficit_precip)],
  on = .(rbd, year), nomatch = NA
][
  migration, on = .(mrun, agno), nomatch = NA
][
  # prop_apr: time-invariant continuous treatment (CENSO 2024 snapshot)
  censo_cov[, .(rbd, prop_apr)],
  on = .(rbd), nomatch = NA
]

# Years since treatment (for event study)
student_panel[, years_since_treat := fifelse(
  !is.na(anio_instalacion),
  year - anio_instalacion,
  NA_integer_
)]

setorder(student_panel, mrun, year)

saveRDS(student_panel, OUT_FILE)

message("=== Student main panel ===")
message("  Rows:                  ", nrow(student_panel))
message("  Unique students:       ", uniqueN(student_panel$mrun))
message("  Schools (RBD):         ", uniqueN(student_panel$rbd))
message("  Years:                 ", min(student_panel$year), "–", max(student_panel$year))
message("  Treated (st-yr):       ", sum(student_panel$treated == 1, na.rm = TRUE))
message("  Emigration obs:        ", sum(!is.na(student_panel$emigra_rural)))
message("  flag_ren_dup (st-yr):  ", sum(student_panel$flag_ren_dup == 1, na.rm = TRUE))
message("  flag_mat_dup (st-yr):  ", sum(!is.na(student_panel$flag_mat_dup) &
                                          student_panel$flag_mat_dup == 1))
message("  prop_apr (st-yr):      ", sum(!is.na(student_panel$prop_apr)))
message("Saved → ", OUT_FILE)
