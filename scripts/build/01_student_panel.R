library(here)
library(data.table)
library(purrr)

RAW_MAT  <- here("data", "A_raw", "MINEDUC", "Matricula")
OUT_FILE <- here("data", "A_raw", "apr_ddbb", "student_panel.rds")

PANEL_YEARS <- 2004:2021

# Columns to read from each student-level file (reduces memory from ~500MB to ~80MB per year)
COLS <- c("agno", "rbd", "mrun", "gen_alu", "fec_nac_alu", "edad_alu",
          "cod_ense", "cod_grado",
          "cod_depe", "cod_depe2", "rural_rbd",
          "cod_reg_rbd", "cod_com_rbd", "nom_com_rbd",
          "cod_reg_alu", "cod_com_alu")

find_mat_csv <- function(year) {
  dir  <- file.path(RAW_MAT, paste0("Matricula-por-estudiante-", year))
  csvs <- list.files(dir, pattern = "\\.(csv|CSV)$", full.names = TRUE)
  # Exclude documentation files — take the largest file
  csvs[which.max(file.size(csvs))]
}

read_mat <- function(year) {
  csv <- find_mat_csv(year)
  if (length(csv) == 0 || is.na(csv)) {
    message("  No CSV for Matricula year ", year)
    return(NULL)
  }
  message("Reading ", year, " — ", basename(csv))
  dt <- fread(csv, sep = ";", encoding = "Latin-1",
              select = intersect(COLS, fread(csv, nrows = 0) |> names()),
              na.strings = c("", "NA"))
  dt[, agno := year]  # use folder year as authoritative
  dt
}

mat <- map(PANEL_YEARS, read_mat) |> rbindlist(fill = TRUE)

# Aggregate to school × year
student_panel <- mat[, .(
  n_matricula  = .N,
  n_mujeres    = sum(gen_alu == 2, na.rm = TRUE),
  n_hombres    = sum(gen_alu == 1, na.rm = TRUE),
  edad_prom    = mean(edad_alu, na.rm = TRUE),
  # School characteristics: take modal value within rbd/year
  cod_depe     = as.integer(names(sort(table(cod_depe),  decreasing = TRUE))[1]),
  cod_depe2    = as.integer(names(sort(table(cod_depe2), decreasing = TRUE))[1]),
  rural_rbd    = as.integer(names(sort(table(rural_rbd), decreasing = TRUE))[1]),
  cod_reg_rbd  = as.integer(names(sort(table(cod_reg_rbd), decreasing = TRUE))[1]),
  cod_com_rbd  = as.integer(names(sort(table(cod_com_rbd), decreasing = TRUE))[1]),
  nom_com_rbd  = names(sort(table(nom_com_rbd), decreasing = TRUE))[1]
), by = .(rbd, agno)]

setnames(student_panel, "agno", "year")
setorder(student_panel, rbd, year)

saveRDS(student_panel, OUT_FILE)
message("Saved: ", nrow(student_panel), " school-years, ",
        uniqueN(student_panel$rbd), " RBDs → ", OUT_FILE)
