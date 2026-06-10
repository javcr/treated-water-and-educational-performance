library(here)
library(data.table)
library(purrr)

# DEIS emergency visits data — daily counts by health facility
# Confirmed structure from 2016 sample:
#   IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa,
#   Total, Menores_1, De_1_a_4, De_5_a_14, De_15_a_64, De_65_y_mas,
#   fecha, semana, GLOSATIPOESTABLECIMIENTO, GLOSATIPOATENCION
#
# Linkage to APR: by lat/lon of health facility → nearest APR centroid.
#   Performed in 11_health_main_panel.R via sf::st_nearest_feature().

RAW_DEIS  <- here("data", "A_raw", "DEIS")
OUT_FILE  <- here("data", "A_raw", "apr_ddbb", "deis_panel.rds")

PANEL_YEARS <- 2008:2020

# IdCausa of interest: Acute Diarrhea (A00-A09) — primary waterborne disease cause
# Verify whether additional causes apply once full data is received
CAUSAS_GI <- 29L

# --- Health facility coordinates ---
estab_coords <- fread(
  file.path(RAW_DEIS, "establecimientos_20260526.csv"),
  sep = ";", encoding = "Latin-1", na.strings = c("", "NA")
)[, .(
  id_estab = EstablecimientoCodigoAntiguo,
  lat      = as.numeric(Latitud),
  lon      = as.numeric(Longitud),
  tipo_estab = TipoEstablecimientoGlosa,
  tiene_urgencia = TieneServicioUrgencia
)][!is.na(id_estab) & id_estab != ""]

# --- Emergency visits ---
find_deis_csv <- function(year) {
  csvs <- list.files(RAW_DEIS, pattern = as.character(year),
                     full.names = TRUE, recursive = TRUE)
  csvs <- csvs[grepl("\\.(csv|CSV)$", csvs) &
               !grepl("establecimientos", csvs)]
  csvs[which.max(file.size(csvs))]
}

read_deis <- function(year) {
  csv <- find_deis_csv(year)
  if (length(csv) == 0 || is.na(csv)) {
    message("  No CSV for DEIS year ", year, " — skipping")
    return(NULL)
  }
  message("Reading DEIS ", year)
  dt <- fread(csv, sep = ";", encoding = "Latin-1", na.strings = c("", "NA"))
  setnames(dt, names(dt), tolower(names(dt)))
  if (!"agno" %in% names(dt))
    dt[, agno := as.integer(format(as.Date(fecha, "%d/%m/%Y"), "%Y"))]
  dt
}

deis_raw <- map(PANEL_YEARS, read_deis) |> rbindlist(fill = TRUE)

deis_panel <- deis_raw[
  idcausa %in% CAUSAS_GI,
  .(
    # All age groups kept as separate variables
    n_gi_menores1 = sum(menores_1,   na.rm = TRUE),
    n_gi_1a4      = sum(de_1_a_4,    na.rm = TRUE),
    n_gi_5a14     = sum(de_5_a_14,   na.rm = TRUE),
    n_gi_15a64    = sum(de_15_a_64,  na.rm = TRUE),
    n_gi_65ymas   = sum(de_65_y_mas, na.rm = TRUE),
    n_gi_total    = sum(total,        na.rm = TRUE),
    # Aggregate 0-14 years (school-age and pre-school)
    n_gi_nna      = sum(menores_1 + de_1_a_4 + de_5_a_14, na.rm = TRUE)
  ),
  by = .(id_estab  = idestablecimiento,
         nom_estab = nestablecimiento,
         year      = agno)
][estab_coords, on = "id_estab", nomatch = NA]   # join coordinates

setorder(deis_panel, id_estab, year)

message("Facilities with coordinates: ",
        sum(!is.na(deis_panel$lat)), " / ", nrow(deis_panel))

saveRDS(deis_panel, OUT_FILE)
message("Saved: ", nrow(deis_panel), " estab-years → ", OUT_FILE)
