library(here)
library(sf)
library(dplyr)
library(stringr)
library(stringdist)
library(stringi)
library(data.table)

RAW_DOH  <- here("data", "A_raw", "DOH")
OUT_FILE <- here("data", "A_raw", "apr_ddbb", "apr_base.gpkg")

# =============================================================================
# 1. DATA LOADING
# =============================================================================

# fread preserves accented column names (Región, año, N°, etc.) unlike read.csv2
df_APR <- setDF(fread(
  file.path(RAW_DOH, "base APR", "base_ssr.csv"),
  sep = ";", encoding = "Latin-1", na.strings = c("", "NA")
))

ssr <- st_read(
  file.path(RAW_DOH, "poligonos de cobertura SSR", "SHAPES_NACIONAL",
            "TG_AREA_DE_SERVICIO_NACIONAL.shp"),
  quiet = TRUE
)

# =============================================================================
# 2. PREPARE gdf_APR (points from coordinates)
# =============================================================================

# Rename to clean names before any processing (original names have spaces, °, ñ)
df_APR <- df_APR %>%
  rename(
    LONGITUD         = `Coord X grados `,          # trailing space in original header
    LATITUD          = `Coord Y grados`,
    NOMBRE_OFICIAL   = `Nombre  oficial sistema`,  # two spaces in original
    ANIO_INSTALACION = `año puesta en marcha`,
    N_ARRANQUES      = `N° de Arranques a diciembre 2020`
  ) %>%
  mutate(
    LATITUD  = as.numeric(str_replace(LATITUD,  ",", ".")),
    LONGITUD = as.numeric(str_replace(LONGITUD, ",", "."))
  )

gdf_APR <- st_as_sf(df_APR, coords = c("LONGITUD", "LATITUD"), crs = 4326) %>%
  st_transform(crs = 5362)

gdf_SSR <- ssr

# =============================================================================
# 3. TEXT CLEANING FUNCTIONS
# =============================================================================

reparar_doble_utf8 <- function(texto) {
  if (is.na(texto)) return("")
  texto <- as.character(texto)
  texto_intento <- tryCatch(
    iconv(texto, from = "latin1", to = "UTF-8"),
    error = function(e) texto
  )
  if (!is.na(texto_intento)) texto <- texto_intento
  replacements <- list(
    c("Ã'", "Ñ"), c("Ã‰", "É"), c("Ã¡", "á"), c("Ã©", "é"),
    c("Ã­", "í"), c("Ã³", "ó"), c("Ãº", "ú"), c("Ã", "Á"),
    c("Ã"", "Ó"), c("Ãš", "Ú"), c("Â", "")
  )
  for (r in replacements) {
    texto <- str_replace_all(texto, fixed(r[1]), r[2])
  }
  return(texto)
}

limpiar_texto <- function(texto) {
  if (is.na(texto)) return("")
  texto <- stri_trans_general(texto, "Latin-ASCII")
  texto <- toupper(str_trim(texto))
  texto <- str_replace_all(texto, "[^A-Z0-9 ]", "")
  texto <- str_replace_all(texto, "\\s+", " ")
  return(texto)
}

arreglar_texto <- function(texto) {
  texto <- reparar_doble_utf8(texto)
  texto <- limpiar_texto(texto)
  return(texto)
}

arreglar_texto_v <- Vectorize(arreglar_texto)

# =============================================================================
# 4. CLEAN gdf_APR COLUMNS
# =============================================================================

gdf_APR <- gdf_APR %>%
  mutate(
    REGION        = arreglar_texto_v(Región),
    PROVINCIA     = arreglar_texto_v(Provincia),
    COMUNA        = arreglar_texto_v(Comuna),
    NOMBRE_LIMPIO = arreglar_texto_v(NOMBRE_OFICIAL)
  ) %>%
  mutate(
    COMUNA = case_when(
      str_to_upper(COMUNA) == "IMPERIAL" ~ "NUEVA IMPERIAL",
      TRUE ~ COMUNA
    ),
    COMUNA = str_replace_all(COMUNA, regex("PAIGUANO", ignore_case = TRUE), "PAIHUANO"),
    COMUNA = str_replace_all(COMUNA, regex("COCHAMO",  ignore_case = TRUE), "COCHAMÓ")
  )

gdf_APR <- gdf_APR %>%
  mutate(
    NOMBRE_OFICIAL = str_replace_all(
      NOMBRE_OFICIAL,
      regex("Comite De Agua Rural De Allipen", ignore_case = TRUE),
      "Comite de Agua Potable Rural de Allipen"
    ),
    NOMBRE_OFICIAL = str_replace_all(
      NOMBRE_OFICIAL,
      regex("Cooperativa De Servicios De Agua Potable y Saneamiento Ambiental El Granizo Limitada", ignore_case = TRUE),
      "COOPERATIVA DE AGUA POTABLE RURAL EL GRANIZO"
    )
  )

# =============================================================================
# 5. CLEAN gdf_SSR COLUMNS
# =============================================================================

gdf_SSR <- gdf_SSR %>%
  mutate(
    REGION        = arreglar_texto_v(REGION),
    PROVINCIA     = arreglar_texto_v(PROVINCIA),
    COMUNA        = arreglar_texto_v(COMUNA),
    NOMBRE_LIMPIO = arreglar_texto_v(NOMBRE_SSR)
  ) %>%
  mutate(
    REGION    = str_replace_all(REGION, regex("^LA\\s+ARAUCANIA$", ignore_case = TRUE), "ARAUCANIA"),
    COMUNA    = str_replace_all(COMUNA, regex("RAO HURTADO",       ignore_case = TRUE), "RIO HURTADO"),
    COMUNA    = str_replace_all(COMUNA, regex("RAFO NEGRO",        ignore_case = TRUE), "RIO NEGRO"),
    COMUNA    = str_replace_all(COMUNA, regex("COCHAMAFAEUROE",    ignore_case = TRUE), "COCHAMÓ"),
    COMUNA    = str_replace_all(COMUNA, regex("HUALAIHUAFAEURDEG", ignore_case = TRUE), "HUALAIHUE"),
    COMUNA    = str_replace_all(COMUNA, regex("CHAITAFAEURDEGN",   ignore_case = TRUE), "CHAITEN"),
    PROVINCIA = str_replace_all(PROVINCIA, regex("MARGA MARGAMARGA MARGA", ignore_case = TRUE), "MARGA MARGA"),
    NOMBRE_LIMPIO = str_replace_all(NOMBRE_LIMPIO, regex("COMIT\\w*",    ignore_case = TRUE), "COMITE"),
    NOMBRE_LIMPIO = str_replace_all(NOMBRE_LIMPIO, regex("COCHAM\\w*",   ignore_case = TRUE), "COCHAMO"),
    NOMBRE_LIMPIO = str_replace_all(NOMBRE_LIMPIO, regex("HUALAIHU\\w*", ignore_case = TRUE), "HUALAIHUE"),
    NOMBRE_LIMPIO = str_replace_all(NOMBRE_LIMPIO, regex("CHAIT\\w*",    ignore_case = TRUE), "CHAITEN")
  ) %>%
  filter(!str_detect(NOMBRE_SSR, regex("LEUFU LAFKEN", ignore_case = TRUE)))

gdf_SSR <- gdf_SSR %>%
  mutate(
    PROVINCIA = case_when(
      REGION == "ARAUCANIA" & COMUNA == "LONQUIMAY" & PROVINCIA == "CAUTIN" ~ "MALLECO",
      REGION == "VALPARAISO" & PROVINCIA == "SAN FELIPE" ~ "SAN FELIPE DE ACONCAGUA",
      TRUE ~ PROVINCIA
    )
  )

# =============================================================================
# 6. FUZZY MATCHING: SSR -> APR by (REGION, PROVINCIA, COMUNA) + name
# =============================================================================

encontrar_mejor_match <- function(nombre, opciones) {
  if (length(opciones) == 0) return(list(match = NA, score = 0))
  scores <- stringsim(nombre, opciones, method = "lv") * 100
  idx <- which.max(scores)
  return(list(match = opciones[idx], score = scores[idx]))
}

df_APR_attr <- gdf_APR %>% st_drop_geometry()
df_APR_attr$geometry_APR <- st_geometry(gdf_APR)

resultados <- vector("list", nrow(gdf_SSR))

for (i in seq_len(nrow(gdf_SSR))) {
  fila_ssr <- gdf_SSR[i, ]

  region_ssr   <- fila_ssr$REGION
  provincia_ssr <- fila_ssr$PROVINCIA
  comuna_ssr   <- fila_ssr$COMUNA
  nombre_ssr   <- fila_ssr$NOMBRE_LIMPIO

  df_filtrado <- df_APR_attr %>%
    filter(REGION == region_ssr, PROVINCIA == provincia_ssr, COMUNA == comuna_ssr)

  fila_resultado <- fila_ssr

  if (nrow(df_filtrado) > 0) {
    mm <- encontrar_mejor_match(nombre_ssr, df_filtrado$NOMBRE_LIMPIO)

    if (!is.na(mm$match)) {
      fila_match <- df_filtrado %>% filter(NOMBRE_LIMPIO == mm$match) %>% slice(1)

      for (col in setdiff(names(fila_match), "geometry_APR")) {
        fila_resultado[[paste0("APR_", col)]] <- fila_match[[col]]
      }
      fila_resultado[["geometry_APR"]] <- fila_match$geometry_APR
    }
  }

  resultados[[i]] <- fila_resultado
}

gdf_resultado <- bind_rows(resultados)
gdf_resultado <- st_as_sf(gdf_resultado)

# =============================================================================
# 7. DIAGNOSTIC: POST-MERGE DUPLICATES
# =============================================================================

duplicados_post_merge <- gdf_resultado %>%
  st_drop_geometry() %>%
  group_by(REGION, PROVINCIA, COMUNA, NOMBRE_SSR) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

message("Duplicate records after merge: ", nrow(duplicados_post_merge))
if (nrow(duplicados_post_merge) > 0) print(duplicados_post_merge)

gdf_resultado <- gdf_resultado %>%
  distinct(ID_SSR, .keep_all = TRUE)

# =============================================================================
# 8. EXPORTAR apr_base.gpkg
# Column names match what 03_apr_linkage.R and 14_censo_coverage.R expect:
#   id, nombre, anio_instalacion, n_arranques (after rename_with(tolower))
# =============================================================================

gdf_APR_SSR <- gdf_resultado %>%
  select(
    id               = ID_SSR,
    nombre           = NOMBRE_SSR,
    anio_instalacion = APR_ANIO_INSTALACION,
    n_arranques      = APR_N_ARRANQUES,
    region           = REGION,
    provincia        = PROVINCIA,
    comuna           = COMUNA,
    geometry
  ) %>%
  mutate(anio_instalacion = as.integer(anio_instalacion),
         n_arranques      = as.integer(n_arranques))

# Intermediate CSV for manual review of match quality
st_drop_geometry(gdf_APR_SSR) %>%
  write.csv(
    here("data", "A_raw", "apr_ddbb", "apr_ssr_match_diagnostico.csv"),
    row.names = FALSE
  )

st_write(gdf_APR_SSR, OUT_FILE, delete_if_exists = TRUE)

message("=== apr_base.gpkg ===")
message("  SSR polygons total:        ", nrow(gdf_APR_SSR))
message("  With anio_instalacion:     ", sum(!is.na(gdf_APR_SSR$anio_instalacion)))
message("  With n_arranques:          ", sum(!is.na(gdf_APR_SSR$n_arranques)))
message("  No APR match:              ", sum(is.na(gdf_APR_SSR$anio_instalacion)))
message("Saved → ", OUT_FILE)
