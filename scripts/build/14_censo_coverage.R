library(here)
library(sf)
library(dplyr)
library(tidyr)
library(data.table)

CENSO_DIR <- here("data", "A_raw", "CENSO")
APR_FILE  <- here("data", "A_raw", "apr_ddbb", "apr_base.gpkg")
SCH_FILE  <- here("data", "A_raw", "apr_ddbb", "school_panel.rds")
OUT_DIR   <- here("data", "A_raw", "apr_ddbb")

# ---------------------------------------------------------------------------
# Continuous treatment: proportion of rural dwellings in an APR polygon's
# service area that are connected to red pública water (= APR connection rate).
#
# Method:
#   1. Entidades_CPV24 → rural entity polygons with tabulated dwelling counts
#      by water source. Two-file approach:
#        - cartografia/Cartografia_censo2024_Pais.gpkg layer "Entidades_CPV24":
#          geometry + MANZENT key + AREA_C/TIPO_MZ for rural filter
#        - manzanas/Base_manzana_entidad_CPV24.csv: 212-variable tabulation
#          including n_fuente_agua_publica and n_vp, joined by MANZENT
#   2. APR polygons (apr_base.gpkg) → service area boundaries.
#   3. Spatial intersection: allocate each entity's dwelling counts to APR
#      polygons proportionally by intersection area.
#   4. Point-in-polygon: assign each school the prop_apr of the APR polygon
#      it falls within (0 for schools outside all APR polygons).
#
# CRS: Entidades_CPV24 is GCS SIRGAS 2000 (EPSG:4674); we reproject everything
# to EPSG:32719 (UTM zone 19S) for area calculations in m².
# ---------------------------------------------------------------------------

VAR_RED_PUB <- "n_fuente_agua_publica"  # dwellings with red pública (diccionario_variables_glosas_censo2024.xlsx)
VAR_TOT_VIV <- "n_vp"                   # total particular dwellings
APR_ID_VAR  <- "id"                     # APR polygon id in apr_base.gpkg (matches 03_apr_linkage.R)

# --- 1. Load Entidades_CPV24 ------------------------------------------------
# Geometry layer from national cartography gpkg
entidades_geo <- st_read(
  file.path(CENSO_DIR, "cartografia", "Cartografia_censo2024_Pais.gpkg"),
  layer = "Entidades_CPV24", quiet = TRUE
)

# Tabular water counts from manzana-entidad dataset (joined by MANZENT)
entidades_tab <- fread(
  file.path(CENSO_DIR, "manzanas", "Base_manzana_entidad_CPV24.csv"),
  sep = ";", encoding = "Latin-1",
  select = c("MANZENT", "TIPO_MZ", VAR_RED_PUB, VAR_TOT_VIV),
  na.strings = c("", "NA")
)

entidades_raw <- entidades_geo |>
  left_join(as.data.frame(entidades_tab), by = "MANZENT")

message("=== Entidades_CPV24 columns ===")
message(paste(names(entidades_raw), collapse = "\n"))

entidades <- entidades_raw |>
  filter(TIPO_MZ == "RURAL") |>
  st_transform(32719) |>
  mutate(area_ent = as.numeric(st_area(geometry)))

message("Rural entities: ", nrow(entidades))

# --- 2. Load APR polygons ---------------------------------------------------
apr <- st_read(APR_FILE, quiet = TRUE) |>
  st_transform(32719)

message("APR polygons: ", nrow(apr))

# --- 3. Spatial intersection: entities × APR polygons ----------------------
# Each row of ent_apr is the piece of an entity that falls within an APR polygon.
ent_apr <- st_intersection(entidades, apr) |>
  mutate(area_intersect = as.numeric(st_area(geometry)))

# Area-weighted dwelling counts within each APR polygon
apr_cov <- ent_apr |>
  st_drop_geometry() |>
  mutate(
    frac_in_apr  = area_intersect / area_ent,
    n_red_in_apr = .data[[VAR_RED_PUB]] * frac_in_apr,
    n_viv_in_apr = .data[[VAR_TOT_VIV]] * frac_in_apr
  ) |>
  group_by(across(all_of(APR_ID_VAR))) |>
  summarise(
    n_red_pub = sum(n_red_in_apr, na.rm = TRUE),
    n_viv     = sum(n_viv_in_apr, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(prop_apr = if_else(n_viv > 0, n_red_pub / n_viv, NA_real_))

message("APR polygons with coverage data: ", sum(!is.na(apr_cov$prop_apr)))
message("Mean prop_apr: ", round(mean(apr_cov$prop_apr, na.rm = TRUE), 3))

# --- 4. Point-in-polygon: assign coverage rate to schools ------------------
school_pts <- readRDS(SCH_FILE) |>
  distinct(rbd, lat, lon) |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4674) |>
  st_transform(32719)

# Join APR coverage to APR polygon geometry
apr_with_cov <- apr |>
  left_join(apr_cov |> select(all_of(APR_ID_VAR), prop_apr),
            by = APR_ID_VAR)

# Spatial join: school point → APR polygon
school_census <- st_join(
  school_pts,
  apr_with_cov |> select(all_of(APR_ID_VAR), prop_apr),
  join = st_within
) |>
  st_drop_geometry() |>
  # Schools outside all APR polygons: prop_apr = 0 (not in any service area)
  mutate(prop_apr = replace_na(prop_apr, 0))

# Sanity check
message("=== School-level APR coverage ===")
message("  Schools with prop_apr > 0:  ", sum(school_census$prop_apr > 0))
message("  Schools with prop_apr = 0:  ", sum(school_census$prop_apr == 0))
message("  Mean prop_apr (all):        ", round(mean(school_census$prop_apr), 3))
message("  Mean prop_apr (treated > 0):", round(mean(school_census$prop_apr[school_census$prop_apr > 0]), 3))

saveRDS(school_census, file.path(OUT_DIR, "censo_apr_coverage.rds"))
message("Saved → ", file.path(OUT_DIR, "censo_apr_coverage.rds"))
