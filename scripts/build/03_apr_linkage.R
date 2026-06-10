library(here)
library(dplyr)
library(sf)

# apr_base: output of Python script — one row per APR system
# Expected columns: id, nombre, anio_instalacion, n_arranques, cod_com, geometry (polygon)
APR_BASE    <- here("data", "A_raw", "apr_ddbb", "apr_base.gpkg")
SCHOOL_FILE <- here("data", "A_raw", "apr_ddbb", "school_panel.rds")
OUT_FILE    <- here("data", "A_raw", "apr_ddbb", "school_apr.rds")

apr   <- st_read(APR_BASE, quiet = TRUE) |>
  st_transform(4326) |>
  rename_with(tolower) |>
  select(apr_id = id, apr_nombre = nombre, anio_instalacion, n_arranques, geometry)

schools <- readRDS(SCHOOL_FILE) |>
  filter(!is.na(lat), !is.na(lon))

schools_sf <- st_as_sf(schools, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Join: which APR polygon contains each school?
# Keeps all schools (left join); schools outside all polygons get NA apr_id
joined <- st_join(schools_sf, apr, join = st_within, left = TRUE)

school_apr <- joined |>
  st_drop_geometry() |>
  mutate(
    treated = as.integer(!is.na(apr_id) & year >= anio_instalacion)
  ) |>
  select(rbd, year, treated, apr_id, apr_nombre, anio_instalacion, n_arranques)

saveRDS(school_apr, OUT_FILE)
message("Saved: ", nrow(school_apr), " rows → ", OUT_FILE)
message("Treated school-years: ", sum(school_apr$treated, na.rm = TRUE))
