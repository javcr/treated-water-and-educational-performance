library(here)
library(dplyr)
library(sf)

OUT_DIR  <- here("data", "A_raw", "apr_ddbb")
APR_BASE <- here("data", "A_raw", "apr_ddbb", "apr_base.gpkg")
OUT_FILE <- file.path(OUT_DIR, "health_main_panel.rds")

PANEL_YEARS <- 2008:2020

deis    <- readRDS(file.path(OUT_DIR, "deis_panel.rds"))

apr <- st_read(APR_BASE, quiet = TRUE) |>
  st_transform(4326) |>
  rename_with(tolower) |>
  select(apr_id = id, apr_nombre = nombre, anio_instalacion, n_arranques)

# Centroid of each APR polygon for distance calculation
apr_pts <- st_centroid(apr)

# Health facilities with coordinates
deis_sf <- deis |>
  filter(!is.na(lat), !is.na(lon)) |>
  distinct(id_estab, nom_estab, lat, lon, tipo_estab, tiene_urgencia) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Nearest APR for each health facility
nearest_idx <- st_nearest_feature(deis_sf, apr_pts)

deis_apr <- deis_sf |>
  st_drop_geometry() |>
  mutate(
    apr_id            = apr$apr_id[nearest_idx],
    apr_nombre        = apr$apr_nombre[nearest_idx],
    anio_instalacion  = apr$anio_instalacion[nearest_idx],
    dist_km           = as.numeric(st_distance(
      deis_sf,
      apr_pts[nearest_idx, ],
      by_element = TRUE
    )) / 1000
  )

# Balanced panel: facility × year
health_panel <- expand_grid(
    id_estab = unique(deis_apr$id_estab),
    year     = PANEL_YEARS
  ) |>
  left_join(deis_apr,
            by = "id_estab") |>
  left_join(deis |> select(-nom_estab, -tipo_estab, -tiene_urgencia,
                            -lat, -lon),
            by = c("id_estab", "year")) |>
  mutate(
    treated = as.integer(!is.na(anio_instalacion) & year >= anio_instalacion)
  ) |>
  arrange(id_estab, year)

saveRDS(health_panel, OUT_FILE)

message("=== Health main panel ===")
message("  Rows:                ", nrow(health_panel))
message("  Facilities:          ", n_distinct(health_panel$id_estab))
message("  With coordinates:    ", sum(!is.na(health_panel$lat) &
                                       health_panel$year == min(PANEL_YEARS)))
message("  Treated facil-years: ", sum(health_panel$treated == 1, na.rm = TRUE))
message("  GI urgency obs:      ", sum(!is.na(health_panel$n_gi_total)))
message("  Median dist. to nearest APR: ",
        round(median(health_panel$dist_km, na.rm = TRUE), 1), " km")
message("Saved → ", OUT_FILE)
