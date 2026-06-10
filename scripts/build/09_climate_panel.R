library(here)
library(dplyr)
library(terra)
library(purrr)

RAW_CR2     <- here("data", "A_raw", "CR2")
SCHOOL_FILE <- here("data", "A_raw", "apr_ddbb", "school_panel.rds")
OUT_FILE    <- here("data", "A_raw", "apr_ddbb", "climate_panel.rds")

PANEL_YEARS  <- 2004:2020
# Historical baseline for percentiles and mean: use full CR2 coverage (1979-2020)
HIST_YEARS   <- 1979:2020

schools <- readRDS(SCHOOL_FILE) |>
  distinct(rbd, lat, lon) |>
  filter(!is.na(lat), !is.na(lon))

pts <- vect(schools, geom = c("lon", "lat"), crs = "EPSG:4326")

# Extract full daily time series for all schools at once (n_schools × n_days)
load_nc <- function(pattern) {
  nc <- list.files(RAW_CR2, pattern = pattern, full.names = TRUE)[1]
  r  <- rast(nc)
  t  <- time(r)
  if (is.null(t)) stop("No time dimension in ", nc)
  vals  <- extract(r, pts, ID = FALSE)   # n_schools × n_days
  years <- as.integer(format(t, "%Y"))
  list(vals = vals, years = years, dates = t)
}

message("Loading tmax...")
tmax_nc <- load_nc("_tmax_")

message("Loading tmin...")
tmin_nc <- load_nc("_tmin_")

message("Loading precip...")
pr_nc   <- load_nc("_pr_")

# --- Per-school historical thresholds (p90 tmax, mean + sd precip) -----------
# Use full historical period for stable baselines

hist_idx <- which(tmax_nc$years %in% HIST_YEARS)

# p90 tmax per school across all historical days
p90_tmax <- apply(tmax_nc$vals[, hist_idx, drop = FALSE], 1,
                  quantile, probs = 0.90, na.rm = TRUE)

# Historical mean and sd of annual precipitation per school
hist_precip_annual <- map_dfc(HIST_YEARS, function(yr) {
  idx <- which(pr_nc$years == yr)
  rowSums(pr_nc$vals[, idx, drop = FALSE], na.rm = TRUE)
}) |> as.matrix()

precip_hist_mean <- rowMeans(hist_precip_annual, na.rm = TRUE)
precip_hist_sd   <- apply(hist_precip_annual, 1, sd, na.rm = TRUE)

# --- Annual variables per school × year --------------------------------------

message("Computing annual climate variables...")

climate_panel <- map_dfr(PANEL_YEARS, function(yr) {
  idx <- which(tmax_nc$years == yr)
  if (length(idx) == 0) return(NULL)

  # Raw averages
  tmax_mean <- rowMeans(tmax_nc$vals[, idx, drop = FALSE], na.rm = TRUE)
  tmin_mean <- rowMeans(tmin_nc$vals[, idx, drop = FALSE], na.rm = TRUE)
  precip_sum <- rowSums(pr_nc$vals[, idx, drop = FALSE], na.rm = TRUE)

  # Days with tmax above historical p90 (Aguilar-Gomez et al. 2024 approach)
  dias_calor <- rowSums(
    tmax_nc$vals[, idx, drop = FALSE] > p90_tmax,
    na.rm = TRUE
  )

  # Standardized precipitation deficit (Bobonis et al. 2022 approach):
  # (precip_yr - hist_mean) / hist_sd  — negative = drier than normal
  deficit_precip <- (precip_sum - precip_hist_mean) / precip_hist_sd

  tibble(
    rbd            = schools$rbd,
    year           = yr,
    tmax_anual     = tmax_mean,
    tmin_anual     = tmin_mean,
    precip_anual   = precip_sum,
    dias_calor     = dias_calor,      # days tmax > p90 historical
    deficit_precip = deficit_precip   # standardized precip deficit (neg = drought)
  )
})

saveRDS(climate_panel, OUT_FILE)
message("Saved: ", nrow(climate_panel), " school-years → ", OUT_FILE)
message("  Variables: tmax_anual, tmin_anual, precip_anual, dias_calor, deficit_precip")
