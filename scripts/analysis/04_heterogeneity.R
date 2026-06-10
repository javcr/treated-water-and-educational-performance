library(here)
library(fixest)
library(dplyr)
library(ggplot2)

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
FIG_DIR <- here("output", "figures")
TAB_DIR <- here("output", "tables")

panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))

panel_het <- panel |>
  group_by(rbd) |>
  mutate(ever_treated = any(treated == 1, na.rm = TRUE)) |>
  ungroup() |>
  filter(ever_treated | rural_rbd == 1) |>
  # Classify schools by historical climate exposure (pre-panel distribution)
  group_by(rbd) |>
  mutate(
    dias_calor_hist    = mean(dias_calor[year < 2010],    na.rm = TRUE),
    deficit_precip_hist = mean(deficit_precip[year < 2010], na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    # Terciles of heat exposure
    q_calor = ntile(dias_calor_hist, 3),
    # Terciles of drought (more negative deficit = drier)
    q_sequia = ntile(-deficit_precip_hist, 3),
    # High-exposure indicators (top tercile)
    high_heat   = as.integer(q_calor  == 3),
    high_drought = as.integer(q_sequia == 3)
  )

panel_het <- panel_het |>
  mutate(
    log_den_total       = log(n_den_total       + 1),
    log_den_convivencia = log(n_den_convivencia + 1),
    log_den_abuso       = log(n_den_abuso       + 1)
  )

OUTCOMES <- c("prom_lect", "prom_mate", "tasa_asistencia",
              "tasa_repitencia", "tasa_desercion",
              "log_den_total", "log_den_convivencia", "log_den_abuso")

controls <- "log(n_matricula + 1)"

# ---------------------------------------------------------------------------
# Part A: Treatment × climate interaction
# Captures whether schools in high-heat / high-drought areas benefit more.
# Two SE versions: cluster RBD (main table) + Conley 50km (appendix)
# ---------------------------------------------------------------------------
run_het <- function(y, var_het, vcov_type) {
  fml <- as.formula(paste(
    y, "~ i(treated,", var_het, ", ref = 0) + dias_calor + deficit_precip +",
    controls, "| rbd + year"
  ))
  if (vcov_type == "cluster") {
    feols(fml, data = panel_het, cluster = ~rbd, warn = FALSE, notes = FALSE)
  } else {
    feols(fml, data = panel_het,
          vcov = conley(lat = "lat", lon = "lon", cutoff = 50),
          warn = FALSE, notes = FALSE)
  }
}

het_results <- lapply(OUTCOMES, function(y) {
  list(
    heat_cluster    = run_het(y, "high_heat",    "cluster"),
    heat_conley     = run_het(y, "high_heat",    "conley"),
    drought_cluster = run_het(y, "high_drought", "cluster"),
    drought_conley  = run_het(y, "high_drought", "conley")
  )
})
names(het_results) <- OUTCOMES

for (y in OUTCOMES) {
  etable(list(het_results[[y]]$heat_cluster, het_results[[y]]$heat_conley),
         headers = c("Cluster RBD", "Conley 50km"),
         file = file.path(TAB_DIR, paste0("het_heat_", y, ".tex")), replace = TRUE)
  etable(list(het_results[[y]]$drought_cluster, het_results[[y]]$drought_conley),
         headers = c("Cluster RBD", "Conley 50km"),
         file = file.path(TAB_DIR, paste0("het_drought_", y, ".tex")), replace = TRUE)
}

# ---------------------------------------------------------------------------
# Part B: Subgroup DiDs — ATT estimated separately for each climate group
# Complements the interaction: shows absolute effects, not just the differential.
# ---------------------------------------------------------------------------
run_subgroup <- function(y, subsample, label) {
  df <- panel_het |> filter({{ subsample }}, !is.na(.data[[y]]))
  fml <- as.formula(paste(y, "~ treated + dias_calor + deficit_precip +",
                          controls, "| rbd + year"))
  list(
    cluster = feols(fml, data = df, cluster = ~rbd, warn = FALSE, notes = FALSE),
    conley  = feols(fml, data = df,
                    vcov = conley(lat = "lat", lon = "lon", cutoff = 50),
                    warn = FALSE, notes = FALSE)
  )
}

subgroups <- list(
  high_heat    = quote(high_heat   == 1),
  low_heat     = quote(high_heat   == 0),
  high_drought = quote(high_drought == 1),
  low_drought  = quote(high_drought == 0)
)

subgroup_results <- lapply(OUTCOMES, function(y) {
  lapply(names(subgroups), function(sg) {
    run_subgroup(y, !!subgroups[[sg]], sg)
  }) |> setNames(names(subgroups))
})
names(subgroup_results) <- OUTCOMES

# Table: high vs. low side-by-side for each climate dimension
for (y in OUTCOMES) {
  etable(
    list(subgroup_results[[y]]$high_heat$cluster,
         subgroup_results[[y]]$low_heat$cluster,
         subgroup_results[[y]]$high_drought$cluster,
         subgroup_results[[y]]$low_drought$cluster),
    headers = c("High heat", "Low heat", "High drought", "Low drought"),
    file = file.path(TAB_DIR, paste0("subgroup_", y, ".tex")), replace = TRUE
  )
}

saveRDS(list(interaction = het_results, subgroup = subgroup_results),
        file.path(TAB_DIR, "heterogeneity_results.rds"))
message("Climate heterogeneity complete.")
