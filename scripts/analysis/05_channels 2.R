library(here)
library(fixest)
library(dplyr)

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
TAB_DIR <- here("output", "tables")

school_panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))
health_panel <- readRDS(file.path(OUT_DIR, "health_main_panel.rds"))

controls <- "dias_calor + deficit_precip + log(n_matricula + 1)"

base_sample <- function(df) {
  df |>
    group_by(rbd) |>
    mutate(ever_treated = any(treated == 1, na.rm = TRUE)) |>
    ungroup() |>
    filter(ever_treated | rural_rbd == 1)
}

run_pair <- function(fml, data, id_var) {
  list(
    cluster = feols(fml, data = data, cluster = as.formula(paste0("~", id_var)),
                    warn = FALSE, notes = FALSE),
    conley  = feols(fml, data = data,
                    vcov = conley(lat = "lat", lon = "lon", cutoff = 50),
                    warn = FALSE, notes = FALSE)
  )
}

# --- Canal 1: Violencia / clima escolar --------------------------------------
# Denuncias Superintendencia como proxy de conflictividad escolar

violence_outcomes <- c("n_den_total", "n_den_convivencia", "n_den_abuso")

violence_results <- lapply(violence_outcomes, function(y) {
  df <- base_sample(school_panel) |>
    mutate(y_log = log(.data[[y]] + 1))
  fml <- as.formula(paste("y_log ~ treated +", controls, "| rbd + year"))
  run_pair(fml, df, "rbd")
})
names(violence_results) <- violence_outcomes

etable(lapply(violence_results, `[[`, "cluster"),
       file = file.path(TAB_DIR, "channels_violence.tex"), replace = TRUE)
etable(lapply(violence_results, `[[`, "conley"),
       file = file.path(TAB_DIR, "channels_violence_conley.tex"), replace = TRUE)

# --- Canal 2: Salud (urgencias gastrointestinales) ---------------------------
# Health facility as unit; nearest APR as treatment

health_outcomes <- c("n_gi_total", "n_gi_nna", "n_gi_5a14")

health_results <- lapply(health_outcomes, function(y) {
  df <- health_panel |>
    filter(!is.na(lat)) |>
    mutate(y_log = log(.data[[y]] + 1))
  fml <- as.formula("y_log ~ treated + dias_calor + deficit_precip | id_estab + year")
  run_pair(fml, df, "id_estab")
})
names(health_results) <- health_outcomes

etable(lapply(health_results, `[[`, "cluster"),
       file = file.path(TAB_DIR, "channels_health.tex"), replace = TRUE)
etable(lapply(health_results, `[[`, "conley"),
       file = file.path(TAB_DIR, "channels_health_conley.tex"), replace = TRUE)

saveRDS(list(violence = violence_results, health = health_results),
        file.path(TAB_DIR, "channels_results.rds"))
message("Canales completo.")
