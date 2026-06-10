library(here)
library(fixest)
library(dplyr)
library(ggplot2)

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
FIG_DIR <- here("output", "figures")
TAB_DIR <- here("output", "tables")

panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))

panel_did <- panel |>
  group_by(rbd) |>
  mutate(ever_treated = any(treated == 1, na.rm = TRUE),
         gname = if_else(ever_treated,
                         min(year[treated == 1], na.rm = TRUE),
                         0L)) |>
  ungroup() |>
  filter(ever_treated | rural_rbd == 1)

controls <- "dias_calor + deficit_precip + log(n_matricula + 1)"
OUTCOMES <- c("prom_lect", "prom_mate", "tasa_asistencia",
              "tasa_repitencia", "tasa_desercion")

# ---------------------------------------------------------------------------
# Placebo 1: Pre-trends — restrict to pre-treatment period
# A significant effect here violates the parallel trends assumption
# ---------------------------------------------------------------------------
panel_pre <- panel_did |>
  filter(is.na(gname) | year < gname) |>   # pre-treatment years only
  group_by(rbd) |>
  mutate(
    # Fake treatment: midpoint of available pre-treatment window
    pre_len     = n(),
    fake_treat  = as.integer(row_number() > floor(pre_len / 2))
  ) |>
  ungroup()

pretrend_results <- lapply(OUTCOMES, function(y) {
  feols(
    as.formula(paste(y, "~ fake_treat +", controls, "| rbd + year")),
    data = panel_pre, cluster = ~rbd, warn = FALSE, notes = FALSE
  )
})
names(pretrend_results) <- OUTCOMES

etable(pretrend_results,
       file = file.path(TAB_DIR, "placebo_pretrends.tex"), replace = TRUE)

# ---------------------------------------------------------------------------
# Placebo 2: Random timing
# Randomly reassign treatment years and re-estimate
# Under a robust estimator, placebo effects should cluster around zero
# ---------------------------------------------------------------------------
set.seed(42)
N_SIM <- 200

run_placebo_sim <- function(y, sim_i) {
  df <- panel_did |>
    filter(!is.na(.data[[y]])) |>
    group_by(rbd) |>
    mutate(
      gname_fake = if_else(
        ever_treated,
        sample(unique(year[year >= min(year) + 2 & year <= max(year) - 2]), 1),
        0L
      ),
      treated_fake = as.integer(year >= gname_fake & gname_fake > 0)
    ) |>
    ungroup()

  m <- feols(
    as.formula(paste(y, "~ treated_fake +", controls, "| rbd + year")),
    data = df, cluster = ~rbd, warn = FALSE, notes = FALSE
  )
  data.frame(sim = sim_i, outcome = y, coef = coef(m)["treated_fake"],
             se = se(m)["treated_fake"])
}

message("Running ", N_SIM, " random timing simulations...")
placebo_sims <- lapply(OUTCOMES, function(y) {
  lapply(seq_len(N_SIM), run_placebo_sim, y = y) |> bind_rows()
}) |> bind_rows()

# Compare placebo distribution vs real estimate
real_ests <- lapply(OUTCOMES, function(y) {
  m <- feols(
    as.formula(paste(y, "~ treated +", controls, "| rbd + year")),
    data = panel_did, cluster = ~rbd, warn = FALSE, notes = FALSE
  )
  data.frame(outcome = y, coef_real = coef(m)["treated"])
}) |> bind_rows()

p_placebo <- placebo_sims |>
  left_join(real_ests, by = "outcome") |>
  ggplot(aes(x = coef)) +
  geom_histogram(bins = 40, fill = "gray70", color = "white") +
  geom_vline(aes(xintercept = coef_real), color = "red", linewidth = 1) +
  facet_wrap(~outcome, scales = "free") +
  labs(title = "Placebo: distribution of effects with random timing",
       subtitle = "Red line = real estimate",
       x = "Placebo coefficient", y = "Frequency") +
  theme_minimal()

ggsave(file.path(FIG_DIR, "placebo_random_timing.png"),
       p_placebo, width = 12, height = 8)

# ---------------------------------------------------------------------------
# Placebo 3: Outcome that should not be affected by treatment
# Use school characteristics predetermined at treatment
# (e.g. cod_depe — school dependency type should not change due to APR)
# ---------------------------------------------------------------------------
cod_depe_placebo <- feols(
  cod_depe ~ treated + dias_calor + deficit_precip | rbd + year,
  data = panel_did, cluster = ~rbd, warn = FALSE, notes = FALSE
)
message("Placebo outcome (cod_depe) — coef treated: ",
        round(coef(cod_depe_placebo)["treated"], 4))

saveRDS(list(pretrends  = pretrend_results,
             simulations = placebo_sims,
             outcome_placebo = cod_depe_placebo),
        file.path(TAB_DIR, "placebo_results.rds"))

message("Placebos complete. Figures → ", FIG_DIR)
