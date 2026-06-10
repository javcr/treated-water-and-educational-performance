# =============================================================================
# 10_mde_montecarlo.R
# Minimum Detectable Effect (MDE) via Monte Carlo simulations
# Design: staggered adoption DiD, TWFE estimator
#
# Parameters marked [PLACEHOLDER] must be updated with real data
# from apr_base.gpkg once available.
# =============================================================================

library(fixest)
library(dplyr)
library(tibble)
library(ggplot2)

set.seed(2024)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. DESIGN PARAMETERS
# =============================================================================

# --- Panel ---
YEAR_MIN <- 2004
YEAR_MAX <- 2018   # up to last available SIMCE year

# --- Sample [PLACEHOLDER: update with real counts from apr_base] ---
N_SCHOOLS     <- 500    # total rural schools in sample
SHARE_TREATED <- 0.40   # fraction ever treated

# --- Adoption cohorts [PLACEHOLDER: real distribution of APR installation years] ---
# Each row: installation year, fraction of schools treated that year
COHORTS <- tibble(
  cohort = c(2005, 2007, 2009, 2011, 2013, 2015),
  share  = c(0.10, 0.15, 0.20, 0.25, 0.20, 0.10)
)

# --- Outcome variance [PLACEHOLDER: compute from real data] ---
# Defined here for SIMCE 4th-grade reading score (scale ~200-300, SD~50)
SD_SCHOOL_FE <- 40.0   # between-school variation
SD_YEAR_FE   <-  5.0   # between-year variation
SD_EPSILON   <- 20.0   # within-school residual variation
MEAN_OUTCOME <- 250.0  # [PLACEHOLDER] outcome mean for reporting MDE as %

# --- Monte Carlo ---
N_SIM        <- 1000   # simulaciones por punto de la grilla
ALPHA        <- 0.05   # nivel de significancia
TARGET_POWER <- 0.80   # poder objetivo

# --- Grilla de efectos a evaluar ---
# en unidades del outcome (puntos SIMCE)
BETAS <- seq(0, 20, length.out = 25)

# =============================================================================
# 2. SIMULATION FUNCTION
# =============================================================================

#' Estimate statistical power for a given treatment effect beta
#' @param beta  treatment effect (outcome units)
#' @return      fraction of simulations where H0 is rejected
simulate_power <- function(beta) {

  years     <- YEAR_MIN:YEAR_MAX
  n_years   <- length(years)
  n_treated <- round(N_SCHOOLS * SHARE_TREATED)
  n_control <- N_SCHOOLS - n_treated

  # Assign cohort to each treated school
  cohort_vec <- c(
    sample(COHORTS$cohort, size = n_treated, replace = TRUE, prob = COHORTS$share),
    rep(Inf, n_control)   # never treated
  )

  # School and year fixed effects (fixed across simulations for the same beta)
  fe_school <- rnorm(N_SCHOOLS, 0, SD_SCHOOL_FE)
  fe_year   <- rnorm(n_years,   0, SD_YEAR_FE)

  rejected <- logical(N_SIM)

  for (s in seq_len(N_SIM)) {
    panel <- expand.grid(rbd = seq_len(N_SCHOOLS), year = years) |>
      as_tibble() |>
      mutate(
        cohort    = cohort_vec[rbd],
        treated   = as.integer(year >= cohort),
        fe_s      = fe_school[rbd],
        fe_y      = fe_year[match(year, years)],
        outcome   = fe_s + fe_y + beta * treated + rnorm(n(), 0, SD_EPSILON)
      )

    fit <- tryCatch(
      feols(outcome ~ treated | rbd + year,
            data    = panel,
            cluster = ~rbd,
            warn    = FALSE,
            notes   = FALSE),
      error = function(e) NULL
    )

    if (!is.null(fit)) {
      pv <- pvalue(fit)["treated"]
      rejected[s] <- !is.na(pv) && pv < ALPHA
    }
  }

  mean(rejected)
}

# =============================================================================
# 3. RUN SIMULATIONS
# =============================================================================

message(sprintf(
  "Running %d simulations x %d grid points...",
  N_SIM, length(BETAS)
))

power_curve <- tibble(
  beta    = BETAS,
  power   = NA_real_,
  beta_sd = BETAS / (SD_SCHOOL_FE + SD_EPSILON)   # en unidades de SD aproximadas
)

for (i in seq_along(BETAS)) {
  message(sprintf("  beta = %.2f  [%d/%d]", BETAS[i], i, length(BETAS)))
  power_curve$power[i] <- simulate_power(BETAS[i])
}

# =============================================================================
# 4. COMPUTE MDE
# =============================================================================

# Linear interpolation to find beta with power = TARGET_POWER
mde_raw  <- approx(power_curve$power, power_curve$beta, xout = TARGET_POWER)$y
mde_pct  <- 100 * mde_raw / MEAN_OUTCOME   # efecto como % de la media

message("\n--- RESULTS ---")
message(sprintf("MDE (outcome units): %.2f", mde_raw))
message(sprintf("MDE (%% of mean):     %.2f%%", mde_pct))
message(sprintf("Schools in sample:   %d (%d treated, %d control)",
                N_SCHOOLS,
                round(N_SCHOOLS * SHARE_TREATED),
                round(N_SCHOOLS * (1 - SHARE_TREATED))))

# =============================================================================
# 5. POWER CURVE PLOT
# =============================================================================

p <- ggplot(power_curve, aes(x = beta, y = power)) +
  geom_line(linewidth = 1, color = "#2c7bb6") +
  geom_point(size = 2,    color = "#2c7bb6") +
  geom_hline(yintercept = TARGET_POWER, linetype = "dashed", color = "red",  linewidth = 0.8) +
  geom_vline(xintercept = mde_raw,      linetype = "dashed", color = "#d73027", linewidth = 0.8) +
  annotate(
    "label",
    x = mde_raw, y = 0.15,
    label = sprintf("MDE = %.1f pts\n(%.1f%% of mean)", mde_raw, mde_pct),
    hjust = -0.05, size = 3.5, color = "#d73027", fill = "white", label.size = 0
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title    = "Statistical power curve — staggered DiD (TWFE)",
    subtitle = sprintf(
      "N = %d schools · %d-%d · %d cohorts · alpha = %.2f · %d simulations",
      N_SCHOOLS, YEAR_MIN, YEAR_MAX, nrow(COHORTS), ALPHA, N_SIM
    ),
    x = "Treatment effect (SIMCE 4th-grade reading score points)",
    y = "Statistical power"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.subtitle = element_text(size = 9, color = "grey40"))

ggsave("output/figures/mde_power_curve.png", p, width = 8, height = 5, dpi = 300)
message("Plot saved to output/figures/mde_power_curve.png")

# =============================================================================
# 6. SUMMARY TABLE
# =============================================================================

summary_tbl <- tibble(
  parameter = c(
    "Total schools",
    "Treated schools",
    "Control schools",
    "Period",
    "Estimator",
    "Significance level (alpha)",
    "Target power",
    "Simulations",
    "MDE (outcome units)",
    "MDE (% of mean)"
  ),
  value = c(
    N_SCHOOLS,
    round(N_SCHOOLS * SHARE_TREATED),
    round(N_SCHOOLS * (1 - SHARE_TREATED)),
    paste(YEAR_MIN, YEAR_MAX, sep = "-"),
    "TWFE with school-level clustering",
    ALPHA,
    TARGET_POWER,
    N_SIM,
    round(mde_raw, 2),
    paste0(round(mde_pct, 1), "%")
  )
)

write.csv(power_curve,  "output/tables/mde_power_curve.csv",  row.names = FALSE)
write.csv(summary_tbl,  "output/tables/mde_summary.csv",      row.names = FALSE)
message("Tables saved to output/tables/")
