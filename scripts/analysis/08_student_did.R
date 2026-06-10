library(here)
library(fixest)
library(data.table)
library(dplyr)
library(ggplot2)

# Student-level DiD analysis (MRUN × year).
# Advantage over school-level: student fixed effects absorb all time-invariant
# student characteristics (ability, family background, gender, etc.).
# Treatment is at the school level (rbd × year) — cluster SEs at rbd.
#
# Panel outcomes (2010-2020): prom_gral, asistencia, aprobado, repite, abandona,
#                              emigra_rural
#
# Long-run outcome — exposure design (Duflo 2001):
#   Treatment = years of secondary school (grades 7-12) spent in an APR-treated
#   school. Identification: within the same school, graduation cohorts differ in
#   how many years of APR they were exposed to before graduating. FEs: school +
#   graduation cohort absorb school-level trends and cohort-level time trends.

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
FIG_DIR <- here("output", "figures")
TAB_DIR <- here("output", "tables")

student <- readRDS(file.path(OUT_DIR, "student_main_panel.rds")) |> setDT()
univ    <- readRDS(file.path(OUT_DIR, "univ_panel.rds"))

# ---------------------------------------------------------------------------
# 1. Prepare student panel outcomes
# ---------------------------------------------------------------------------
student[, `:=`(
  aprobado  = as.integer(sit_fin == "P"),
  repite    = as.integer(sit_fin == "R"),
  abandona  = as.integer(sit_fin == "Y"),
  log_n_mat = log(n_matricula + 1),
  secondary = as.integer(cod_grado >= 7)   # grades 7-12
)]

# Restrict to same sample as school-level analysis
student <- student[
  student[, .(ever_treated = any(treated == 1, na.rm = TRUE)), by = rbd],
  on = "rbd"
][ever_treated == TRUE | rural_rbd == 1]

# ---------------------------------------------------------------------------
# 2. Exposure design — student-level summary
#    For each student, compute APR exposure during secondary school (grades 7-12)
#    and estimate their graduation cohort.
# ---------------------------------------------------------------------------
student_summary <- student[, .(
  # APR exposure: years in secondary school while school was treated
  years_exposed_sec = sum(treated == 1L & cod_grado >= 7L, na.rm = TRUE),
  years_in_sec      = sum(cod_grado >= 7L, na.rm = TRUE),
  # Total exposure (all grades)
  years_exposed_all = sum(treated == 1L, na.rm = TRUE),
  years_in_panel    = .N,
  # School and last observed grade/year
  rbd               = rbd[which.max(year)],
  last_grade        = cod_grado[which.max(year)],
  last_year         = max(year),
  anio_instalacion  = anio_instalacion[1],
  ever_treated      = any(treated == 1L, na.rm = TRUE),
  # Covariates (time-average for cross-section)
  lat               = lat[1],
  lon               = lon[1],
  gen_alu           = gen_alu[1],
  dias_calor        = mean(dias_calor,    na.rm = TRUE),
  deficit_precip    = mean(deficit_precip, na.rm = TRUE),
  log_n_mat         = mean(log(n_matricula + 1), na.rm = TRUE)
), by = mrun]

# Estimated graduation year: last observed year + years left to grade 12
# (capped: if last_grade > 12 set to last_year)
student_summary[, grad_year_est := last_year + pmax(0L, 12L - last_grade)]

# Exposure share: fraction of secondary school years under APR treatment
student_summary[, share_exposed_sec := fcase(
  years_in_sec > 0L, years_exposed_sec / years_in_sec,
  default           = NA_real_
)]

# Binary: school was treated before student graduated
student_summary[, treated_before_grad := as.integer(
  !is.na(anio_instalacion) & anio_instalacion <= grad_year_est
)]

# Years between APR installation and graduation (for event-study-style plot)
# Negative = graduated before APR arrived; positive = graduated after
student_summary[, yrs_to_grad_from_apr := fcase(
  !is.na(anio_instalacion), grad_year_est - anio_instalacion,
  default             = NA_integer_
)]

# ---------------------------------------------------------------------------
# 3. Merge university enrollment outcome
# ---------------------------------------------------------------------------
univ_first <- univ$first[, .(mrun, anio_primer_es,
                               univ_alguna_vez, cft_ip_alguna_vez)]

student_lr <- student_summary[univ_first, on = "mrun", nomatch = NA][, `:=`(
  # Enrolled within 2 years of estimated graduation
  ingreso_es_2yr = as.integer(!is.na(anio_primer_es) &
                                anio_primer_es <= grad_year_est + 2L),
  ingreso_univ   = univ_alguna_vez,
  ingreso_cft_ip = cft_ip_alguna_vez
)]
# Students not in univ data → not enrolled
student_lr[is.na(ingreso_univ),   ingreso_univ   := 0L]
student_lr[is.na(ingreso_cft_ip), ingreso_cft_ip := 0L]
student_lr[is.na(ingreso_es_2yr), ingreso_es_2yr := 0L]

# ---------------------------------------------------------------------------
# 4. Static TWFE — panel outcomes with student FEs
#    Y_ist = alpha_i (student FE) + lambda_t (year FE) +
#            beta * treated_rt + controls + eps_ist
# ---------------------------------------------------------------------------
PANEL_OUTCOMES <- c(
  "prom_gral", "asistencia",
  "aprobado", "repite", "abandona",
  "emigra_rural"
)

controls_panel <- "dias_calor + deficit_precip + log_n_mat"

run_student_twfe <- function(y) {
  df <- student[!is.na(student[[y]]) & !is.na(mrun)]
  fml <- as.formula(paste(y, "~ treated +", controls_panel, "| mrun + year"))
  list(
    cluster = feols(fml, data = df, cluster = ~rbd,
                    warn = FALSE, notes = FALSE),
    conley  = feols(fml, data = df,
                    vcov = conley(lat = "lat", lon = "lon", cutoff = 50),
                    warn = FALSE, notes = FALSE)
  )
}

panel_results <- lapply(PANEL_OUTCOMES, run_student_twfe)
names(panel_results) <- PANEL_OUTCOMES

etable(lapply(panel_results, `[[`, "cluster"),
       file = file.path(TAB_DIR, "student_twfe_panel.tex"), replace = TRUE)
etable(lapply(panel_results, `[[`, "conley"),
       file = file.path(TAB_DIR, "student_twfe_panel_conley.tex"), replace = TRUE)

# ---------------------------------------------------------------------------
# 5. Long-run outcome — exposure design
#
# Spec A (continuous): Y_i = alpha_s + gamma_c + beta * years_exposed_sec + X + e
#   Variation: within same school, graduation cohorts differ in years of APR exposure
#
# Spec B (binary):     Y_i = alpha_s + gamma_c + beta * treated_before_grad + X + e
#   Cleaner but loses intensity information
#
# FEs: school (rbd) + graduation cohort (grad_year_est)
# ---------------------------------------------------------------------------
LR_OUTCOMES <- c("ingreso_es_2yr", "ingreso_univ", "ingreso_cft_ip")
controls_lr <- "gen_alu + dias_calor + deficit_precip + log_n_mat"

run_lr_exposure <- function(y) {
  df <- student_lr[!is.na(student_lr[[y]]) & years_in_sec > 0]

  # Spec A: continuous exposure (years in treated secondary school)
  fml_a <- as.formula(paste(
    y, "~ years_exposed_sec +", controls_lr, "| rbd + grad_year_est"
  ))
  # Spec B: binary (treated before graduation)
  fml_b <- as.formula(paste(
    y, "~ treated_before_grad +", controls_lr, "| rbd + grad_year_est"
  ))

  list(
    exposure_cluster = feols(fml_a, data = df, cluster = ~rbd,
                             warn = FALSE, notes = FALSE),
    exposure_conley  = feols(fml_a, data = df,
                             vcov = conley(lat = "lat", lon = "lon", cutoff = 50),
                             warn = FALSE, notes = FALSE),
    binary_cluster   = feols(fml_b, data = df, cluster = ~rbd,
                             warn = FALSE, notes = FALSE),
    binary_conley    = feols(fml_b, data = df,
                             vcov = conley(lat = "lat", lon = "lon", cutoff = 50),
                             warn = FALSE, notes = FALSE)
  )
}

lr_results <- lapply(LR_OUTCOMES, run_lr_exposure)
names(lr_results) <- LR_OUTCOMES

# Table: continuous exposure (cols) + binary (cols), cluster SEs
for (y in LR_OUTCOMES) {
  etable(list(lr_results[[y]]$exposure_cluster,
              lr_results[[y]]$binary_cluster),
         headers = c("Years exposed", "Treated before grad"),
         file = file.path(TAB_DIR, paste0("student_longrun_", y, ".tex")),
         replace = TRUE)
}

# ---------------------------------------------------------------------------
# 6. Dose-response plot
#    Average university enrollment rate by years of APR exposure.
#    Intuition check before running regressions.
# ---------------------------------------------------------------------------
dose_response <- student_lr[years_in_sec > 0 & !is.na(ingreso_univ),
  .(rate_univ = mean(ingreso_univ),
    rate_es   = mean(ingreso_es_2yr),
    n         = .N),
  by = years_exposed_sec
][order(years_exposed_sec)]

p_dose <- ggplot(dose_response, aes(x = years_exposed_sec)) +
  geom_line(aes(y = rate_univ,  color = "University"),   linewidth = 0.9) +
  geom_line(aes(y = rate_es,    color = "Any higher ed"), linewidth = 0.9) +
  geom_point(aes(y = rate_univ, size = n, color = "University")) +
  geom_point(aes(y = rate_es,   size = n, color = "Any higher ed")) +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  labs(title = "Dose-response: APR exposure during secondary school vs. higher ed enrollment",
       x = "Years in APR-treated secondary school",
       y = "Share enrolled in higher education",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(FIG_DIR, "dose_response_univ.png"),
       p_dose, width = 8, height = 5)

# ---------------------------------------------------------------------------
# 7. Event-study style plot for long-run outcome
#    X-axis: years between APR installation and student's estimated graduation.
#    Negative = graduated before APR arrived (not exposed); positive = after.
# ---------------------------------------------------------------------------
es_lr <- student_lr[
  !is.na(yrs_to_grad_from_apr) & !is.na(ingreso_univ),
  .(rate_univ = mean(ingreso_univ), n = .N),
  by = .(yrs_bin = fcase(
    yrs_to_grad_from_apr <= -4L, -4L,
    yrs_to_grad_from_apr >=  6L,  6L,
    default = yrs_to_grad_from_apr
  ))
][order(yrs_bin)]

p_es_lr <- ggplot(es_lr, aes(x = yrs_bin, y = rate_univ)) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.9, color = "steelblue") +
  geom_point(aes(size = n), color = "steelblue") +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  labs(title = "University enrollment by years between APR installation and graduation",
       subtitle = "Negative = graduated before APR arrived",
       x = "Graduation year − APR installation year",
       y = "Share enrolled in university") +
  theme_minimal()

ggsave(file.path(FIG_DIR, "eventstudy_longrun_univ.png"),
       p_es_lr, width = 8, height = 5)

# ---------------------------------------------------------------------------
# 8. Panel event study (student FEs, primary outcomes)
# ---------------------------------------------------------------------------
student[, rel_year_bin := fcase(
  is.na(anio_instalacion),         NA_real_,
  year - anio_instalacion <= -5L,  -5,
  year - anio_instalacion >=  5L,   5,
  default = as.numeric(year - anio_instalacion)
)]

es_panel <- lapply(c("prom_gral", "asistencia", "aprobado"), function(y) {
  df <- student[!is.na(student[[y]]) & !is.na(rel_year_bin)]
  fml <- as.formula(paste(
    y, "~ i(rel_year_bin, ref = -1) +", controls_panel, "| mrun + year"
  ))
  feols(fml, data = df, cluster = ~rbd, warn = FALSE, notes = FALSE)
})
names(es_panel) <- c("prom_gral", "asistencia", "aprobado")

for (y in names(es_panel)) {
  png(file.path(FIG_DIR, paste0("eventstudy_student_", y, ".png")),
      width = 800, height = 500)
  iplot(es_panel[[y]],
        main = paste("Event study (student level) —", y),
        xlab = "Years since APR installation",
        ylab = "Estimated effect")
  abline(v = -0.5, lty = 2, col = "gray50")
  dev.off()
}

saveRDS(list(panel      = panel_results,
             longrun    = lr_results,
             eventstudy = es_panel),
        file.path(TAB_DIR, "student_results.rds"))

message("=== Student-level DiD complete ===")
message("  Panel outcomes:    ", paste(PANEL_OUTCOMES, collapse = ", "))
message("  Long-run outcomes: ", paste(LR_OUTCOMES, collapse = ", "))
message("  Outputs → ", TAB_DIR, " | ", FIG_DIR)
