library(here)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

# Purpose: pre-analysis baseline diagnostics for the Informe de Avance (Sep 2026).
# Staggered-DiD equivalent of a balance table + power check.
# Outputs: cohort table, covariate balance by cohort, pre-treatment trends plot.

OUT_DIR <- here("data", "A_raw", "apr_ddbb")
FIG_DIR <- here("output", "figures")
TAB_DIR <- here("output", "tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

panel <- readRDS(file.path(OUT_DIR, "school_main_panel.rds"))

panel_did <- panel |>
  group_by(rbd) |>
  mutate(
    ever_treated = any(treated == 1, na.rm = TRUE),
    gname = if_else(ever_treated, min(year[treated == 1], na.rm = TRUE), 0L)
  ) |>
  ungroup() |>
  filter(ever_treated | rural_rbd == 1)

# ---------------------------------------------------------------------------
# 1. Cohort table
#    How many schools enter treatment each year (the staggered pattern).
#    Never-treated schools shown separately.
# ---------------------------------------------------------------------------
cohort_tab <- panel_did |>
  distinct(rbd, gname, ever_treated) |>
  mutate(cohort = if_else(gname == 0, "Never treated", as.character(gname))) |>
  count(cohort, name = "n_schools") |>
  arrange(cohort)

write.csv(cohort_tab, file.path(TAB_DIR, "cohort_table.csv"), row.names = FALSE)
message("Cohort table:")
print(cohort_tab)

# Bar chart of cohort sizes
p_cohort <- cohort_tab |>
  filter(cohort != "Never treated") |>
  ggplot(aes(x = cohort, y = n_schools)) +
  geom_col(fill = "steelblue") +
  labs(title = "Schools entering APR treatment by year",
       x = "Year of first treatment", y = "Number of schools") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(FIG_DIR, "cohort_distribution.png"),
       p_cohort, width = 8, height = 5)

# ---------------------------------------------------------------------------
# 2. Covariate balance by cohort
#    Compare pre-treatment means of key school characteristics across cohorts
#    and vs. never-treated. Analogous to a balance table in an RCT.
#    Uses only pre-treatment observations for each school.
# ---------------------------------------------------------------------------
BALANCE_VARS <- c(
  "n_matricula",      # school size
  "tasa_asistencia",  # baseline attendance
  "tasa_repitencia",  # baseline grade repetition
  "prom_lect",        # baseline SIMCE reading
  "prom_mate",        # baseline SIMCE math
  "dias_calor",       # historical heat exposure
  "deficit_precip"    # historical precipitation deficit
)

# Pre-treatment means: for each school, average before first treatment year
pre_means <- panel_did |>
  filter(gname == 0 | year < gname) |>      # pre-treatment period per school
  group_by(rbd, gname) |>
  summarise(across(all_of(BALANCE_VARS), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(cohort_group = case_when(
    gname == 0                     ~ "Never treated",
    gname <= quantile(gname[gname > 0], 0.33, na.rm = TRUE) ~ "Early adopters",
    gname <= quantile(gname[gname > 0], 0.66, na.rm = TRUE) ~ "Mid adopters",
    TRUE                           ~ "Late adopters"
  ))

balance_tab <- pre_means |>
  group_by(cohort_group) |>
  summarise(
    n = n(),
    across(all_of(BALANCE_VARS),
           list(mean = \(x) round(mean(x, na.rm = TRUE), 3),
                sd   = \(x) round(sd(x,   na.rm = TRUE), 3)),
           .names = "{.col}__{.fn}")
  ) |>
  arrange(factor(cohort_group,
                 levels = c("Early adopters", "Mid adopters",
                            "Late adopters", "Never treated")))

write.csv(balance_tab, file.path(TAB_DIR, "balance_by_cohort.csv"), row.names = FALSE)
message("Balance table saved → ", file.path(TAB_DIR, "balance_by_cohort.csv"))

# F-test: joint significance of cohort differences (one variable at a time)
balance_ftests <- sapply(BALANCE_VARS, function(v) {
  df <- pre_means |> filter(!is.na(.data[[v]]))
  fit <- lm(as.formula(paste(v, "~ factor(cohort_group)")), data = df)
  pval <- anova(fit)[["Pr(>F)"]][1]
  round(pval, 4)
}, USE.NAMES = TRUE)

message("F-test p-values (cohort balance):")
print(balance_ftests)
write.csv(data.frame(variable = names(balance_ftests), pvalue = balance_ftests),
          file.path(TAB_DIR, "balance_ftests.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Pre-treatment trends plot
#    Average outcomes over calendar time for treated vs. never-treated schools.
#    The visual parallel-trends check before any normalization to event time.
# ---------------------------------------------------------------------------
OUTCOMES <- c("prom_lect", "prom_mate", "tasa_asistencia",
              "tasa_repitencia", "tasa_desercion")

trends_data <- panel_did |>
  mutate(group = if_else(ever_treated, "Ever treated", "Never treated")) |>
  group_by(year, group) |>
  summarise(across(all_of(OUTCOMES),
                   \(x) mean(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(all_of(OUTCOMES), names_to = "outcome", values_to = "mean_val")

p_trends <- ggplot(trends_data, aes(x = year, y = mean_val,
                                     color = group, linetype = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~outcome, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Ever treated" = "steelblue",
                                "Never treated" = "gray40")) +
  labs(title = "Pre-treatment outcome trends: treated vs. never-treated schools",
       subtitle = "Parallel trends assumption — visual check",
       x = "Year", y = "Mean outcome",
       color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(FIG_DIR, "pretrends_raw.png"),
       p_trends, width = 10, height = 8)

message("Pre-treatment trends plot saved.")
message("=== Balance baseline complete. Outputs → ", TAB_DIR, " | ", FIG_DIR)
