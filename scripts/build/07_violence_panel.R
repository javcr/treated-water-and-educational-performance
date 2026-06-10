library(here)
library(dplyr)
library(readr)
library(purrr)

RAW_DEN  <- here("data", "A_raw", "SUPERINTENDENCIA_EDUCACION", "Denuncias")
OUT_FILE <- here("data", "A_raw", "apr_ddbb", "violence_panel.rds")

PANEL_YEARS <- 2014:2021

read_denuncias <- function(year) {
  den_dir <- file.path(RAW_DEN, paste0("DEN_", year), paste("Denuncias", year))
  csv <- list.files(den_dir, pattern = "PUBL\\.csv$", full.names = TRUE)[1]
  if (is.na(csv)) {
    message("  No CSV found for Denuncias year ", year)
    return(NULL)
  }
  read_delim(csv, delim = ";", show_col_types = FALSE,
             locale = locale(encoding = "UTF-8"),
             name_repair = "minimal") |>
    select(any_of(c("AGNO", "RBD", "DEN_AMBITO", "DEN_TEMA",
                    "EE_COD_REGION", "EE_COD_COMUNA")))
}

den_raw <- map(PANEL_YEARS, read_denuncias) |> compact() |> list_rbind()

violence_panel <- den_raw |>
  rename_with(tolower) |>
  filter(!is.na(rbd)) |>
  mutate(
    convivencia = grepl("CONVIVENCIA", toupper(den_ambito)),
    abuso       = grepl("ABUSO|ACOSO|MALTRATO|VIOLENCIA", toupper(den_tema))
  ) |>
  group_by(rbd, year = agno) |>
  summarise(
    n_den_total       = n(),
    n_den_convivencia = sum(convivencia, na.rm = TRUE),
    n_den_abuso       = sum(abuso,       na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(violence_panel, OUT_FILE)
message("Saved: ", nrow(violence_panel), " rows, years ",
        min(violence_panel$year), "–", max(violence_panel$year), " → ", OUT_FILE)
