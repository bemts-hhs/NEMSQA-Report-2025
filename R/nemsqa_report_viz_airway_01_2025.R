### IOWA NEMSQA REPORT VISUALIZATIONS AIRWAY-01 2025 ---------------------------

###_____________________________________________________________________________
# this script will contain all reporting visualizations for Airway-01 use:
# nemsqa_report_prep_2025.R to get critical functions into memory
# nemsqa_report_airway_01_2025.R to generate statistical files for the report
###_____________________________________________________________________________
# assume:
# that nemsqa_report_prep_2025.R was already ran to load needed packages
# and project-specific custom functions in the project
# nemsqa_report_airway_01_2025.R was ran to generate statistical files
###_____________________________________________________________________________

### DATA -----------------------------------------------------------------------

# import statistical outputs for this measure
import_nemsqa_statistical_files(measure = "Airway-01")

### TABLES ---------------------------------------------------------------------

### population data ############################################################

airway_01_pop_years |>
  tidyr::pivot_wider(id_cols = filter,
                     names_from = YEAR,
                     values_from = count
                     ) |>
  dplyr::mutate(filter = stringr::str_replace_all(string = filter,
                                                  pattern = "(?:\\s)?call[s]?",
                                                  replacement = " runs")
                ) |>
  dplyr::rowwise() |>
  dplyr::mutate(`Population Trend` = list(c(`2021`, `2022`, `2023`, `2024`))) |>
  dplyr::ungroup() |>
  dplyr::mutate(dplyr::across(`2021`:`2024`, ~ traumar::small_count_label(., cutoff = 6, replacement = NA_integer_))) |>
  dplyr::rename(Populations = filter) |>
  gt::gt() |>
  gt::fmt_integer(columns = -Populations) |>
  gtExtras::gt_plt_sparkline(column = `Population Trend`,
                             type = "ref_mean",
                             palette = c("#70C8B8", "transparent", "#19405B", "#F27026", "#03617A"),
                             same_limit = FALSE,
                             label = TRUE
                             ) |>
  gt::sub_missing(columns = -Populations,
                  missing_text = "*"
                  ) |>
  tab_style_hhs(table_title = "Iowa Airway-01",
                table_subtitle = "For the years 2021:2024",
                message_text = "* Indicates masked data with n < 6. Population Trend horizontal lines indicate the arithmetic mean for that population group.",
                border_cols = -1
                )
