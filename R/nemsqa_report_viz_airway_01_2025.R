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

# generate the population gt table
airway_01_pop_gt <- airway_01_pop_years |>
  prepare_population_statistical_file() |>
  population_statistical_file_gt(measure = "Airway-01", fig_dim = c(8, 40)) |>
  tab_style_hhs(message_text = "* Indicates masked data with n < 6. Population Trend horizontal lines indicate the arithmetic mean for that population group.",
                border_cols = -1,
                row_groups = 25,
                column_labels = 25,
                title = 35,
                subtitle = 33,
                spanners = 31,
                body = 22,
                source_note = 20,
                footnote = 20
                )

# save the table
export_nemsqa_gt(gt_object = airway_01_pop_gt, measure = "Airway-01", folder = "pop", extension = "png")

### results data ###############################################################

# generate the results gt table
airway_01_results_gt <- airway_01_result_year |>
  prepare_results_statistical_file() |>
  results_statistical_file_gt(groups = c("INCIDENT_YEAR")) |>
  # Add various source notes with icons from fontawesome
  gt::tab_source_note(source_note = gt::md(paste0(
    fontawesome::fa("note-sticky"),
    " ",
    "* Indicates masked data with n < 6."
  ))) |>
  tab_style_hhs(message_text = "`Comparison` indicates the result with 95% confidence intervals.",
                border_cols = c(-1, -2),
                row_groups = 25,
                column_labels = 25,
                title = 35,
                subtitle = 28,
                spanners = 31,
                body = 22,
                source_note = 20,
                footnote = 20
                )

# save the table
export_nemsqa_gt(gt_object = airway_01_results_gt, measure = "Airway-01", folder = "result", extension = "png")

# FIXME try a tigris shapefile table
iowa_counties_sf <- tigris::counties(state = "Iowa")

# summarize performance statewide over the timeframe of interest
iowa_counties_sf |>
  dplyr::left_join(county_data, by = dplyr::join_by(NAME == County)) |>
  dplyr::left_join(airway_01_result_regions, by = dplyr::join_by(`Region: Preparedness`)) |>
  ggplot2::ggplot(ggplot2::aes(fill = prop)) +
  ggplot2::geom_sf() +
  viridis::scale_fill_viridis(option = "magma", direction = -1) +
  ggplot2::theme_void()
