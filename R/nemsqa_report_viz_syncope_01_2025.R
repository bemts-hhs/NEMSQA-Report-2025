### IOWA NEMSQA REPORT VISUALIZATIONS Syncope-01 2025 ---------------------------

###_____________________________________________________________________________
# this script will contain all reporting visualizations for Syncope-01 use:
# nemsqa_report_prep_2025.R to get critical functions into memory
# nemsqa_report_syncope_01_2025.R to generate statistical files for the report
###_____________________________________________________________________________
# assume:
# that nemsqa_report_prep_2025.R was already ran to load needed packages
# and project-specific custom functions in the project
# nemsqa_report_syncope_01_2025.R was ran to generate statistical files
###_____________________________________________________________________________

### DATA -----------------------------------------------------------------------

# import statistical outputs for this measure
import_nemsqa_statistical_files(measure = "Syncope-01")

### TABLES ---------------------------------------------------------------------

### population data ############################################################

# generate the population gt table
syncope_01_pop_gt <- syncope_01_pop_years |>
  prepare_population_statistical_file() |>
  population_statistical_file_gt(measure = "Syncope-01", fig_dim = c(8, 40)) |>
  tab_style_hhs(
    message_text = "* Indicates masked data with n < 6. Population Trend horizontal lines indicate the arithmetic mean for that population group.",
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
export_nemsqa_gt(
  gt_object = syncope_01_pop_gt,
  measure = "Syncope-01",
  folder = "pop",
  extension = "png"
)

### results data ###############################################################

# generate the results gt table
syncope_01_results_gt <- syncope_01_result_year |>
  prepare_results_statistical_file() |>
  results_statistical_file_gt(groups = c("INCIDENT_YEAR")) |>
  # Add various source notes with icons from fontawesome
  gt::tab_source_note(source_note = gt::md(paste0(
    fontawesome::fa("note-sticky"),
    " * Indicates masked data with n < 6."
  ))) |>
  tab_style_hhs(
    message_text = "`Comparison` indicates the result with 95% confidence intervals.",
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
export_nemsqa_gt(
  gt_object = syncope_01_results_gt,
  measure = "Syncope-01",
  folder = "result",
  extension = "png"
)

# Load the shapefile into memory
# Run only once per session
iowa_counties_sf <- prepare_map_data()

# summarize performance statewide over the timeframe of interest
syncope_01_result_counties_map <- results_to_county_map(df = syncope_01_result_counties,
                                                       add_text = FALSE,
                                                       format = "percent")

# save the plot
ggplot2::ggsave(
  filename = "syncope_01_result_counties_map.png",
  plot = syncope_01_result_counties_map,
  path = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/NEMSQA Report/2025/output/Syncope-01/result",
  width = 7,
  height = 6
)
