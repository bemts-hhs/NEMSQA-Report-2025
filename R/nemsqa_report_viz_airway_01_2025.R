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

# file path for exports
population_viz_path <-

### TABLES ---------------------------------------------------------------------

### population data ############################################################

# generate the population gt table
airway_01_pop_gt <- airway_01_pop_years |>
  prepare_population_statistical_file() |>
  population_statistical_file_gt(fig_dim = c(8, 40)) |>
  tab_style_hhs(table_title = "NEMSQA Airway-01 Populations: Iowa",
                table_subtitle = "For the years 2021-2024",
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
export_nemsqa_gt(gt_object = airway_01_pop_gt, measure = "Airway-01", folder = "pop", extension = "png")
