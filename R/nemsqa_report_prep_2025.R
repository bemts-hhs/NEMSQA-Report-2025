### IOWA NEMSQAR REPORT PREP 2025 ----------------------------------------------

# This script prepares for the analyses using the `nemsqar` package v1.1.0

### PACKAGES -------------------------------------------------------------------

# CRAN versions ================================================================

# install these packages if not already

# install.packages(c("tidyverse", "traumar", "devtools", "remotes", "janitor",
#                    "gt", "gtsummary", "gtExtras", "zipcodeR", "naniar",
#                    "ggrepel", "devtools", "renv", "roxygen2",
#                    "roxygen2md", "nemsqar", "extrafont"
#                   ))

# load packages ----
library(tidyverse)
library(traumar)
library(devtools)
library(remotes)
library(nemsqar)
library(janitor)
library(gt)
library(gtsummary)
library(gtExtras)
library(zipcodeR)
library(naniar)
library(ggrepel)
library(devtools)
library(renv)
library(roxygen2)
library(roxygen2md)
library(extrafont)
library(fontawesome)

# Handy Functions ==============================================================

###_____________________________________________________________________________
# After observing the different problems with Iowa counties, we can
# clean these county names so they are uniform and spelled correctly using
# regex within a custom map() function
# add nature of injury data
###_____________________________________________________________________________

# Clean County Names in EMS Data
# This function standardizes county names in an EMS dataset by:
# - Removing unnecessary suffixes (e.g., "County", "Co").
# - Correcting common misspellings using regex patterns.
# - Inferring county names based on city names or ZIP codes when available.
  function(df,
           county_column,
           city_column,
           zip_column) {

    # let x be a named column within a data.frame


    if (!is.data.frame(df) && !is_tibble(df)) {

      cli::cli_abort(
        "The first argument `df` of the input was of class {.cls {class(df)}}but must be a {.cls data.frame}.  Please supply a {.cls data.frame} to the argument {.var df}."
      )

    }

    clean_counties <- df |>
      dplyr::mutate({
        {
          county_column
        }
      } := str_remove_all({
        {
          county_column
        }
      }, pattern = "(?:\\sCounty|\\scounty|/.*$|\\sCo$)"),
      {
        {
          county_column
        }
      } := str_to_title({
        {
          county_column
        }
      })) |>
      dplyr::mutate({
        {
          county_column
        }
      } := dplyr::if_else(
        grepl(pattern = "(?:Al[l]*am[a]*kee)", {
          {
            county_column
          }
        }, ignore.case = TRUE),
        "Allamakee",
        # using various regex formulations to address mispellings to standardize county
        dplyr::if_else(
          grepl(pattern = "(?:[0-9]+)", {
            {
              county_column
            }
          }, ignore.case = TRUE),
          new_county,
          dplyr::if_else(
            grepl(
              pattern = "waterloo",
              x = {
                {
                  city_column
                }
              },
              ignore.case = TRUE
            ),
            "Black Hawk",
            dplyr::if_else(
              grepl(
                pattern = "saint clair",
                x = {
                  {
                    city_column
                  }
                },
                ignore.case = TRUE
              ),
              "Benton",
              dplyr::if_else(
                grepl(pattern = "(?:Audrain)", {
                  {
                    county_column
                  }
                }, ignore.case = TRUE),
                new_county,
                dplyr::if_else(
                  grepl(pattern = "(?:Blair)", {
                    {
                      county_column
                    }
                  }, ignore.case = TRUE),
                  new_county,
                  dplyr::if_else(
                    grepl(
                      pattern = "evansdale",
                      x = {
                        {
                          city_column
                        }
                      },
                      ignore.case = TRUE
                    ),
                    "Black Hawk",
                    dplyr::if_else(
                      grepl(pattern = "(?:Buchan[ao]n)", {
                        {
                          county_column
                        }
                      }, ignore.case = TRUE),
                      "Buchanan",
                      dplyr::if_else(
                        grepl(
                          pattern = "iowa city",
                          x = {
                            {
                              city_column
                            }
                          },
                          ignore.case = TRUE
                        ),
                        "Johnson",
                        dplyr::if_else(
                          grepl(pattern = "(?:Clay[r]*ton)", {
                            {
                              county_column
                            }
                          }, ignore.case = TRUE),
                          "Clayton",
                          dplyr::if_else(
                            grepl(pattern = "(?:Del[ae]*ware)", {
                              {
                                county_column
                              }
                            }, ignore.case = TRUE),
                            "Delaware",
                            dplyr::if_else(
                              grepl(pattern = "(?:di[ck]*[a-z]+(osn|son|non))", {
                                {
                                  county_column
                                }
                              }, ignore.case = TRUE),
                              "Dickinson",
                              dplyr::if_else(
                                grepl(pattern = "(?:green[e]*)", {
                                  {
                                    county_column
                                  }
                                }, ignore.case = TRUE),
                                "Greene",
                                dplyr::if_else(
                                  grepl(
                                    pattern = "saylor",
                                    x = {
                                      {
                                        city_column
                                      }
                                    },
                                    ignore.case = TRUE
                                  ),
                                  "Polk",
                                  dplyr::if_else(
                                    grepl(pattern = "(?:^ia$)", {
                                      {
                                        county_column
                                      }
                                    }, ignore.case = TRUE),
                                    new_county,
                                    dplyr::if_else(
                                      grepl(pattern = "clinton", {
                                        {
                                          city_column
                                        }
                                      }, ignore.case = TRUE),
                                      "Clinton",
                                      dplyr::if_else(
                                        grepl(pattern = "(?:indianola)", {
                                          {
                                            city_column
                                          }
                                        }, ignore.case = TRUE),
                                        "Warren",
                                        dplyr::if_else(
                                          grepl(pattern = "(?:jo(n|h)(hson|hnson|nson|oson|son))", {
                                            {
                                              county_column
                                            }
                                          }, ignore.case = TRUE),
                                          "Johnson",
                                          dplyr::if_else(
                                            grepl(pattern = "(?:Kewaunee)", {
                                              {
                                                county_column
                                              }
                                            }, ignore.case = TRUE),
                                            new_county,
                                            dplyr::if_else(
                                              grepl(pattern = "(?:mar[r]*ion)", {
                                                {
                                                  county_column
                                                }
                                              }, ignore.case = TRUE),
                                              "Marion",
                                              dplyr::if_else(
                                                grepl(pattern = "(?:o[']*brien)", {
                                                  {
                                                    county_column
                                                  }
                                                }, ignore.case = TRUE),
                                                "O'Brien",
                                                dplyr::if_else(
                                                  grepl(pattern = "(?:wesley|Algona)", {
                                                    {
                                                      city_column
                                                    }
                                                  }, ignore.case = TRUE),
                                                  "Kossuth",
                                                  dplyr::if_else(
                                                    grepl(pattern = "(?:Iowa City)", {
                                                      {
                                                        city_column
                                                      }
                                                    }, ignore.case = TRUE),
                                                    "Johnson",
                                                    dplyr::if_else(
                                                      grepl(pattern = "(?:poc[h]*ahontas)", {
                                                        {
                                                          county_column
                                                        }
                                                      }, ignore.case = TRUE),
                                                      "Pocahontas",
                                                      dplyr::if_else(
                                                        grepl(pattern = "(?:altoona)", {
                                                          {
                                                            city_column
                                                          }
                                                        }, ignore.case = TRUE),
                                                        "Polk",
                                                        dplyr::if_else(
                                                          grepl(pattern = "(?:Council Bluffs)", {
                                                            {
                                                              city_column
                                                            }
                                                          }, ignore.case = TRUE),
                                                          "Pottawattamie",
                                                          dplyr::if_else(
                                                            grepl(pattern = "(?:Iowa Falls)", {
                                                              {
                                                                city_column
                                                              }
                                                            }, ignore.case = TRUE),
                                                            "Hardin",
                                                            dplyr::if_else(
                                                              grepl(pattern = "(?:des moines|urbandale|ankeny)", {
                                                                {
                                                                  city_column
                                                                }
                                                              }, ignore.case = TRUE),
                                                              "Polk",
                                                              dplyr::if_else(
                                                                grepl(pattern = "(?:van bur[r]*en)", {
                                                                  {
                                                                    county_column
                                                                  }
                                                                }, ignore.case = TRUE),
                                                                "Van Buren",
                                                                dplyr::if_else(
                                                                  grepl(pattern = "(?:war[nr]en)", {
                                                                    {
                                                                      county_column
                                                                    }
                                                                  }, ignore.case = TRUE),
                                                                  "Warren",
                                                                  dplyr::if_else(
                                                                    grepl(pattern = "(?:essex)", {
                                                                      {
                                                                        city_column
                                                                      }
                                                                    }, ignore.case = TRUE),
                                                                    "Page",
                                                                    dplyr::if_else(
                                                                      grepl(pattern = "(?:all[a]*makee)", {
                                                                        {
                                                                          county_column
                                                                        }
                                                                      }, ignore.case = TRUE),
                                                                      "Allamakee",
                                                                      dplyr::if_else(
                                                                        grepl(pattern = "(?:m[ao]nona)", {
                                                                          {
                                                                            county_column
                                                                          }
                                                                        }, ignore.case = TRUE),
                                                                        "Monona",
                                                                        dplyr::if_else(
                                                                          grepl(pattern = "(?:story)", {
                                                                            {
                                                                              county_column
                                                                            }
                                                                          }, ignore.case = TRUE),
                                                                          "Story",
                                                                          dplyr::if_else(
                                                                            grepl(pattern = "(?:[^a-z]+[0-9]+)", {
                                                                              {
                                                                                county_column
                                                                              }
                                                                            }, ignore.case = TRUE),
                                                                            new_county,
                                                                            dplyr::if_else(
                                                                              grepl(pattern = "(?:51012)", {
                                                                                {
                                                                                  zip_column
                                                                                }
                                                                              }, ignore.case = TRUE),
                                                                              "Cherokee",
                                                                              dplyr::if_else(
                                                                                grepl(pattern = "(?:mingo)", {
                                                                                  {
                                                                                    county_column
                                                                                  }
                                                                                }, ignore.case = TRUE),
                                                                                "Jasper",
                                                                                dplyr::if_else(
                                                                                  grepl(pattern = "(?:norwalk)", {
                                                                                    {
                                                                                      county_column
                                                                                    }
                                                                                  }, ignore.case = TRUE),
                                                                                  "Warren",
                                                                                  dplyr::if_else(
                                                                                    grepl(pattern = "(?:elkhart)", {
                                                                                      {
                                                                                        county_column
                                                                                      }
                                                                                    }, ignore.case = TRUE),
                                                                                    "Polk",
                                                                                    dplyr::if_else({
                                                                                      {
                                                                                        county_column
                                                                                      }
                                                                                    } == "County", new_county,
                                                                                    dplyr::if_else({
                                                                                      {
                                                                                        county_column
                                                                                      }
                                                                                    } == "Grant", "Montgomery",
                                                                                    dplyr::if_else({
                                                                                      {
                                                                                        county_column
                                                                                      }
                                                                                    } == "Burt", "Kossuth",
                                                                                    dplyr::if_else({
                                                                                      {
                                                                                        county_column
                                                                                      }
                                                                                    } == "Carlisle", "Warren",
                                                                                    dplyr::if_else({
                                                                                      {
                                                                                        zip_column
                                                                                      }
                                                                                    } == "52358", "Cedar",
                                                                                    dplyr::if_else({
                                                                                      {
                                                                                        county_column
                                                                                      }
                                                                                    } == "Fulton", "Jackson", {
                                                                                      {
                                                                                        county_column
                                                                                      }
                                                                                    }
                                                                                    ))))))
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ))

    return(clean_counties)

  }

# Function to Clean County Names (Part 2)
# This function corrects county names in a data frame based on known
# misspellings and ZIP code associations. It is a continuation of a previous
# cleaning function due to `if_else()` limitations with excessive nesting.
clean_county_names_2 <-
  function(df,
           county_column,
           zip_column) {

    # Validate that the input is a data frame or tibble
    if (!is.data.frame(df) && !tibble::is_tibble(df)) {

      # Abort execution with an error if `df` is not a data frame
      cli::cli_abort(
        "The first argument `df` of the input was of class {.cls {class(df)}} but must be a {.cls data.frame}.
        Please supply a {.cls data.frame} to the argument {.var df}."
      )

    }

    # Perform county name corrections based on common misspellings and ZIP codes
    clean_counties <- df |>
      dplyr::mutate({{ county_column }} :=
                      dplyr::case_when(
                        # Correct common misspellings of "Harrison"
                        grepl(pattern = "harrision|harison", {{ county_column }}, ignore.case = TRUE) ~ "Harrison",
                        # Assign "Warren" county based on ZIP code 50125
                        grepl(pattern = "50125", {{ zip_column }}, ignore.case = TRUE) ~ "Warren",
                        # Assign "Harrison" county based on ZIP code 51546
                        grepl(pattern = "51546", {{ zip_column }}, ignore.case = TRUE) ~ "Harrison",
                        # Retain original county name if no match is found
                        TRUE ~ {{ county_column }}
                      )
      )

    # Return the cleaned data frame with corrected county names
    return(clean_counties)
  }

# get location data
# Iowa county data
county_data <- readxl::read_excel(path = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/Reference-Files/IA Counties, Regions.xlsx")

# essential service counties
essential_counties <- county_data |>
  dplyr::filter(`EMS Essential Service` == TRUE) |>
  dplyr::pull(County)

# helper object for manipulations
location <-
  readxl::read_excel(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/Reference-Files/IA Counties, Regions.xlsx"
  ) |>
  dplyr::select(County,
                `Region: Preparedness`,
                Pop:Designation,
                `EMS Essential Service`) |>
  dplyr::mutate(`EMS Essential Service` = dplyr::if_else(County %in% essential_counties, TRUE, FALSE))

# zipcode level data
zipcodes <- zipcodeR::zip_code_db |>
  dplyr::mutate(county = stringr::str_remove_all(county, pattern = "(?:\\sCounty)")) |>
  dplyr::select(major_city, state, county, zipcode, lat, lng) |>
  dplyr::mutate(county = dplyr::if_else(state == "IA" &
                                          county == "", "Polk", county)) |>
  dplyr::rename("new_city" = "major_city") |>
  dplyr::rename("new_state" = "state") |>
  dplyr::rename("new_county" = "county") |>
  dplyr::rename("new_zipcode" = "zipcode")

### DATA IMPORT FACILITIES =====================================================

# Import NEMSQA Data from a CSV File
# This function imports data for a specified National EMS Quality Alliance
# (NEMSQA) table and year, reading the corresponding CSV file into R. If no file
# location is provided, it defaults to a predefined directory.
import_nemsqa_data <- function(location = NULL, table, year) {

  # If no file location is provided, set the default directory.
  if (is.null(location)) {
    location <- file.path(
      "C:/Users/nfoss0/OneDrive - State of Iowa HHS",
      "Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/tables/"
    )
  }

  # Construct the full file path using the expected filename format.
  # Example: "C:/path/to/location/nemsqa_example_table_data_Export_2024.csv"
  final_path <- glue::glue("{location}/nemsqa_{table}_data_Export_{year}.csv")

  # Read the CSV file into a tibble and return the result.
  readr::read_csv(file = final_path)
}

# Import and Load NEMSQA Statistical Files into the Global Environment
# This function imports all CSV statistical files related to a specified
# National EMS Quality Alliance (NEMSQA) measure from a directory structure. It
# reads files from subdirectories within the measure folder, stores them in a
# list, and loads them into the global environment as named objects.
import_nemsqa_statistical_files <- function(location = NULL, measure) {

  # Create a temporary environment to manage variable assignment.
  temp_env <- new.env()

  with(temp_env, {

    # If no file location is provided, use the default directory.
    if (is.null(location)) {
      location <- file.path(
        "C:/Users/nfoss0/OneDrive - State of Iowa HHS",
        "Analytics/BEMTS/NEMSQA Report/2025/output/"
      )
    }

    # Initialize an empty list to store the imported data.
    file_list <- list()

    # Construct the full path to the measure directory.
    final_path <- glue::glue("{location}/{measure}")

    # Iterate through all subdirectories (folders) in the measure directory.
    for (folder in list.files(path = final_path)) {

      # Construct the full path to the current subdirectory.
      working_path <- glue::glue("{final_path}/{folder}")

      # Iterate through all CSV files in the current subdirectory.
      for (file in list.files(path = working_path, pattern = "\\.csv$", full.names = TRUE)) {

        # Extract the file name (without the .csv extension) for object naming.
        file_name <- stringr::str_remove(basename(file), pattern = "\\.csv$")

        # Read the CSV file and store it in the file_list with its name as the key.
        file_list[[file_name]] <- readr::read_csv(file)

      }
    }

    # Load the named data frames into the global environment.
    list2env(file_list, envir = .GlobalEnv)

  }) # End of with(temp_env)

}

### DATA CLEANING FACILITIES ===================================================
# Clean Column Names and Standardize Date Fields in EMS Data
# This function performs common data cleaning tasks, including renaming columns
# to SCREAMING_SNAKE_CASE, standardizing date and datetime fields, generating
# unique identifiers, and deriving additional time-related variables.
clean_names_dates_data <- function(df) {

  cleaned_df <- df |>

    # Standardize column names to SCREAMING_SNAKE_CASE
    janitor::clean_names(case = "screaming_snake", sep_out = "_") |>

    # Convert date and datetime fields
    dplyr::mutate(
      # Convert date fields (excluding datetime fields)
      dplyr::across(matches("date(?!.*time)", perl = TRUE),
                    ~ lubridate::mdy(
                      stringr::str_remove_all(., pattern = "\\s\\d+:\\d+(?::\\d+)?\\s[AP]M$")
                    )
      ),

      # Convert datetime fields
      dplyr::across(matches("date(?=.*time)", perl = TRUE),
                    ~ lubridate::mdy_hms(
                      stringr::str_remove_all(., pattern = "\\s[AP]M$")
                    )
      ),

      # Create a unique ePCR number by concatenating PCR number with either datetime or date
      UNIQUE_EPCR_NUMBER = dplyr::if_else(
        !is.na(INCIDENT_DATE_TIME),
        stringr::str_c(INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01, INCIDENT_DATE_TIME),
        stringr::str_c(INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01, INCIDENT_DATE)
      ),

      # Create a unique run ID by concatenating agency number, PCR number, and either datetime or date
      UNIQUE_RUN_ID = dplyr::if_else(
        !is.na(INCIDENT_DATE_TIME),
        stringr::str_c(AGENCY_NUMBER_D_AGENCY_02,
                       INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                       INCIDENT_DATE_TIME),
        stringr::str_c(AGENCY_NUMBER_D_AGENCY_02,
                       INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                       INCIDENT_DATE)
      )
    ) |>

    # Add derived time-related variables
    dplyr::mutate(
      INCIDENT_YEAR = lubridate::year(INCIDENT_DATE),
      INCIDENT_CY_QUARTER = lubridate::quarter(INCIDENT_DATE),
      INCIDENT_MONTH = lubridate::month(INCIDENT_DATE, label = FALSE),
      INCIDENT_DAY = weekdays(INCIDENT_DATE, abbreviate = FALSE),
      INCIDENT_WEEK_PART = traumar::weekend(INCIDENT_DATE),
      INCIDENT_SEASON = traumar::season(INCIDENT_DATE),
      .before = INCIDENT_DATE
    ) |>

    # Filter out demo services and ensure agency numbers meet format requirements
    dplyr::filter(
      AGENCY_IS_DEMO_SERVICE == FALSE, # Exclude demo services
      stringr::str_sub(AGENCY_NUMBER_D_AGENCY_02, 1, 1) %in% c("2", "8", "9"), # Keep valid agency prefixes
      nchar(AGENCY_NUMBER_D_AGENCY_02) == 7 # Ensure agency number is exactly 7 characters long
    )

  return(cleaned_df)

}

### DATA VISUALIZATION FACILITIES ==============================================
# Plot Population Trends from NEMSQA Population Data
# This function creates a column or line chart visualizing population trends
# across multiple years from the outputs of `*_population` functions in the
# `nemsqar` package.
plot_nemsqa_pops <- function(df, wrap_width = 50, type = c("col", "line"),
                             plot_title, ...) {

  # Add a helper variable to adjust text label positioning
  df <- df |>
    dplyr::mutate(nudge_var = dplyr::if_else(count > 10, -1, -count * 0.1))

  # Ensure `type` is a single valid choice
  type <- match.arg(type, choices = c("col", "line"))

  # Generate the base plot according to the selected type
  temp_plot <- if (type == "col") {

    # Column chart: Displays count data grouped by YEAR
    ggplot2::ggplot(df, ggplot2::aes(x = YEAR, y = count, fill = factor(YEAR))) +
      ggplot2::geom_col(alpha = 0.5, position = ggplot2::position_dodge())

  } else {

    # Line chart: Shows trends across years with a connecting line
    ggplot2::ggplot(df, ggplot2::aes(x = YEAR, y = count, color = "lightgray")) +
      ggplot2::geom_line(alpha = 0.5, linewidth = 1.5, lineend = "round", linejoin = "round")

  }

  # Finalize the plot with additional aesthetics and labels
  plot_pops <- temp_plot +
    ggplot2::geom_text(
      ggplot2::aes(y = count + nudge_var,
                   label = traumar::pretty_number(count, n_decimal = 2)),
      size = 4, color = "darkslategray", fontface = "bold", family = "sans"
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_number(x, n_decimal = 2, truncate = TRUE)
    ) +
    ggplot2::guides(fill = "none", color = "none") +
    ggplot2::facet_wrap(
      ~ stringr::str_wrap(filter, width = wrap_width), scales = "free_y"
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = glue::glue("{plot_title} Population Trends"),
      subtitle = "Source: ImageTrend Elite EMS Registry | CY 2021-2024"
    ) +
    traumar::theme_cleaner(...)

  return(plot_pops)

}

# Apply HHS Styling to a {gt} Table
# This function applies a standardized Health & Human Services (HHS) style theme
# to a `{gt}` table, enhancing readability and ensuring a professional,
# consistent look for reports.

# The function modifies several aspects of the `{gt}` table:
# - Row Groups: Custom styling for row group text and background fill.
# - Column Labels & Spanners: Adjusted font size, color, and alignment.
# - Table Body: Formats text with different alignments and font styles.
# - Borders: Adds top borders to row groups and left borders to selected
#   columns.
# - Source Notes: Includes `{fontawesome}` icons and relevant metadata.
tab_style_hhs <- function(gt_object, table_title, table_subtitle, row_groups = 14, column_labels = 14,
                          title = 20, subtitle = 18, spanners = 16, body = 14,
                          source_note = 12, footnote = 12, message_text,
                          row_group_fill = "#E0A624", row_group_fill_alpha = 0.5,
                          bold_first_col = 1, border_cols,
                          border_color1 = "#19405B", border_color2 = "#70C8B8") {

  out <- gt_object |>

    # Set the font for the table
    opt_table_font(
      font = "Work Sans",
      stack = NULL,
      weight = NULL,
      style = NULL,
      add = TRUE
    ) |>

    # Add title and subtitle
    tab_header(title = gt::md(paste0(
      fontawesome::fa("truck-medical"),
      glue::glue(" **{table_title}**")
    )),
    subtitle = gt::md(
      glue::glue("{table_subtitle}")
    )) |>

    # Style the stub (row names) section
    gt::tab_style(
      locations = gt::cells_stub(),
      style = gt::cell_text(size = gt::px(body),
                            font = "Work Sans SemiBold",
                            color = "black",
                            align = "left")
    ) |>

    # Style the row groups
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(row_groups),
        font = "Work Sans SemiBold",
        color = "#03617A",
        align = "left"
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    )  |>

    # Apply background color to row groups
    gt::tab_style(
      style = gt::cell_fill(color = row_group_fill, alpha = row_group_fill_alpha),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Add top border to row groups
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = border_color1,
        weight = gt::px(3)  # Adjust thickness as needed
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Style column labels
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(column_labels),
        font = "Work Sans SemiBold",
        color = "#03617A",
        align = "center",
        style = "italic"
      ),
      locations = gt::cells_column_labels(gt::everything())
    ) |>

    # Style the table title
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans ExtraBold",
        color = "#19405B",
        size = gt::px(title)
      ),
      locations = gt::cells_title(groups = "title")
    )  |>

    # Style the table subtitle
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "#70C8B8",
        size = gt::px(subtitle)
      ),
      locations = gt::cells_title(groups = "subtitle")
    ) |>

    # Style the spanner labels (column headers spanning multiple columns)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "#03617A",
        size = gt::px(spanners),
        align = "center"
      ),
      locations = gt::cells_column_spanners()
    )  |>

    # Style the first column (typically used for labels)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "black",
        size = gt::px(body),
        align = "left"
      ),
      locations = gt::cells_body(columns = {{bold_first_col}})
    ) |>

    # Style all other body cells (except first column)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans",
        color = "black",
        size = gt::px(body),
        align = "center"
      ),
      locations = gt::cells_body(columns = -1)
    )  |>

    # Style row names (stub)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans Black",
        size = gt::px(body),
        color = "black",
        align = "left"
      ),
      locations = gt::cells_stub(rows = gt::everything())
    ) |>

    # Style footnotes text
    gt::tab_style(
      style = gt::cell_text(
        weight = "normal",
        font = "Work Sans",
        size = gt::px(footnote),
        color = "#19405B"
      ),
      locations = gt::cells_source_notes()
    ) |>

    # Style source note text
    gt::tab_style(
      style = gt::cell_text(
        weight = "normal",
        font = "Work Sans",
        size = gt::px(source_note),
        color = "#19405B"
      ),
      locations = gt::cells_footnotes()
    ) |>

    # Add a left-side border to specified columns
    gt::tab_style(
      locations = gt::cells_body(columns = {{border_cols}}),
      style = gt::cell_borders(
        sides = c("left"),
        weight = gt::px(2),
        color = border_color2
      )
    ) |>

    # Add various source notes with icons from fontawesome
    gt::tab_source_note(source_note = gt::md(paste0(
      fontawesome::fa("note-sticky"),
      " ",
      message_text
    ))) |>

    gt::tab_source_note(source_note = gt::md(
      paste0(
        fontawesome::fa("database"),
        " Iowa ImageTrend Elite EMS Registry"
      )
    )) |>

    gt::tab_source_note(source_note = gt::md(
      paste0(
        fontawesome::fa("house-medical"),
        " Bureau of Emergency Medical and Trauma Services"
      )
    )) |>

    gt::tab_source_note(source_note = gt::md(paste0(
      fontawesome::fa("building-shield"),
      " Division of Public Health"
    ))) |>

    gt::tab_source_note(source_note = gt::md(
      paste0(
        fontawesome::fa("code"),
        " Dr. Nicolas Foss, MS || Epidemiologist"
      )
    )) |>

    # Align all columns except the first one to the center
    gt::cols_align(align = "center",
                   columns = 2)

  return(out)
}

### DATA EXPORT FACILITY =======================================================

# Export NEMSQA Data to CSV
# This function exports objects from the global environment that match a
# specified pattern. It ensures that only data frames or tibbles are exported
# and organizes them into predefined output folders. Non-data-frame objects are
# skipped with a warning.
export_nemsqa_data <- function(pattern, measure, folder = c("population", "result")) {

  # Validate folder selection
  folder <- match.arg(folder, choices = c("population", "result"))

  # Construct the output directory path
  output_path <- glue::glue("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/NEMSQA Report/2025/output/{measure}/{folder}")

  # Ensure the output directory exists
  fs::dir_create(output_path)

  # Find objects in the global environment matching the pattern
  objects <- ls(pattern = pattern, envir = .GlobalEnv)

  if (length(objects) == 0) {
    cli::cli_warn("No objects found matching pattern: {pattern}")
    return(invisible(NULL))
  }

  # Initial count objects for dynamic assignment
  exported_count <- 0
  skipped_count <- 0

  # Report header
  cli::cli_h1("NEMSQA Exports for {measure} {folder}s")
  cli::cli_text("\n")  # Add space for readability

  # Iterate through matched objects
  for (i in objects) {
    data <- get(i, envir = .GlobalEnv)  # Retrieve object from global environment

    if (is.data.frame(data)) {
      file_path <- glue::glue("{output_path}/{i}.csv")
      readr::write_csv(x = data, file = file_path)
      cli::cli_inform(c("v" = "Exported: {file_path}"))
      exported_count <- exported_count + 1
    } else {
      cli::cli_warn("Skipped {i}: Not a data frame")
      skipped_count <- skipped_count + 1
    }
  }

  # Final summary report
  cli::cli_h2("{measure} {folder}s Export Summary")
  cli::cli_alert_success("Total objects matched: {length(objects)}")
  cli::cli_alert_success("Total successfully exported: {exported_count}")
  cli::cli_alert_warning("Total skipped (not data frames): {skipped_count}")

  cli::cli_text("\n")  # Add space before warnings

  return(invisible(NULL))
}

###_____________________________________________________________________________
# we will:
# - examine calendar years 2021-2024 of EMS data
# - ingest various tables from each data section of NEMSIS to leverage that
#   approach in the `nemsqar` package
# - visualize statistical outputs for clean reporting
###_____________________________________________________________________________
