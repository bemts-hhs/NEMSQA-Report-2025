### IOWA NEMSQAR REPORT PREP 2025 ----------------------------------------------

# This script prepares for the analyses using the `nemsqar` package v1.1.0

### PACKAGES -------------------------------------------------------------------

# CRAN versions ================================================================

# install these packages if not already

# install.packages(c("tidyverse", "traumar", "devtools", "remotes", "janitor",
#                    "gt", "gtsummary", "gtExtras", "zipcodeR", "naniar",
#                    "ggrepel", "devtools", "renv", "roxygen2",
#                    "roxygen2md", "nemsqar"
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

# Handy Functions ==============================================================

###_____________________________________________________________________________
# After observing the different problems with Iowa counties, we can
# clean these county names so they are uniform and spelled correctly using
# regex within a custom map() function
# add nature of injury data
###_____________________________________________________________________________

# clean county names, second part of the ems data analysis workflow
clean_county_names_1 <-
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

# clean county names, still the second part but have to start a new one
# due to if_else() breaking after so many nested statements
clean_county_names_2 <-
  function(df,
           county_column,
           zip_column) {
    # let x be a named column within a data.frame


    if (!is.data.frame(df) && !is_tibble(df)) {

      cli::cli_abort(
        "The first argument `df` of the input was of class {.cls {class(df)}}but must be a {.cls data.frame}.  Please supply a {.cls data.frame} to the argument {.var df}."
      )

    }

    clean_counties <- df |>
      dplyr::mutate({{ county_column }} :=
                      dplyr::case_when(
                        grepl(pattern = "harrision|harison", {{ county_column }}, ignore.case = TRUE) ~ "Harrison",
                        grepl(pattern = "50125", {{ zip_column }}, ignore.case = TRUE) ~ "Warren",
                        grepl(pattern = "51546", {{ zip_column }}, ignore.case = TRUE) ~ "Harrison",
                        TRUE ~ {{ county_column }}
                      )
      )

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

# files have common text in the names to make import and management easier
# function to make import streamlined and easy
import_nemsqa_data <- function(location = NULL, table, year) {

  if(is.null(location)) {

    location <- file.path("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/tables/")

  }

  final_path <- glue::glue("{location}/nemsqa_{table}_data_Export_{year}.csv")

  readr::read_csv(file = final_path)

}

# a function to make common cleaning tasks more straightforward
clean_names_dates_data <- function(df) {

  cleaned_df <- df |>
    janitor::clean_names(case = "screaming_snake", sep_out = "_") |>
    dplyr::mutate(

      dplyr::across(matches("date(?!.*time)", perl = TRUE), ~ lubridate::mdy(
        stringr::str_remove_all(string = ., pattern = "\\s\\d+:\\d+(?::\\d+)?\\s[AP]M$")
      )),

      dplyr::across(matches("date(?=.*time)", perl = TRUE), ~ lubridate::mdy_hms(
        stringr::str_remove_all(string = ., pattern = "\\s[AP]M$")
      )),
      UNIQUE_EPCR_NUMBER = dplyr::if_else(!is.na(INCIDENT_DATE_TIME),
                                          stringr::str_c(INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                          INCIDENT_DATE_TIME
                                          ),
                                          stringr::str_c(INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                          INCIDENT_DATE
                                          )),
      UNIQUE_RUN_ID = dplyr::if_else(!is.na(INCIDENT_DATE_TIME),
                                     stringr::str_c(AGENCY_NUMBER_D_AGENCY_02,
                                     INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                     INCIDENT_DATE_TIME),
                                     stringr::str_c(AGENCY_NUMBER_D_AGENCY_02,
                                     INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                     INCIDENT_DATE)
                                     )
    ) |>
    dplyr::mutate(
      INCIDENT_YEAR = lubridate::year(INCIDENT_DATE),
      INCIDENT_CY_QUARTER = lubridate::quarter(INCIDENT_DATE),
      INCIDENT_MONTH = lubridate::month(INCIDENT_DATE, label = FALSE),
      INCIDENT_DAY = weekdays(INCIDENT_DATE, abbreviate = FALSE),
      INCIDENT_WEEK_PART = traumar::weekend(INCIDENT_DATE),
      INCIDENT_SEASON = traumar::season(INCIDENT_DATE),
      .before = INCIDENT_DATE
      ) |>
    dplyr::filter(
      AGENCY_IS_DEMO_SERVICE == FALSE,
      stringr::str_sub(AGENCY_NUMBER_D_AGENCY_02, 1, 1) %in% c("2", "8", "9"),
      nchar(AGENCY_NUMBER_D_AGENCY_02) == 7
    )

  return(cleaned_df)

}

# a plotting function for the analytical script
plot_nemsqa_pops <- function(df, wrap_width = 50, type = c("col", "line"),
                             plot_title, ...
                             ) {

  # set up the df with a helper variable for
  # nudge_y the geom_text_repel labels

  df <- df |>
    dplyr::mutate(nudge_var = dplyr::if_else(count > 10, -1, -count * 0.1))

  # the filter_process object returned by all *_population functions
  # has the same structure across all nemsqar *_population functions
  # as such, this function will help plot the progression of counts across
  # an object that includes filter_process data from multiple years

  if(length(type) > 1) { # default to column chart

    type <- "col"

  }

  if(type == "col") { # column chart option

  temp_plot <- df |>
    ggplot2::ggplot(ggplot2::aes(x = YEAR, y = count, fill = factor(YEAR))) +
    ggplot2::geom_col(alpha = 0.5, position = ggplot2::position_dodge())

  } else if(type == "line") { # line chart

    temp_plot <- df |>
      ggplot2::ggplot(ggplot2::aes(x = YEAR, y = count, color = "lightgray")) +
      ggplot2::geom_line(alpha = 0.5, linewidth = 1.5, lineend = "round", linejoin = "round")

  }

  # finish off the plot with final adjustments to graphics parameters
  plot_pops <- temp_plot +
    ggplot2::geom_text(aes(y = count - count * 0.1, label = traumar::pretty_number(count, n_decimal = 2)),
                             size = 4,
                             color = "darkslategray",
                             fontface = "bold",
                             font = "sans"
                             ) +
    ggplot2::scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 2, truncate = TRUE)) +
    ggplot2::guides(fill = "none", color = "none") +
    ggplot2::facet_wrap(~ stringr::str_wrap(filter, width = wrap_width), scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL,
                  title = glue::glue("{plot_title} Population Trends"), subtitle = "Source: ImageTrend Elite EMS Registry | CY 2021-2024"
                  ) +
    traumar::theme_cleaner(...)

  return(plot_pops)

}

### DATA EXPORT FACILITY =======================================================

# outputs from all scripts have common naming conventions
# utilize a for loop to export applicable files
# skip files that are not a data.frame/tibble
# export to location outside the GitHub repo to protect confidentiality

export_nemsqa_data <- function(pattern, measure, folder = c("population", "result")) {

  folder <- match.arg(folder, choices = c("population", "result"))
  output_path <- glue::glue("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/NEMSQA Report/2025/output/{measure}/{folder}")

  # Ensure output directory exists
  fs::dir_create(output_path)

  # Explicitly search for objects in .GlobalEnv
  objects <- ls(pattern = pattern, envir = .GlobalEnv)

  if (length(objects) == 0) {
    cli::cli_warn("No objects found matching pattern: {pattern}")
    return(invisible(NULL))
  }

  exported_count <- 0
  skipped_count <- 0

  # report header
  cli::cli_h1("NEMSQA Exports for {measure} {folder}s")

  # make a space
  cli::cli_text("\n")

  for (i in objects) {
    data <- get(i, envir = .GlobalEnv)  # Retrieve from .GlobalEnv

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

  # make another space before warnings
  cli::cli_text("\n")

}

###_____________________________________________________________________________
# we will examine calendar years 2021-2024 of EMS data
# we will ingest various tables from each data section of
# NEMSIS to leverage that approach in the `nemsqar` package
###_____________________________________________________________________________
