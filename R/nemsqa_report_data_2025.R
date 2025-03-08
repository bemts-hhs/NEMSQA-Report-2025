### IOWA NEMSQAR REPORT DATA 2025 ----------------------------------------------

# This script prepares for the analyses using the `nemsqar` package in the

### PACKAGES -------------------------------------------------------------------

# CRAN versions ================================================================
# install.packages(c("tidyverse", "traumar", "devtools", "remotes", "janitor",
#                    "gt", "gtsummary", "gtExtras", "zipcodeR", "naniar",
#                    "ggrepel", "nemsqar", "binom"
#                   ))

# CRAN version of `nemsqar` =====================================================
# install.packages("nemsqar")

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
library(binom)

# Handy Functions ==============================================================

###_____________________________________________________________________________
# After observing the different problems with Iowa counties, we can
# clean these county names so they are uniform and spelled correctly using
# regex within a custom map() function
# add nature of injury data
###_____________________________________________________________________________

# test confidence interval function for the nemsqar package
nemsqa_binomial_confint <- function(data = NULL, x, n,
                           method = c("wilson", "clopper-pearson"),
                           conf.level = 0.95,
                           correct = TRUE) {

  # Set default method and adjustment method
  method <- match.arg(method)

  # If the user passes a tibble or data.frame
  if (!is.null(data)) {
    x <- data |> pull({{x}})
    n <- data |> pull({{n}})
  }

  # Initialize lower and upper CI bounds
  lower <- numeric()
  upper <- numeric()

  # Initialize the calculated proportion
  estimate <- numeric()

  # Vectorized Wilson Interval
  # Based on Wilson, E. B. (1927)
  if (method == "wilson") {

    # Create a vectorized version of the function for computing confidence intervals
    # for each pair of (x, n) values using prop.test().
    # Vectorize() makes the function work element-wise over vectors of x and n
    # Define an anonymous function here
    ci <- Vectorize(function(x, n) {

      if (n == 0) {
        return(c(NaN, NaN))  # Return NaN if n == 0
      }

      # Calculate the confidence interval for the proportion using the Wilson method
      # calculate the estimate (proportion) as well
      result <- prop.test(x, n, correct = correct, conf.level = conf.level)

      # Return CI bounds and the estimate
      c(result$conf.int, result$estimate)

    }, vectorize.args = c("x", "n"))  # Specify the arguments to be vectorized

    # Call the vectorized function on the x and n values
    ci_result <- ci(x, n)  # Apply the vectorized function to the vectors of x and n

    # Extract the lower confidence interval (CI) values from the result matrix
    lower <- ci_result[1,]  # First row contains lower CIs

    # Extract the upper confidence interval (CI) values from the result matrix
    upper <- ci_result[2,]  # Second row contains upper CIs

    # Extract the estimate from the result matrix
    estimate <- ci_result[3,] # Third row contains the estimates

    }

  # Vectorized Clopper-Pearson Interval
  # Based on Clopper, C. & Pearson, E. S. (1934)
  if (method == "clopper-pearson") {

    # Create a vectorized version of the function for computing confidence intervals
    # for each pair of (x, n) values using binom.test().
    # Vectorize() makes the function work element-wise over vectors of x and n
    # Define an anonymous function here
    ci <- Vectorize(function(x, n) {

      if (n == 0) {
        return(c(NaN, NaN))  # Return NaN if n == 0
      }

      # Calculate the confidence interval for the proportion using the Clopper-Pearson method
      # calculate the estimate (proportion) as well
      result <- binom.test(x, n, conf.level = conf.level)

      # Return CI bounds and the estimate
      c(result$conf.int, result$estimate)

    }, vectorize.args = c("x", "n"))  # Specify the arguments to be vectorized

    # Call the vectorized function on the x and n values
    ci_result <- ci(x, n)  # Apply the vectorized function to the vectors of x and n

    # Extract the lower confidence interval (CI) values from the result matrix
    lower <- ci_result[1,]  # First row contains lower CIs

    # Extract the upper confidence interval (CI) values from the result matrix
    upper <- ci_result[2,]  # Second row contains upper CIs

    # Extract the estimate from the result matrix
    estimate <- ci_result[3,] # Third row contains the estimates

    }

  # Return as a dataframe/tibble-compatible structure
  lower_upper <- tibble::tibble(prop = estimate, lower_ci = lower, upper_ci = upper) |>
    dplyr::mutate(prop_label = pretty_percent(prop),
                  .after = prop
                  )

  if (!is.null(data)) {

    return(dplyr::bind_cols(data, lower_upper))

  } else {

    return(lower_upper)

  }

}

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

### DATA =======================================================================

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

###_____________________________________________________________________________
# we will examine calendar years 2021-2024 of EMS data
# we will ingest various tables from each data section of
# NEMSIS to leverage that approach in the `nemsqar` package
###_____________________________________________________________________________

### airway tables ################################################################
airway_2021 <- import_nemsqa_data(table = "airway", year = 2021)
airway_2022 <- import_nemsqa_data(table = "airway", year = 2022)
airway_2023 <- import_nemsqa_data(table = "airway", year = 2023)
airway_2024 <- import_nemsqa_data(table = "airway", year = 2024)

# bind rows for the airway table
airway_rbind <- dplyr::bind_rows(airway_2021,
                                 airway_2022,
                                 airway_2023,
                                 airway_2024
                                 )

# set up airway table for manipulations
airway_table <- airway_rbind |>
  clean_names_dates_data()

### arrest tables ################################################################
arrest_2021 <- import_nemsqa_data(table = "arrest", year = 2021)
arrest_2022 <- import_nemsqa_data(table = "arrest", year = 2022)
arrest_2023 <- import_nemsqa_data(table = "arrest", year = 2023)
arrest_2024 <- import_nemsqa_data(table = "arrest", year = 2024)

# bind rows for the arrest table
arrest_rbind <- dplyr::bind_rows(arrest_2021,
                                 arrest_2022,
                                 arrest_2023,
                                 arrest_2024
                                 )

# set up arrest table for manipulations
arrest_table <- arrest_rbind |>
  clean_names_dates_data()

### disposition tables ###########################################################
disposition_2021 <- import_nemsqa_data(table = "disposition", year = 2021)
disposition_2022 <- import_nemsqa_data(table = "disposition", year = 2022)
disposition_2023 <- import_nemsqa_data(table = "disposition", year = 2023)
disposition_2024 <- import_nemsqa_data(table = "disposition", year = 2024)

# bind rows for the disposition table
disposition_rbind <- dplyr::bind_rows(disposition_2021,
                                      disposition_2022,
                                      disposition_2023,
                                      disposition_2024
                                      )

# set up the disposition table for manipulations
disposition_table <- disposition_rbind |>
  clean_names_dates_data()

### exam tables ##################################################################

###_____________________________________________________________________________
# handle the exam tables differently due to size
# workflow will change from going through the dplyr::bind_rows() set to
# manipulations to manipulations before dplyr::bind_rows()
# import and clean each file
# break up the 2024 file into its 2 month parts (6) and clean each
# then bind all together at the end
###_____________________________________________________________________________

# 2021
exam_2021 <- import_nemsqa_data(table = "exam", year = 2021)

exam_2021_clean <- exam_2021 |>
  clean_names_dates_data()

# 2022
exam_2022 <- import_nemsqa_data(table = "exam", year = 2022)

exam_2022_clean <- exam_2022 |>
  clean_names_dates_data()

# 2023
exam_2023 <- import_nemsqa_data(table = "exam", year = 2023)

exam_2023_clean <- exam_2023 |>
  clean_names_dates_data()

# 2024 is broken up into several tables due to its size, ~ 40m rows

# 2024 jan-feb
exam_2024_1 <- import_nemsqa_data(table = "exam", year = "2024_1")

exam_2024_1_clean <- exam_2024_1 |>
  clean_names_dates_data()

# 2024 mar-apr
exam_2024_2 <- import_nemsqa_data(table = "exam", year = "2024_2")

exam_2024_2_clean <- exam_2024_2 |>
  clean_names_dates_data()

# 2024 may-june
exam_2024_3 <- import_nemsqa_data(table = "exam", year = "2024_3")

exam_2024_3_clean <- exam_2024_3 |>
  clean_names_dates_data()

# 2024 july-aug
exam_2024_4 <- import_nemsqa_data(table = "exam", year = "2024_4")

exam_2024_4_clean <- exam_2024_4 |>
  clean_names_dates_data()

# 2024 sept-oct
exam_2024_5 <- import_nemsqa_data(table = "exam", year = "2024_5")

exam_2024_5_clean <- exam_2024_5 |>
  clean_names_dates_data()

# 2024 nov-dec
exam_2024_6 <- import_nemsqa_data(table = "exam", year = "2024_6")

exam_2024_6_clean <- exam_2024_6 |>
  clean_names_dates_data()

# bind rows for the exam table
exam_table <- dplyr::bind_rows(exam_2021_clean,
                               exam_2022_clean,
                               exam_2023_clean,
                               exam_2024_1_clean,
                               exam_2024_2_clean,
                               exam_2024_3_clean,
                               exam_2024_4_clean,
                               exam_2024_5_clean,
                               exam_2024_6_clean
                               )

### injury tables ################################################################
injury_2021 <- import_nemsqa_data(table = "injury", year = 2021)
injury_2022 <- import_nemsqa_data(table = "injury", year = 2022)
injury_2023 <- import_nemsqa_data(table = "injury", year = 2023)
injury_2024 <- import_nemsqa_data(table = "injury", year = 2024)

# bind rows for the injury table
injury_rbind <- dplyr::bind_rows(injury_2021,
                                 injury_2022,
                                 injury_2023,
                                 injury_2024
                                 )

# set up the injury table for manipulations
injury_table <- injury_rbind |>
  clean_names_dates_data()

### medications tables ###########################################################
medications_2021 <- import_nemsqa_data(table = "medications", year = 2021)
medications_2022 <- import_nemsqa_data(table = "medications", year = 2022)
medications_2023 <- import_nemsqa_data(table = "medications", year = 2023)
medications_2024 <- import_nemsqa_data(table = "medications", year = 2024)

# bind rows for the medications table
medications_rbind <- dplyr::bind_rows(medications_2021,
                                      medications_2022,
                                      medications_2023,
                                      medications_2024
                                      )

# set up the medications table for manipulations
medications_table <- medications_rbind |>
  clean_names_dates_data()

### patient/scene tables #########################################################
# given that patient and scene data are 1-1 relationship, join those tables
patient_scene_2021 <- import_nemsqa_data(table = "patient_scene", year = 2021)
patient_scene_2022 <- import_nemsqa_data(table = "patient_scene", year = 2022)
patient_scene_2023 <- import_nemsqa_data(table = "patient_scene", year = 2023)
patient_scene_2024 <- import_nemsqa_data(table = "patient_scene", year = 2024)

# bind rows for the patient/scene table
patient_scene_rbind <- dplyr::bind_rows(patient_scene_2021,
                                        patient_scene_2022,
                                        patient_scene_2023,
                                        patient_scene_2024
                                        )

# set up patient/scene table for manipulations
patient_scene_clean <- patient_scene_rbind |>
  clean_names_dates_data()

# final manipulations on the patient/scene table
# handle multiple issues with location using external data sources with
# consistent names
patient_scene_table <- patient_scene_clean |>
  dplyr::left_join(zipcodes, by = c("SCENE_INCIDENT_POSTAL_CODE_E_SCENE_19" = "new_zipcode")) |>
  dplyr::left_join(location, by = c("SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21" = "County")) |>
  dplyr::relocate(new_county, .after = SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21) |>
  dplyr::relocate(new_state, .after = SCENE_INCIDENT_STATE_NAME_E_SCENE_18) |>
  dplyr::relocate(`Region: Preparedness`, .after = new_county) |>
  dplyr::relocate(Pop, .after = `Region: Preparedness`) |>
  dplyr::relocate(State, .after = SCENE_INCIDENT_STATE_NAME_E_SCENE_18) |>
  dplyr::relocate(Country, .after = new_state) |>
  dplyr::relocate(Designation, .after = new_county) |>
  dplyr::mutate(
    SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21 = dplyr::if_else(
      is.na(SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21) &
        !is.na(new_county),
      new_county,
      SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21
    ),
    SCENE_INCIDENT_STATE_NAME_E_SCENE_18 = dplyr::if_else(
      is.na(SCENE_INCIDENT_STATE_NAME_E_SCENE_18) &
        !is.na(new_state),
      new_state,
      SCENE_INCIDENT_STATE_NAME_E_SCENE_18
    )
  ) |>
  clean_county_names_1(county_column = SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21,
                       city_column = SCENE_INCIDENT_CITY_NAME_E_SCENE_17,
                       zip_column = SCENE_INCIDENT_POSTAL_CODE_E_SCENE_19
                       ) |>
  clean_county_names_1(county_column = PATIENT_HOME_COUNTY_NAME_E_PATIENT_07,
                       city_column = PATIENT_HOME_CITY_NAME_E_PATIENT_06,
                       zip_column = PATIENT_HOME_POSTAL_CODE_E_PATIENT_09
                       ) |>
  clean_county_names_2(county_column = SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21,
                       zip_column = SCENE_INCIDENT_POSTAL_CODE_E_SCENE_19
                       ) |>
  clean_county_names_2(county_column = PATIENT_HOME_COUNTY_NAME_E_PATIENT_07,
                       zip_column = PATIENT_HOME_POSTAL_CODE_E_PATIENT_09
                       )

### procedures tables ############################################################
procedures_2021 <- import_nemsqa_data(table = "procedures", year = 2021)
procedures_2022 <- import_nemsqa_data(table = "procedures", year = 2022)
procedures_2023 <- import_nemsqa_data(table = "procedures", year = 2023)
procedures_2024 <- import_nemsqa_data(table = "procedures", year = 2024)

# bind rows for the procedures table
procedures_rbind <- dplyr::bind_rows(procedures_2021,
                                     procedures_2022,
                                     procedures_2023,
                                     procedures_2024
                                     )

# set up procedures table for manipulations
procedures_table <- procedures_rbind |>
  clean_names_dates_data()

### response tables ##############################################################
response_2021 <- import_nemsqa_data(table = "response", year = 2021)
response_2022 <- import_nemsqa_data(table = "response", year = 2022)
response_2023 <- import_nemsqa_data(table = "response", year = 2023)
response_2024 <- import_nemsqa_data(table = "response", year = 2024)

# bind rows for the response table
response_rbind <- dplyr::bind_rows(response_2021,
                                     response_2022,
                                     response_2023,
                                     response_2024
                                   )

# set up response table for manipulations
response_table <- response_rbind |>
  clean_names_dates_data()

### situation tables #############################################################
situation_2021 <- import_nemsqa_data(table = "situation", year = 2021)
situation_2022 <- import_nemsqa_data(table = "situation", year = 2022)
situation_2023 <- import_nemsqa_data(table = "situation", year = 2023)
situation_2024 <- import_nemsqa_data(table = "situation", year = 2024)

# bind rows for the situation table
situation_rbind <- dplyr::bind_rows(situation_2021,
                                   situation_2022,
                                   situation_2023,
                                   situation_2024
                                   )

# set up situation table for manipulations
situation_table <- situation_rbind |>
  clean_names_dates_data()

### vitals tables ################################################################
vitals_2021 <- import_nemsqa_data(table = "vitals", year = 2021)
vitals_2022 <- import_nemsqa_data(table = "vitals", year = 2022)
vitals_2023 <- import_nemsqa_data(table = "vitals", year = 2023)
vitals_2024 <- import_nemsqa_data(table = "vitals", year = 2024)

# bind rows for the vitals table
vitals_rbind <- dplyr::bind_rows(vitals_2021,
                                    vitals_2022,
                                    vitals_2023,
                                    vitals_2024
)

# set up vitals table for manipulations
vitals_table <- vitals_rbind |>
  clean_names_dates_data()
