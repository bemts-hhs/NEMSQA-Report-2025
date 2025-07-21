### IOWA NEMSQAR REPORT PREP 2025 ----------------------------------------------

# This script prepares for the analyses using the `nemsqar` package v1.1.0
# For the shapefiles, it is assumed that the files are downloaded from
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php using the year 2024
# and then utilizing the Counties (and equivalent), States (and equivalent), and
# Zip Code Tabulation Areas (ZCTAS) options in the dropdown dialogue to download
# the files manually and put them in the directory used here.

### PACKAGES -------------------------------------------------------------------

# CRAN versions ================================================================

# install these packages if not already

# install.packages(c("tidyverse", "traumar", "devtools", "remotes", "janitor",
#                    "gt", "gtsummary", "gtExtras", "zipcodeR", "naniar",
#                    "ggrepel", "devtools", "renv", "roxygen2",
#                    "roxygen2md", "nemsqar", "extrafont", "patchwork", "showtext"
#                   ))

# load packages ----
# library(tidyverse)
# library(traumar)
# library(devtools)
# library(remotes)
# library(nemsqar)
# library(janitor)
# library(gt)
# library(gtsummary)
# library(gtExtras)
# library(zipcodeR)
# library(naniar)
# library(ggrepel)
# library(devtools)
# library(renv)
# library(roxygen2)
# library(roxygen2md)
# library(extrafont)
# library(fontawesome)
# library(patchwork)
# library(systemfonts)
# library(showtext)

# showtext setup ----

# run showtext auto, use throughout project
showtext::showtext_auto()

# get 300 dpi with showtext
showtext::showtext_opts(dpi = 300)

# get work sans fonts of interest
all_fonts <- systemfonts::system_fonts()

# regular
work_sans <- all_fonts |>
  dplyr::filter(name == "WorkSans-Regular") |>
  dplyr::pull(path)

# semibold
work_sans_semibold <- all_fonts |>
  dplyr::filter(name == "WorkSans-SemiBold") |>
  dplyr::pull(path)

# extrabold
work_sans_extrabold <- all_fonts |>
  dplyr::filter(name == "WorkSans-ExtraBold") |>
  dplyr::pull(path)

# use sysfonts to load the fonts
sysfonts::font_add(
  family = "Work Sans",
  regular = work_sans,
  bold = work_sans_semibold
)

sysfonts::font_add(
  family = "Work Sans",
  regular = work_sans,
  bold = work_sans_extrabold
)


# Handy Functions --------------------------------------------------------------

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
      dplyr::across(
        tidyselect::matches("date(?!.*time)", perl = TRUE),
        ~ lubridate::mdy(
          stringr::str_remove_all(
            .,
            pattern = "\\s\\d+:\\d+(?::\\d+)?\\s[AP]M$"
          )
        )
      ),

      # Convert datetime fields
      dplyr::across(
        tidyselect::matches("date(?=.*time)", perl = TRUE),
        ~ lubridate::mdy_hms(
          stringr::str_remove_all(., pattern = "\\s[AP]M$")
        )
      ),

      # Create a unique ePCR number by concatenating PCR number with either datetime or date
      UNIQUE_EPCR_NUMBER = dplyr::if_else(
        !is.na(INCIDENT_DATE_TIME),
        stringr::str_c(
          INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
          INCIDENT_DATE_TIME
        ),
        stringr::str_c(
          INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
          INCIDENT_DATE
        )
      ),

      # Create a unique run ID by concatenating agency number, PCR number, and either datetime or date
      UNIQUE_RUN_ID = dplyr::if_else(
        !is.na(INCIDENT_DATE_TIME),
        stringr::str_c(
          AGENCY_NUMBER_D_AGENCY_02,
          INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
          INCIDENT_DATE_TIME
        ),
        stringr::str_c(
          AGENCY_NUMBER_D_AGENCY_02,
          INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
          INCIDENT_DATE
        )
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
clean_county_names_1 <- function(df, county_column, city_column, zip_column) {
  # let x be a named column within a data.frame

  if (!is.data.frame(df) && !is_tibble(df)) {
    cli::cli_abort(
      "The first argument `df` of the input was of class {.cls {class(df)}}but must be a {.cls data.frame}.  Please supply a {.cls data.frame} to the argument {.var df}."
    )
  }

  clean_counties <- df |>
    dplyr::mutate(
      {{ county_column }} := stringr::str_remove_all(
        {{ county_column }},
        pattern = "(?:\\sCounty|\\scounty|/.*$|\\sCo$)"
      ),
      {{ county_column }} := stringr::str_to_title({{ county_column }})
    ) |>
    dplyr::mutate(
      {{ county_column }} := dplyr::if_else(
        grepl(
          pattern = "(?:Al[l]*am[a]*kee)",
          {{ county_column }},
          ignore.case = TRUE
        ),
        "Allamakee",
        # using various regex formulations to address mispellings to standardize county
        dplyr::if_else(
          grepl(
            pattern = "(?:[0-9]+)",
            {{ county_column }},
            ignore.case = TRUE
          ),
          new_county,
          dplyr::if_else(
            grepl(
              pattern = "waterloo",
              x = {{ city_column }},
              ignore.case = TRUE
            ),
            "Black Hawk",
            dplyr::if_else(
              grepl(
                pattern = "saint clair",
                x = {{ city_column }},
                ignore.case = TRUE
              ),
              "Benton",
              dplyr::if_else(
                grepl(
                  pattern = "(?:Audrain)",
                  {{ county_column }},
                  ignore.case = TRUE
                ),
                new_county,
                dplyr::if_else(
                  grepl(
                    pattern = "(?:Blair)",
                    {{ county_column }},
                    ignore.case = TRUE
                  ),
                  new_county,
                  dplyr::if_else(
                    grepl(
                      pattern = "evansdale",
                      x = {{ city_column }},
                      ignore.case = TRUE
                    ),
                    "Black Hawk",
                    dplyr::if_else(
                      grepl(
                        pattern = "(?:Buchan[ao]n)",
                        {{ county_column }},
                        ignore.case = TRUE
                      ),
                      "Buchanan",
                      dplyr::if_else(
                        grepl(
                          pattern = "iowa city",
                          x = {{ city_column }},
                          ignore.case = TRUE
                        ),
                        "Johnson",
                        dplyr::if_else(
                          grepl(
                            pattern = "(?:Clay[r]*ton)",
                            {{ county_column }},
                            ignore.case = TRUE
                          ),
                          "Clayton",
                          dplyr::if_else(
                            grepl(
                              pattern = "(?:Del[ae]*ware)",
                              {{ county_column }},
                              ignore.case = TRUE
                            ),
                            "Delaware",
                            dplyr::if_else(
                              grepl(
                                pattern = "(?:di[ck]*[a-z]+(osn|son|non))",
                                {{ county_column }},
                                ignore.case = TRUE
                              ),
                              "Dickinson",
                              dplyr::if_else(
                                grepl(
                                  pattern = "(?:green[e]*)",
                                  {{ county_column }},
                                  ignore.case = TRUE
                                ),
                                "Greene",
                                dplyr::if_else(
                                  grepl(
                                    pattern = "saylor",
                                    x = {{ city_column }},
                                    ignore.case = TRUE
                                  ),
                                  "Polk",
                                  dplyr::if_else(
                                    grepl(
                                      pattern = "(?:^ia$)",
                                      {{ county_column }},
                                      ignore.case = TRUE
                                    ),
                                    new_county,
                                    dplyr::if_else(
                                      grepl(
                                        pattern = "clinton",
                                        {{ city_column }},
                                        ignore.case = TRUE
                                      ),
                                      "Clinton",
                                      dplyr::if_else(
                                        grepl(
                                          pattern = "(?:indianola)",
                                          {{ city_column }},
                                          ignore.case = TRUE
                                        ),
                                        "Warren",
                                        dplyr::if_else(
                                          grepl(
                                            pattern = "(?:jo(n|h)(hson|hnson|nson|oson|son))",
                                            {{ county_column }},
                                            ignore.case = TRUE
                                          ),
                                          "Johnson",
                                          dplyr::if_else(
                                            grepl(
                                              pattern = "(?:Kewaunee)",
                                              {{ county_column }},
                                              ignore.case = TRUE
                                            ),
                                            new_county,
                                            dplyr::if_else(
                                              grepl(
                                                pattern = "(?:mar[r]*ion)",
                                                {{ county_column }},
                                                ignore.case = TRUE
                                              ),
                                              "Marion",
                                              dplyr::if_else(
                                                grepl(
                                                  pattern = "(?:o[']*brien)",
                                                  {{ county_column }},
                                                  ignore.case = TRUE
                                                ),
                                                "O'Brien",
                                                dplyr::if_else(
                                                  grepl(
                                                    pattern = "(?:wesley|Algona)",
                                                    {{ city_column }},
                                                    ignore.case = TRUE
                                                  ),
                                                  "Kossuth",
                                                  dplyr::if_else(
                                                    grepl(
                                                      pattern = "(?:Iowa City)",
                                                      {{ city_column }},
                                                      ignore.case = TRUE
                                                    ),
                                                    "Johnson",
                                                    dplyr::if_else(
                                                      grepl(
                                                        pattern = "(?:poc[h]*ahontas)",
                                                        {{ county_column }},
                                                        ignore.case = TRUE
                                                      ),
                                                      "Pocahontas",
                                                      dplyr::if_else(
                                                        grepl(
                                                          pattern = "(?:altoona)",
                                                          {{ city_column }},
                                                          ignore.case = TRUE
                                                        ),
                                                        "Polk",
                                                        dplyr::if_else(
                                                          grepl(
                                                            pattern = "(?:Council Bluffs)",
                                                            {{ city_column }},
                                                            ignore.case = TRUE
                                                          ),
                                                          "Pottawattamie",
                                                          dplyr::if_else(
                                                            grepl(
                                                              pattern = "(?:Iowa Falls)",
                                                              {{ city_column }},
                                                              ignore.case = TRUE
                                                            ),
                                                            "Hardin",
                                                            dplyr::if_else(
                                                              grepl(
                                                                pattern = "(?:des moines|urbandale|ankeny)",
                                                                {{
                                                                  city_column
                                                                }},
                                                                ignore.case = TRUE
                                                              ),
                                                              "Polk",
                                                              dplyr::if_else(
                                                                grepl(
                                                                  pattern = "(?:van bur[r]*en)",
                                                                  {{
                                                                    county_column
                                                                  }},
                                                                  ignore.case = TRUE
                                                                ),
                                                                "Van Buren",
                                                                dplyr::if_else(
                                                                  grepl(
                                                                    pattern = "(?:war[nr]en)",
                                                                    {{
                                                                      county_column
                                                                    }},
                                                                    ignore.case = TRUE
                                                                  ),
                                                                  "Warren",
                                                                  dplyr::if_else(
                                                                    grepl(
                                                                      pattern = "(?:essex)",
                                                                      {{
                                                                        city_column
                                                                      }},
                                                                      ignore.case = TRUE
                                                                    ),
                                                                    "Page",
                                                                    dplyr::if_else(
                                                                      grepl(
                                                                        pattern = "(?:all[a]*makee)",
                                                                        {{
                                                                          county_column
                                                                        }},
                                                                        ignore.case = TRUE
                                                                      ),
                                                                      "Allamakee",
                                                                      dplyr::if_else(
                                                                        grepl(
                                                                          pattern = "(?:m[ao]nona)",
                                                                          {{
                                                                            county_column
                                                                          }},
                                                                          ignore.case = TRUE
                                                                        ),
                                                                        "Monona",
                                                                        dplyr::if_else(
                                                                          grepl(
                                                                            pattern = "(?:story)",
                                                                            {{
                                                                              county_column
                                                                            }},
                                                                            ignore.case = TRUE
                                                                          ),
                                                                          "Story",
                                                                          dplyr::if_else(
                                                                            grepl(
                                                                              pattern = "(?:[^a-z]+[0-9]+)",
                                                                              {{
                                                                                county_column
                                                                              }},
                                                                              ignore.case = TRUE
                                                                            ),
                                                                            new_county,
                                                                            dplyr::if_else(
                                                                              grepl(
                                                                                pattern = "(?:51012)",
                                                                                {{
                                                                                  zip_column
                                                                                }},
                                                                                ignore.case = TRUE
                                                                              ),
                                                                              "Cherokee",
                                                                              dplyr::if_else(
                                                                                grepl(
                                                                                  pattern = "(?:mingo)",
                                                                                  {{
                                                                                    county_column
                                                                                  }},
                                                                                  ignore.case = TRUE
                                                                                ),
                                                                                "Jasper",
                                                                                dplyr::if_else(
                                                                                  grepl(
                                                                                    pattern = "(?:norwalk)",
                                                                                    {{
                                                                                      county_column
                                                                                    }},
                                                                                    ignore.case = TRUE
                                                                                  ),
                                                                                  "Warren",
                                                                                  dplyr::if_else(
                                                                                    grepl(
                                                                                      pattern = "(?:elkhart)",
                                                                                      {{
                                                                                        county_column
                                                                                      }},
                                                                                      ignore.case = TRUE
                                                                                    ),
                                                                                    "Polk",
                                                                                    dplyr::if_else(
                                                                                      {{
                                                                                        county_column
                                                                                      }} ==
                                                                                        "County",
                                                                                      new_county,
                                                                                      dplyr::if_else(
                                                                                        {{
                                                                                          county_column
                                                                                        }} ==
                                                                                          "Grant",
                                                                                        "Montgomery",
                                                                                        dplyr::if_else(
                                                                                          {{
                                                                                            county_column
                                                                                          }} ==
                                                                                            "Burt",
                                                                                          "Kossuth",
                                                                                          dplyr::if_else(
                                                                                            {{
                                                                                              county_column
                                                                                            }} ==
                                                                                              "Carlisle",
                                                                                            "Warren",
                                                                                            dplyr::if_else(
                                                                                              {{
                                                                                                zip_column
                                                                                              }} ==
                                                                                                "52358",
                                                                                              "Cedar",
                                                                                              dplyr::if_else(
                                                                                                {{
                                                                                                  county_column
                                                                                                }} ==
                                                                                                  "Fulton",
                                                                                                "Jackson",
                                                                                                {{
                                                                                                  county_column
                                                                                                }}
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
                  )
                )
              )
            )
          )
        )
      )
    )

  return(clean_counties)
}

# Function to Clean County Names (Part 2)
# This function corrects county names in a data frame based on known
# misspellings and ZIP code associations. It is a continuation of a previous
# cleaning function due to `if_else()` limitations with excessive nesting.
clean_county_names_2 <-
  function(df, county_column, zip_column) {
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
      dplyr::mutate(
        {{ county_column }} := dplyr::case_when(
          # Correct common misspellings of "Harrison"
          grepl(
            pattern = "harrision|harison",
            {{ county_column }},
            ignore.case = TRUE
          ) ~
            "Harrison",
          # Assign "Warren" county based on ZIP code 50125
          grepl(pattern = "50125", {{ zip_column }}, ignore.case = TRUE) ~
            "Warren",
          # Assign "Harrison" county based on ZIP code 51546
          grepl(pattern = "51546", {{ zip_column }}, ignore.case = TRUE) ~
            "Harrison",
          # Retain original county name if no match is found
          TRUE ~ {{ county_column }}
        )
      )

    # Return the cleaned data frame with corrected county names
    return(clean_counties)
  }

### HELPER FILES/FUNCTIONS =====================================================

#_____________________________________________________________________________
# Function: generate_random_ID()
#_____________________________________________________________________________
# This function generates a set of unique random IDs consisting of 10
# uppercase and lowercase letters followed by a 10-digit random number.
#
# Arguments:
#   - n: The number of random IDs to generate.
#   - set_seed: (Optional) A numeric seed value for reproducibility.
#               If NULL, randomness is not controlled.
#
# Returns:
#   - A character vector of length `n`, where each element is a unique ID
#     formatted as "XXXXXXXXXX-YYYYYYYYYY".
#
# Notes:
#   - The function ensures each generated ID consists of a randomly sampled
#     combination of letters and a randomly generated numeric sequence.
#   - Uses `cli::cli_warn()` for warnings and `cli::cli_abort()` for errors.
#_____________________________________________________________________________
generate_random_ID <- function(n, set_seed = 12345) {
  # Validate input: `n` must be a positive integer
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n %% 1 != 0) {
    cli::cli_abort(
      "The argument {.var n} must be a single positive integer.
      Received {.val {n}} of class {.cls {class(n)}}."
    )
  }

  # Validate input: `set_seed` must be either NULL or a single numeric value
  if (!is.null(set_seed) && (!is.numeric(set_seed) || length(set_seed) != 1)) {
    cli::cli_abort(
      "The argument {.var set_seed} must be either NULL or a single numeric value.
      Received {.val {set_seed}} of class {.cls {class(set_seed)}}."
    )
  }

  # Optionally set the seed for reproducibility
  if (!is.null(set_seed)) {
    set.seed(set_seed)
  } else {
    cli::cli_warn(
      "Reproducibility warning: {.var set_seed} was not specified, so results
      may vary across function calls. To ensure consistent results, provide a
      numeric value to {.var set_seed}."
    )
  }

  # Initialize an empty character vector to store generated IDs
  random_strings <- vector(mode = "character", length = n)

  # Generate `n` random IDs
  for (i in seq_len(n)) {
    random_strings[i] <- paste0(
      paste0(sample(c(LETTERS, letters, LETTERS), size = 10), collapse = ""), # 10-letter string
      "-",
      sample(1000000000:9999999999, size = 1) # 10-digit number
    )
  }

  # Return the vector of generated IDs
  return(random_strings)
}

#_____________________________________________________________________________
# Function: prepare_map_data()
#_____________________________________________________________________________
# This function loads and prepares shapefile data for mapping purposes,
# specifically filtering to Iowa (STATEFP == "19"). It supports three shapefile
# types: county, state, and ZIP Code Tabulation Area (ZCTA).
#
# Arguments:
#   - type: Character. Specifies the geographic unit to load. Must be one of:
#           "county", "state", or "zcta". Default is "county".
#
# Returns:
#   - A tibble (class `sf` with tibble structure) containing shapefile geometry
#     and attribute data, filtered to Iowa (STATEFP == "19").
#
# Notes:
#   - Uses {sf} for reading shapefiles and {dplyr} for filtering.
#   - Relies on standardized 2024 TIGER/Line shapefiles located in a
#     predetermined directory.
#   - Ensures consistent file selection and state filtering for downstream
#     geospatial analysis.
#_____________________________________________________________________________
prepare_map_data <- function(type = c("county", "state", "zcta")) {
  # Validate the `type` argument and resolve its value
  type <- match.arg(type, choices = c("county", "state", "zcta"))

  # Determine the correct shapefile name based on `type`
  file <- if (type == "county") {
    "tl_2024_us_county.shp"
  } else if (type == "state") {
    "tl_2024_us_state.shp"
  } else if (type == "zcta") {
    "tl_2024_us_zcta520.shp"
  }

  # Get the secure shapefile path
  shapefile_path <- Sys.getenv("SHAPE_FILE_PATH")

  # Construct the full file path to the shapefile
  filepath <- file.path(
    shapefile_path,
    type,
    file
  )

  # Validate that the shapefile exists
  if (!file.exists(filepath)) {
    cli::cli_abort(
      "The shapefile {.file {filepath}} does not exist.
      Please verify the path and file structure."
    )
  }

  # Attempt to read the shapefile using {sf}
  shapefile <- tryCatch(
    sf::read_sf(dsn = filepath, as_tibble = TRUE),
    error = function(e) {
      cli::cli_abort(
        "Failed to read shapefile: {.file {filepath}}. \nError: {e$message}"
      )
    }
  )

  # Validate that the required field `STATEFP` exists
  if (!"STATEFP" %in% names(shapefile)) {
    cli::cli_abort(
      "The shapefile is missing the required field {.var STATEFP}."
    )
  }

  # Filter the shapefile to only include records from Iowa (FIPS code "19")
  shapefile <- shapefile |>
    dplyr::filter(STATEFP == "19")

  # Return the filtered shapefile
  return(shapefile)
}

# Get the secure county data file path
county_data_path <- Sys.getenv("COUNTY_FILE_PATH")

# get location data
# Iowa county data
county_data <- readxl::read_excel(
  path = county_data_path
)

# essential service counties
essential_counties <- county_data |>
  dplyr::filter(`EMS Essential Service` == TRUE) |>
  dplyr::pull(County)

# helper object for manipulations
location <-
  county_data |>
  dplyr::select(
    County,
    `Region: Preparedness`,
    Pop:Designation,
    `EMS Essential Service`
  ) |>
  dplyr::mutate(
    `EMS Essential Service` = dplyr::if_else(
      County %in% essential_counties,
      TRUE,
      FALSE
    )
  )

# zipcode level data
zipcodes <- zipcodeR::zip_code_db |>
  dplyr::mutate(
    county = stringr::str_remove_all(county, pattern = "(?:\\sCounty)")
  ) |>
  dplyr::select(major_city, state, county, zipcode, lat, lng) |>
  dplyr::mutate(
    county = dplyr::if_else(
      state == "IA" &
        county == "",
      "Polk",
      county
    )
  ) |>
  dplyr::rename("new_city" = "major_city") |>
  dplyr::rename("new_state" = "state") |>
  dplyr::rename("new_county" = "county") |>
  dplyr::rename("new_zipcode" = "zipcode")

# unzip the larger US data from Geonames
# create a temp file to hold the download as a container
temp <- tempfile()

# download the temp file
download.file("https://download.geonames.org/export/dump/US.zip", temp)

# unzip the zip file and pull the specific flat file
con <- unz(temp, "US.txt")

# read in the file of interest and specify column names and types using readr
US <-
  readr::read_delim(
    file = con,
    col_names = FALSE,
    col_types = list(
      readr::col_character(),
      readr::col_character(),
      readr::col_character(),
      readr::col_character(),
      readr::col_number(),
      readr::col_number(),
      readr::col_character(),
      readr::col_character(),
      readr::col_character(),
      readr::col_character(),
      readr::col_character(),
      readr::col_character(),
      readr::col_character(),
      readr::col_character(),
      readr::col_integer(),
      readr::col_integer(),
      readr::col_guess(),
      readr::col_character(),
      readr::col_date()
    )
  )

# remove the temp file
unlink(temp)

###_____________________________________________________________________________
# manipulate the US file, filter down to Iowa, and get formatting right
# for colnames, refer to: https://download.geonames.org/export/dump/readme.txt
###_____________________________________________________________________________

US_clean <- US |>
  dplyr::rename(
    geonameid = X1,
    name = X2,
    asciiname = X3,
    alternatenames = X4,
    latitude = X5,
    longitude = X6,
    feature_class = X7,
    feature_code = X8,
    country_code = X9,
    cc2 = X10,
    admin1_code = X11,
    admin2_code = X12,
    admin3_code = X13,
    admin4_code = X14,
    population = X15,
    elevation = X16,
    dem = X17,
    timezone = X18,
    modification_date = X19
  ) |>
  dplyr::filter(admin1_code == "IA", feature_class == "P") |>
  # remove the (historical) suffix to the "abandonded" populated places
  dplyr::mutate(
    name = stringr::str_squish(stringr::str_remove(
      name,
      pattern = "\\s\\(historical\\)"
    ))
  )

###_____________________________________________________________________________
# load in geonames county-level data
###_____________________________________________________________________________

geonames_county <-
  readr::read_delim(
    file = "https://download.geonames.org/export/dump/admin2Codes.txt",
    col_names = F
  ) |>
  dplyr::rename(
    code = X1,
    name = X2,
    asciiname = X3,
    geonameID = X4
  ) |>
  dplyr::mutate(
    country_code = stringr::str_split(code, "\\.", simplify = TRUE)[, 1],
    state_code = stringr::str_split(code, "\\.", simplify = TRUE)[, 2],
    county_code = stringr::str_split(code, "\\.", simplify = TRUE)[, 3],
    .after = code
  )

###_____________________________________________________________________________
# geonames admin2 data filtered data to US and Iowa
###_____________________________________________________________________________

geonames_admin2_iowa <- geonames_county |>
  dplyr::filter(country_code == "US", state_code == "IA")

###_____________________________________________________________________________
# join the county names / codes from geonames to the city data from geonames as their codes are unique
# geonames does not use the same codes as Census Bureau
###_____________________________________________________________________________

# final manipulations of the Iowa data to join to the AED data

Iowa_Data_Final <- US_clean |>
  dplyr::left_join(
    geonames_admin2_iowa |> dplyr::select(county_code, name),
    by = c("admin2_code" = "county_code"),
    suffix = c("_city", "_county")
  ) |>
  dplyr::relocate(name_county, .after = name_city) |>
  dplyr::mutate(
    name_county = stringr::str_remove_all(
      name_county,
      pattern = "\\s[Cc]ounty"
    ),
    name_city = stringr::str_to_upper(name_city)
  ) |>
  dplyr::filter(
    !(name_city == "CENTERVILLE" & name_county == "Boone") &
      !(name_city == "PLEASANT HILL" &
        name_county == "Van Buren") &
      !(name_city == "HOLY CROSS" &
        name_county == "Delaware") &
      !(name_city == "FOREST CITY" &
        name_county == "Howard") &
      !(name_city == "GENEVA" & name_county == "Benton") &
      !(name_city == "WESTFIELD" &
        name_county == "Poweshiek") &
      !(name_city == "TROY" & name_county == "Lucas") &
      !(name_city == "WASHINGTON" &
        name_county == "Woodbury") &
      !(name_city == "WEBSTER" & name_county == "Madison") &
      !(name_city == "RIVERSIDE" &
        name_county == "Woodbury") &
      !(name_city == "WASHINGTON" &
        name_county == "Franklin") &
      !(name_city == "VAN")
  ) |>
  dplyr::left_join(
    county_data |> dplyr::select(County, `Region: Preparedness`, Designation),
    by = c("name_county" = "County")
  ) |>
  dplyr::relocate(`Region: Preparedness`, .after = name_county)

# geonames is missing some of the cities / towns in Iowa, this is a product
# of the analyses below and may need to be revised periodically

missing_location_data <- tibble::tibble(
  geonameid = generate_random_ID(7),
  # Generating random geoname IDs
  name_city = c(
    "TERRILL",
    "LEMARS",
    "FREDRICKSBURG",
    "MOLVILLE",
    "CALLENDAR",
    "SYDNEY",
    "DESOTO"
  ),
  name_county = c(
    "Dickinson",
    "Plymouth",
    "Chickasaw",
    "Woodbury",
    "Webster",
    "Fremont",
    "Dallas"
  ),
  latitude = c(
    43.305473,
    42.7942,
    42.964586,
    42.488210,
    42.362592,
    40.7592,
    41.5316
  ),
  longitude = c(
    -94.971433,
    -96.1656,
    -92.198465,
    -96.069997,
    -94.293268,
    -95.6668,
    -94.0078
  ),
  population = rep(NA, 7),
  elevation = rep(NA, 7),
  dem = rep(NA, 7),
  timezone = rep(NA_character_, 7),
  modification_date = rep(lubridate::NA_Date_, 7), # Change to NA_Date_
  `Region: Preparedness` = c("7", "3", "2", "3", "7", "4", "1A"), # Adding region information
  asciiname = rep(NA_character_, 7),
  alternatenames = rep(NA_character_, 7),
  feature_class = rep(NA_character_, 7),
  feature_code = rep(NA_character_, 7),
  country_code = rep(NA_character_, 7),
  cc2 = rep(NA_character_, 7),
  admin1_code = rep(NA_character_, 7),
  admin2_code = c("059", "149", "037", "193", "187", "071", "049"),
  # Adding admin2_code
  admin3_code = rep(NA_character_, 7),
  admin4_code = rep(NA_character_, 7)
) |>
  dplyr::left_join(
    county_data |> dplyr::select(County, Designation),
    by = c("name_county" = "County")
  ) |>
  dplyr::select(colnames(Iowa_Data_Final))

Iowa_Data_Final <- dplyr::bind_rows(Iowa_Data_Final, missing_location_data)

### DATA MANIPULATION FACILITIES ===============================================

#_____________________________________________________________________________
# Function: format_cut_levels()
#_____________________________________________________________________________
# Description:
#   Converts a vector of interval-style bin labels (e.g., "(0.1,0.2]") into
#   a more readable percentage range format (e.g., "10%-20%").
#
#   This is useful for relabeling cut() interval outputs in visualizations or
#   summary tables with more user-friendly percent formatting.
#
# Arguments:
#   - bins: A character or factor vector containing interval labels in the
#           standard cut() format (e.g., "(0.1,0.2]", "[0.3,0.4)", etc.).
#
# Returns:
#   - A character vector with formatted percentage ranges (e.g., "30%-40%").
#
# Dependencies:
#   - Uses {cli} for input validation errors.
#_____________________________________________________________________________
format_bin_levels <- function(bins, format = c("decimal", "percent")) {
  # --- DATA VALIDATION ---

  # Validate `format` choices
  format <- match.arg(format, choices = c("decimal", "percent"))

  # Check that 'bins' argument was supplied
  if (missing(bins)) {
    cli::cli_abort(
      "The {.arg bins} argument is missing. Please supply a character or factor vector."
    )
  }

  # Check that input is character or factor
  if (!is.character(bins) && !is.factor(bins)) {
    cli::cli_abort(
      "Input must be a character vector or factor. Input of class {.cls {class(bins)}} is not supported."
    )
  }

  # --- TRANSFORMATION LOGIC ---

  # Trim whitespace to ensure consistency in parsing
  bins_trim <- bins |> trimws()

  # Remove brackets and parentheses (e.g., "(0.1,0.2]" → "0.1,0.2")
  bins_sub <- bins_trim |>
    gsub(pattern = "\\[|\\]|\\(|\\)", replacement = "", x = _)

  # Optionally format as a percentage
  if (format == "percent") {
    # Split the cleaned strings at the comma to isolate lower/upper bounds
    bins_split <- bins_sub |> strsplit(x = _, split = ",")

    # Convert each lower/upper bound to a numeric percentage and format as text
    bins_list <- bins_split |>
      lapply(X = _, function(x) {
        lower <- as.numeric(x[1]) * 100
        upper <- as.numeric(x[2]) * 100
        paste0(round(lower), "%-", round(upper), "%")
      })

    # Unlist to convert from list to character vector
    bins_out <- unlist(bins_list)

    # Return the formatted bin labels
    return(bins_out)

    # Otherwise just return the trimmed/formatted bins
  } else if (format == "decimal") {
    # Remove comma
    bins_sub <- bins_sub |> gsub(pattern = ",", replacement = "-", x = _)

    # Return the formatted bin labels
    return(bins_sub)
  }
}

#_____________________________________________________________________________
# Function: fix_county_region()
#_____________________________________________________________________________
# This function standardizes city names, fills in missing county information
# using an external reference, and assigns region values based on county data.
# It ensures consistency in geographic data and handles out-of-state or
# missing counties.
#
# Arguments:
#   - df: A data frame containing city, county, and region columns.
#   - city_col: Column in `df` containing city names.
#   - county_col: Column in `df` containing county names.
#   - region_col: Column in `df` containing region names.
#   - external_city: External reference vector of city names.
#   - external_county: External reference vector of county names.
#   - external_region: External reference vector of region names.
#
# Returns:
#   - A modified version of `df` with:
#     * City names standardized (uppercase, unnecessary prefixes removed).
#     * Missing county values filled in based on city-to-county mapping.
#     * Counties labeled as "OOS or Missing" if they do not match `external_county`.
#     * Missing region values assigned based on county-to-region mapping.
#
# Notes:
#   - Uses `{dplyr}` for data transformation.
#   - Ensures county and region assignments are accurate by cross-referencing
#     external datasets.
#   - Cities with prefixes like "NORTH OF", "SOUTH OF" are normalized.
#_____________________________________________________________________________
fix_county_region <- function(
  df,
  city_col,
  county_col,
  region_col,
  external_city,
  external_county,
  external_region
) {
  # Validate input: `df` must be a data frame or tibble
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      "The argument {.var df} must be a data frame or tibble, but received {.cls {class(df)}}."
    )
  }

  # Validate input: `external_city`, `external_county`, `external_region` must be vectors
  if (
    !is.vector(external_city) ||
      !is.vector(external_county) ||
      !is.vector(external_region)
  ) {
    cli::cli_abort(
      "The arguments {.var external_city}, {.var external_county}, and {.var external_region} must be vectors."
    )
  }

  # Standardize city names: Convert to uppercase and remove directional prefixes
  df <- df |>
    dplyr::mutate(
      {{ city_col }} := stringr::str_to_upper({{ city_col }}),
      {{ city_col }} := stringr::str_remove_all(
        {{ city_col }},
        pattern = "\\w+\\sOF\\s"
      )
    )

  # Fill in missing county names using external city-to-county mapping
  df <- df |>
    dplyr::mutate(
      {{ county_col }} := dplyr::coalesce(
        {{ county_col }},
        Iowa_Data_Final$name_county[match({{ city_col }}, {{ external_city }})]
      )
    )

  # Assign "OOS or Missing" if county is not found in the reference county list
  df <- df |>
    dplyr::mutate(
      {{ county_col }} := dplyr::if_else(
        !{{ county_col }} %in% unique({{ external_county }}),
        "OOS or Missing",
        {{ county_col }}
      )
    )

  # Assign region values using external county-to-region mapping
  df <- df |>
    dplyr::mutate(
      {{ region_col }} := dplyr::coalesce(
        {{ region_col }},
        {{ external_region }}[match({{ county_col }}, {{ external_county }})]
      )
    )

  return(df)
}

#_____________________________________________________________________________
# Function: Prepare Population Statistical File
#_____________________________________________________________________________
# This function processes population statistical data by reshaping, renaming,
# and labeling small counts. It ensures consistency in formatting for analysis.
#_____________________________________________________________________________
prepare_population_statistical_file <- function(df) {
  # Validate input: Ensure `df` is a data frame or tibble
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      "The input `df` must be a {.cls data.frame} or {.cls tibble},
      but received an object of class {.cls {class(df)}}."
    )
  }

  # Validate required columns exist in `df`
  required_columns <- c("filter", "YEAR", "count")
  missing_columns <- setdiff(required_columns, colnames(df))

  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "The input data frame is missing required columns: {.var {missing_columns}}.
      Ensure `df` contains {required_columns}."
    )
  }

  # Transform the data: Pivot, modify labels, and apply small count suppression
  prepared_df <- df |>
    # Reshape data from long to wide format
    tidyr::pivot_wider(
      id_cols = filter,
      names_from = YEAR,
      values_from = count
    ) |>

    # Standardize terminology by replacing "call" or "calls" with "runs"
    dplyr::mutate(
      filter = stringr::str_replace_all(
        string = filter,
        pattern = "call",
        replacement = "run"
      )
    ) |>

    # Create a trend column with population counts over multiple years
    dplyr::rowwise() |>
    dplyr::mutate(
      `Population Trend` = list(c(`2021`, `2022`, `2023`, `2024`))
    ) |>
    dplyr::ungroup() |>

    # Apply small count suppression for confidentiality
    dplyr::mutate(dplyr::across(
      `2021`:`2024`,
      ~ traumar::small_count_label(., cutoff = 6, replacement = NA_integer_)
    )) |>

    # Rename the primary identifier column for clarity
    dplyr::rename(Populations = filter)

  # Return the processed data frame
  return(prepared_df)
}

#_____________________________________________________________________________
# Function: Prepare Results Statistical File with Small Count Suppression
#_____________________________________________________________________________
# This function processes a statistical dataset by applying small count
# suppression to the numerator and adjusting the denominator accordingly.
# It ensures compliance with data privacy standards by replacing small values
# with NA when they fall below a defined cutoff.
#
# Arguments:
#   - df: A data frame containing statistical results, including at least
#         `numerator` and `denominator` columns.
#
# Returns:
#   - A modified data frame with suppressed small counts in the numerator
#     and corresponding adjustments in the denominator.
#_____________________________________________________________________________
prepare_results_statistical_file <- function(df) {
  # Validate input: Ensure `df` is a data frame or tibble
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      "The input `df` must be a {.cls data.frame} or {.cls tibble},
      but received an object of class {.cls {class(df)}}."
    )
  }

  # Validate required columns exist
  required_cols <- c("numerator", "denominator")
  missing_cols <- setdiff(
    required_cols,
    colnames(df)[colnames(df) %in% c("numerator", "denominator")]
  )

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "The input `df` is missing the following required columns: {.val {missing_cols}}.
      Ensure the dataset contains all necessary fields before processing."
    )
  }

  # Apply small count suppression to the numerator and adjust the denominator
  prepared_df <- df |>
    dplyr::mutate(
      numerator = traumar::small_count_label(
        var = numerator, # Apply small count suppression to numerator
        cutoff = 6, # Replace values below the threshold
        replacement = NA_integer_ # Replace small values with NA
      ),
      denominator = dplyr::if_else(
        is.na(numerator), # If numerator is suppressed (NA),
        NA_integer_, # also suppress the denominator
        denominator # Otherwise, retain original value
      )
    )

  # Return the modified data frame
  return(prepared_df)
}

### DATA IMPORT FACILITIES =====================================================

# Import NEMSQA Data from a CSV File
# This function imports data for a specified National EMS Quality Alliance
# (NEMSQA) table and year, reading the corresponding CSV file into R. If no file
# location is provided, it defaults to a predefined directory.
import_nemsqa_data <- function(location = NULL, table, year) {
  # Secure the base path
  base_path <- Sys.getenv("BASE_FILE_PATH")

  # If no file location is provided, set the default directory.
  if (is.null(location)) {
    location <- base_path
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

  # Get the secure output file path
  output_file_path <- Sys.getenv("OUTPUT_FILE_PATH")

  with(temp_env, {
    # If no file location is provided, use the default directory.
    if (is.null(location)) {
      location <- file.path(
        output_file_path
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
      for (file in list.files(
        path = working_path,
        pattern = "\\.csv$",
        full.names = TRUE
      )) {
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

# Load and clean multiple NEMSQA data files in parallel.
#
# This function imports and cleans multiple years or file chunks for a given NEMSQA table
# using parallel processing. Each year/chunk is handled independently in parallel worker
# processes, and the results are combined into a single tibble upon completion.
#
# Arguments:
# - table: character string indicating the table name to import.
# - years: character or numeric vector of year or chunk identifiers.
# - cores: optional integer specifying how many cores to use (defaults to physical cores - 4).
#
# Requirements:
# - The import_nemsqa_data() and clean_names_dates_data() functions must be defined
#   in the global environment.
# - The cli package is used for progress messaging.
load_nemsqa_parallel <- function(table, years, cores = NULL) {
  # Use default: all physical cores minus 4 for safety, unless specified
  if (is.null(cores)) {
    cores <- max(1, parallel::detectCores(logical = FALSE) - 4)
  }

  cli::cli_h1("Parallel NEMSQA Data Import")
  cli::cli_alert_info("Table: {.val {table}}")
  cli::cli_alert_info("Years/chunks: {.val {paste(years, collapse = ', ')}}")
  cli::cli_alert_info("Workers: {.val {cores}}")

  # Define a function to run in each parallel worker
  load_and_clean <- function(yr) {
    dat <- import_nemsqa_data(table = table, year = yr)
    clean_names_dates_data(dat)
  }

  # Initialize the parallel cluster
  cl <- parallel::makeCluster(cores)

  # Export required functions and variables to workers
  parallel::clusterExport(
    cl,
    varlist = c("import_nemsqa_data", "clean_names_dates_data", "table"),
    envir = .GlobalEnv
  )

  cli::cli_alert_info("Starting parallel import and cleaning...")

  # Process years/chunks in parallel, one at a time to show progress
  cleaned_list <- vector("list", length(years))
  for (i in seq_along(years)) {
    yr <- years[[i]]
    cleaned_list[[i]] <- parallel::parLapply(cl, list(yr), load_and_clean)[[1]]
    cli::cli_alert_success("Completed year/chunk {.val {yr}}")
  }

  # Shut down the cluster
  parallel::stopCluster(cl)
  cli::cli_alert_info("Combining results...")

  result <- dplyr::bind_rows(cleaned_list)

  cli::cli_alert_success("Finished. Returning combined tibble.")
  return(result)
}


### DATA VISUALIZATION FACILITIES ==============================================

#_____________________________________________________________________________
# Function: results_to_county_map()
#_____________________________________________________________________________
# This function generates a choropleth map displaying county-level performance
# metrics for a given measure using a shapefile of Iowa counties.
#
# Arguments:
#   - df: A data frame containing measure results by county.
#   - county_col: The column in `df` containing county names.
#   - add_text: Logical, if TRUE, adds text labels to the map.
#   - format: Logical, if TRUE, formats `bins` as a percentage.
#     This mostly affects the plot legend.
#
# Returns:
#   - A ggplot object visualizing the performance data by county.
#
# Notes:
#   - Requires {ggplot2}, {dplyr}, {tidyr}, {stringr}, {glue}, and {sf}.
#   - Uses {traumar::pretty_percent()} to format proportions.
#   - The `bins` variable categorizes proportions into (default = 10%) intervals.
#   - The color scale is based on the `magma` palette from {viridis}.
#_____________________________________________________________________________
results_to_county_map <- function(
  df,
  county_col = SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21,
  add_text = FALSE,
  format = c("decimal", "percent"),
  by = 0.1
) {
  # Validate input: `df` must be a data frame or tibble
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      "The argument {.var df} must be a data frame or tibble, but received {.cls {class(df)}}."
    )
  }

  # Validate input: `county_col` must exist in `df`
  if (!rlang::as_string(rlang::ensym(county_col)) %in% names(df)) {
    cli::cli_abort(
      "The specified {.var county_col} column does not exist in {.var df}."
    )
  }

  # Extract the measure name for the plot title
  measure <- unique(df$measure)

  # Check if there is an "all" category in the statistics
  # groupings, and if so just use that for the counties
  populations <- unique(df$pop)

  if (
    sum(
      grepl(
        pattern = "all",
        x = populations,
        ignore.case = TRUE
      ),
      na.rm = TRUE
    ) >
      0
  ) {
    check_result <- TRUE
  } else {
    check_result <- FALSE
  }

  # If there is an "all" category, use it
  if (!check_result) {
    # Aggregate statistics created from the nemsqar function
    aggregates <- df |>
      dplyr::summarize(
        numerator = sum(numerator, na.rm = TRUE),
        denominator = sum(denominator, na.rm = TRUE),
        prop = round(numerator / denominator, digits = 3),
        prop_label = dplyr::if_else(
          is.na(prop),
          "NA",
          traumar::pretty_percent(prop, n_decimal = 0)
        ),
        .by = {{ county_col }}
      )
  } else if (check_result) {
    # Aggregate statistics created from the nemsqar function
    aggregates <- df |>
      dplyr::filter(pop == "All") |>
      dplyr::mutate(
        prop_label = dplyr::if_else(
          is.na(prop),
          "NA",
          traumar::pretty_percent(prop, n_decimal = 0)
        )
      )
  }

  # Prepare county-level data: Aggregate numerators and denominators, calculate proportions
  # Optionally format the bins as percentages
  temp_obj <- iowa_counties_sf |>
    dplyr::left_join(county_data, by = dplyr::join_by(NAME == County)) |>
    dplyr::left_join(
      aggregates,
      by = dplyr::join_by(NAME == {{ county_col }})
    ) |>
    tidyr::replace_na(replace = list(prop = 0)) |>
    dplyr::mutate(
      bins = cut(
        prop,
        breaks = c(seq(from = 0, to = 1, by = by)),
        include.lowest = TRUE
      ),
      bins = format_bin_levels(bins = bins, format = format),
      bins = factor(bins)
    )

  # get missing or OOS county data
  oos_missing <- aggregates |>
    dplyr::filter({{ county_col }} == "OOS or Missing") |>
    dplyr::pull(prop_label)

  # get missing or OOS county text for caption
  oos_missing_caption <- glue::glue(
    "Performance for responses to Out of State or Missing counties: {oos_missing}"
  )

  # Define ggplot object with text in county borders
  if (add_text) {
    temp_plot <- suppressWarnings(
      temp_obj |>
        # Define text color
        dplyr::mutate(
          text_color = dplyr::if_else(
            prop < 0.3,
            "black",
            dplyr::if_else(is.na(prop), "black", "white")
          )
        ) |>
        ggplot2::ggplot(ggplot2::aes(fill = bins)) +
        ggplot2::geom_sf(color = "#70C8B8") +
        ggplot2::geom_sf_text(
          # Ensure text_color is inside aes()
          ggplot2::aes(label = prop_label, color = text_color),

          fontface = "bold",
          family = "Work Sans"
        ) +
        ggplot2::scale_fill_viridis_d(direction = -1, option = "magma") +
        ggplot2::scale_color_identity() + # Use the colors as-is without mapping to a scale
        ggplot2::labs(
          fill = "",
          title = glue::glue("NEMSQA {measure} Overall Performance: Iowa"),
          subtitle = "Source: Iowa ImageTrend Elite || Years: 2021-2024",
          caption = oos_missing_caption
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            size = 20,
            face = "bold",
            family = "Work Sans",
            color = "#19405B"
          ),
          plot.subtitle = ggplot2::element_text(
            hjust = 0.5,
            size = 18,
            face = "bold",
            family = "Work Sans",
            color = "#70C8B8"
          ),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.text = ggplot2::element_text(
            size = 12,
            family = "Work Sans",
            face = "bold"
          ),
          # Increase legend text size
          legend.key.size = ggplot2::unit(1.25, "lines"),
          # Increase fill box size
          legend.margin = ggplot2::margin(t = 10, unit = "pt"),
          # Move legend up
          plot.caption = ggplot2::element_text(
            hjust = 0,
            size = 14,
            face = "bold",
            family = "Work Sans",
            color = "#03617A"
          )
        )
    )

    # Define ggplot object without text in county borders
  } else if (!add_text) {
    temp_plot <- suppressWarnings(
      temp_obj |>
        ggplot2::ggplot(ggplot2::aes(fill = bins)) +
        ggplot2::geom_sf(color = "#70C8B8") +
        ggplot2::geom_sf_text(
          # Ensure text_color is inside aes()
          ggplot2::aes(
            label = dplyr::if_else(prop_label == "NA", prop_label, ""),
            color = "black"
          ),

          fontface = "bold",
          family = "Work Sans"
        ) +
        ggplot2::scale_fill_viridis_d(direction = -1, option = "magma") +
        ggplot2::scale_color_identity() + # Use the colors as-is without mapping to a scale
        ggplot2::labs(
          fill = "",
          title = glue::glue("NEMSQA {measure} Overall Performance: Iowa"),
          subtitle = "Source: Iowa ImageTrend Elite || Years: 2021-2024",
          caption = oos_missing_caption
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            size = 20,
            face = "bold",
            family = "Work Sans",
            color = "#19405B"
          ),
          plot.subtitle = ggplot2::element_text(
            hjust = 0.5,
            size = 18,
            face = "bold",
            family = "Work Sans",
            color = "#70C8B8"
          ),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.text = ggplot2::element_text(
            size = 12,
            family = "Work Sans",
            face = "bold"
          ),
          # Increase legend text size
          legend.key.size = ggplot2::unit(1.25, "lines"),
          # Increase fill box size
          legend.margin = ggplot2::margin(t = 10, unit = "pt"),
          # Move legend up
          plot.caption = ggplot2::element_text(
            hjust = 0,
            size = 14,
            face = "bold",
            family = "Work Sans",
            color = "#03617A"
          )
        )
    )
  }

  return(temp_plot)
}

# Plot Population Trends from NEMSQA Population Data
# This function creates a column or line chart visualizing population trends
# across multiple years from the outputs of `*_population` functions in the
# `nemsqar` package.
plot_nemsqa_pops <- function(
  df,
  wrap_width = 50,
  type = c("col", "line"),
  plot_title,
  ...
) {
  # Add a helper variable to adjust text label positioning
  df <- df |>
    dplyr::mutate(nudge_var = dplyr::if_else(count > 10, -1, -count * 0.1))

  # Ensure `type` is a single valid choice
  type <- match.arg(type, choices = c("col", "line"))

  # Generate the base plot according to the selected type
  temp_plot <- if (type == "col") {
    # Column chart: Displays count data grouped by YEAR
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = YEAR, y = count, fill = factor(YEAR))
    ) +
      ggplot2::geom_col(alpha = 0.5, position = ggplot2::position_dodge())
  } else {
    # Line chart: Shows trends across years with a connecting line
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = YEAR, y = count, color = "lightgray")
    ) +
      ggplot2::geom_line(
        alpha = 0.5,
        linewidth = 1.5,
        lineend = "round",
        linejoin = "round"
      )
  }

  # Finalize the plot with additional aesthetics and labels
  plot_pops <- temp_plot +
    ggplot2::geom_text(
      ggplot2::aes(
        y = count + nudge_var,
        label = traumar::pretty_number(count, n_decimal = 2)
      ),
      size = 4,
      color = "darkslategray",
      fontface = "bold",
      family = "sans"
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x)
        traumar::pretty_number(x, n_decimal = 2, truncate = TRUE)
    ) +
    ggplot2::guides(fill = "none", color = "none") +
    ggplot2::facet_wrap(
      ~ stringr::str_wrap(filter, width = wrap_width),
      scales = "free_y"
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = glue::glue("{plot_title} Population Trends"),
      subtitle = "Source: ImageTrend Elite EMS Registry | CY 2021-2024"
    ) +
    traumar::theme_cleaner(...)

  return(plot_pops)
}

#_____________________________________________________________________________
# Function: Format Population Statistical File for GT Tables
#_____________________________________________________________________________
# This function takes a processed population statistical dataset and formats it
# into a high-quality table using the {gt} package. It applies integer
# formatting, generates a sparkline for population trends, and replaces missing
# values with "*".
#_____________________________________________________________________________
population_statistical_file_gt <- function(df, measure, fig_dim = c(5, 30)) {
  # Validate input: Ensure `df` is a data frame or tibble
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      "The input `df` must be a {.cls data.frame} or {.cls tibble},
      but received an object of class {.cls {class(df)}}."
    )
  }

  # Validate `measure`
  if (!is.character(measure)) {
    cli::cli_abort(
      c(
        "{.var measure} must be a character vector of length 1 indicating which NEMSQA measure the data are in reference to.",

        "i" = "{.var measure} had class {.cls {class(measure)}}."
      )
    )
  }

  # Validate required columns exist in `df`
  required_columns <- c(
    "Populations",
    "Population Trend",
    "2021",
    "2022",
    "2023",
    "2024"
  )
  missing_columns <- setdiff(required_columns, colnames(df))

  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "The input data frame is missing required columns: {.var {missing_columns}}.
      Ensure `df` contains {required_columns} before using this function."
    )
  }

  # Construct the GT table with formatted elements
  gt_df <- suppressWarnings(
    df |>
      # Create a gt table
      gt::gt() |>

      # Add title and subtitle
      gt::tab_header(
        title = gt::md(paste0(
          fontawesome::fa("truck-medical"),
          glue::glue(" **Iowa NEMSQA {measure} Filter Process**")
        )),
        subtitle = gt::md(
          glue::glue(
            "Source: Iowa ImageTrend Elite Registry || Years: 2021-2024"
          )
        )
      ) |>

      # Format all numeric columns (except "Populations") as integers
      gt::fmt_integer(columns = -Populations) |>

      # Add a sparkline visualization for population trends
      gtExtras::gt_plt_sparkline(
        column = `Population Trend`,
        # Use reference mean to standardize visualization
        type = "ref_mean",
        # Set sparkline colors
        palette = c("#70C8B8", "transparent", "#19405B", "#F27026", "#03617A"),
        same_limit = FALSE,
        # Allow independent scaling of sparklines
        label = FALSE,
        # Display labels on sparklines for clarity
        fig_dim = fig_dim # Dynamic sparkline dimensions
      ) |>

      # Replace missing values in all numeric columns (except "Populations") with "*"
      gt::sub_missing(columns = -Populations, missing_text = "*")
  )

  # Return the formatted GT table
  return(gt_df)
}

#_____________________________________________________________________________
# Function: Format Results Statistical File for GT Table
#_____________________________________________________________________________
# This function formats and processes a results-based statistical dataset
# into a {gt} table with enhanced formatting for reporting.
# It integrates various {gt} and {gtExtras} functionalities, including:
#   - Column transformations
#   - Confidence interval visualization
#   - Percentage formatting
#   - Custom styling using `tab_style_hhs()`
#
# Arguments:
#   - df: A data frame containing statistical results, including columns
#         for numerator, denominator, proportion, confidence intervals, and
#         the incident year for grouping.
#
# Returns:
#   - A {gt} table object with formatted numerical and percentage values,
#     confidence intervals, and appropriate styling.
#_____________________________________________________________________________
results_statistical_file_gt <- function(df, groups) {
  # Validate input: Ensure `df` is a data frame or tibble
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      "The input `df` must be a {.cls data.frame} or {.cls tibble},
      but received an object of class {.cls {class(df)}}."
    )
  }

  # Validate required columns exist
  required_cols <- c(
    glue::glue("{groups}"),
    "measure",
    "prop",
    "lower_ci",
    "upper_ci",
    "numerator",
    "denominator"
  )
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "The input `df` is missing the following required columns: {.val {missing_cols}}.
      Ensure the dataset contains all necessary fields before processing."
    )
  }

  # Get the dynamic measure name
  measure <- unique(df$measure)

  # Transform the data: Pivot, modify labels, and apply small count suppression
  prepared_df <- suppressWarnings(
    df |>
      dplyr::select(-measure) |> # Remove measure column if present
      gt::gt(groupname_col = groups) |> # Group table dynamically

      # Add title and subtitle
      gt::tab_header(
        title = gt::md(paste0(
          fontawesome::fa("truck-medical"),
          glue::glue(" **NEMSQA {measure} Performance: Iowa**")
        )),
        subtitle = gt::md(
          glue::glue("Reporting Years: 2021-2024")
        )
      ) |>

      gt::cols_hide(columns = "prop_label") |> # Hide proportion label column
      gt::cols_label(pop = "") |> # Rename `pop` column (if applicable)
      gtExtras::gt_duplicate_column(prop, dupe_name = "Comparison") |> # Duplicate `prop` for comparison
      gtExtras::gt_plt_conf_int(
        column = Comparison,
        # Apply confidence interval plot to comparison column
        ci_columns = c(lower_ci, upper_ci),
        text_size = 0 # Hide text in plot for cleaner display
      ) |>
      gt::fmt_number(
        columns = gt::matches("numer|denom"),
        # Format numerator and denominator
        drop_trailing_zeros = TRUE,
        drop_trailing_dec_mark = TRUE
      ) |>
      gt::fmt_percent(columns = c(prop, matches("_ci")), decimals = 1) |> # Format proportions and confidence intervals as percentages
      gt::cols_merge(
        columns = c("prop", "lower_ci", "upper_ci"),
        # Merge confidence interval columns for readability
        pattern = "<<{1}>><< [{2},>><< {3}]>>"
      ) |>
      gt::cols_label(
        numerator = "Numerator",
        denominator = "Denominator",
        prop = "Result [95% CI]"
      ) |>

      # Replace missing values in all numeric columns (except "Populations") with "*"
      gt::sub_missing(columns = numerator:upper_ci, missing_text = "*")
  )

  # Return the formatted {gt} table
  return(prepared_df)
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
tab_style_hhs <- function(
  gt_object,
  row_groups = 14,
  column_labels = 14,
  title = 20,
  subtitle = 18,
  spanners = 16,
  body = 14,
  source_note = 12,
  footnote = 12,
  message_text,
  row_group_fill = "#E0A624",
  row_group_fill_alpha = 0.5,
  bold_first_col = 1,
  border_cols,
  border_color1 = "#19405B",
  border_color2 = "#70C8B8"
) {
  out <- gt_object |>

    # Set the font for the table
    gt::opt_table_font(
      font = "Work Sans",
      stack = NULL,
      weight = NULL,
      style = NULL,
      add = TRUE
    ) |>

    # Style the stub (row names) section
    gt::tab_style(
      locations = gt::cells_stub(),
      style = gt::cell_text(
        size = gt::px(body),
        font = "Work Sans SemiBold",
        color = "black",
        align = "left"
      )
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
    ) |>

    # Apply background color to row groups
    gt::tab_style(
      style = gt::cell_fill(
        color = row_group_fill,
        alpha = row_group_fill_alpha
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Add top border to row groups
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = border_color1,
        weight = gt::px(3) # Adjust thickness as needed
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
    ) |>

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
    ) |>

    # Style the first column (typically used for labels)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "black",
        size = gt::px(body),
        align = "left"
      ),
      locations = gt::cells_body(columns = {{ bold_first_col }})
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
    ) |>

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
      locations = gt::cells_body(columns = {{ border_cols }}),
      style = gt::cell_borders(
        sides = c("left"),
        weight = gt::px(2),
        color = border_color2
      )
    ) |>

    # Add various source notes with icons from fontawesome
    gt::tab_source_note(
      source_note = gt::md(paste0(
        fontawesome::fa("note-sticky"),
        " ",
        message_text
      ))
    ) |>

    gt::tab_source_note(
      source_note = gt::md(paste0(
        fontawesome::fa("database"),
        " Iowa ImageTrend Elite EMS Registry"
      ))
    ) |>

    # Align all columns except the first one to the center
    gt::cols_align(align = "center", columns = 2)

  return(out)
}

### DATA EXPORT FACILITIES =====================================================

# Export NEMSQA Data to CSV
# This function exports objects from the global environment that match a
# specified pattern. It ensures that only data frames or tibbles are exported
# and organizes them into predefined output folders. Non-data-frame objects are
# skipped with a warning.
export_nemsqa_data <- function(
  pattern,
  measure,
  folder = c("population", "result")
) {
  # Validate folder selection
  folder <- match.arg(folder, choices = c("population", "result"))

  # Get the secure file path
  output_file_path <- Sys.getenv("OUTPUT_FILE_PATH")

  # Construct the output directory path
  output_path <- glue::glue(
    output_file_path,
    "{measure}/{folder}"
  )

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
  cli::cli_text("\n") # Add space for readability

  # Iterate through matched objects
  for (i in objects) {
    data <- get(i, envir = .GlobalEnv) # Retrieve object from global environment

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

  cli::cli_text("\n") # Add space before warnings

  return(invisible(NULL))
}

#_____________________________________________________________________________
# Function: Export Formatted GT Table for NEMSQA Reports
#_____________________________________________________________________________
# This function saves a {gt} table object to a specified file format for reporting.
# It integrates with the previously defined functions that process and format
# population statistical data, ensuring seamless export of the final table.
#
# Arguments:
#   - gt_object: A {gt} table object to be saved.
#   - measure: The measure category (e.g., "asthma_01", "airway_18").
#   - folder: A character string specifying whether the file belongs to the
#             "population" or "result" folder (default: "population").
#   - filename: Optional. The name of the output file. If not provided,
#               the function generates one based on the object name.
#   - path: Optional. The directory where the file should be saved. If not
#           provided, it defaults to the standardized NEMSQA report path.
#   - extension: Optional. The file extension/type. Defaults to ".docx".
#   - ...: Optional. Arguments passed to gt::gtsave(...).
#_____________________________________________________________________________
export_nemsqa_gt <- function(
  gt_object,
  measure,
  folder = c("population", "result"),
  filename = NULL,
  path = NULL,
  extension = NULL,
  ...
) {
  # Validate folder selection
  folder <- match.arg(folder, choices = c("population", "result"))

  # Validate `gt_object` is a gt table
  if (!inherits(gt_object, "gt_tbl")) {
    cli::cli_abort(
      "The input `gt_object` must be a {.cls gt_tbl}, but received an object of class {.cls {class(gt_object)}}.
      Ensure that `gt_object` is created using the {gt} package before exporting."
    )
  }

  # Validate `measure` is a non-empty string
  if (!is.character(measure) || length(measure) != 1 || measure == "") {
    cli::cli_abort(
      "The argument `measure` must be a non-empty character string."
    )
  }

  # Get the secure file path
  output_file_path <- Sys.getenv("OUTPUT_FILE_PATH")

  # Set default output path if not provided
  if (is.null(path)) {
    path <- file.path(
      output_file_path,
      measure,
      folder
    )
  }

  # Ensure the output directory exists
  fs::dir_create(path)

  # Set default file extension if not provided
  if (is.null(extension)) {
    extension <- ".docx"
  }

  # Ensure extension starts with a dot (.)
  if (!grepl("^\\.", extension)) {
    extension <- paste0(".", extension)
  }

  # Set default filename if not provided
  if (is.null(filename)) {
    filename <- paste0(deparse(substitute(gt_object)), extension)
  }

  # Construct full file path
  full_path <- file.path(path, filename)

  # Export the gt table
  gt::gtsave(gt_object, filename = filename, path = path, ...)

  # Confirmation message
  cli::cli_inform(c("v" = "GT table successfully exported: {full_path}"))
}

###_____________________________________________________________________________
# we will:
# - examine calendar years 2021-2024 of EMS data
# - ingest various tables from each data section of NEMSIS to leverage that
#   approach in the `nemsqar` package
# - visualize statistical outputs for clean reporting
###_____________________________________________________________________________
