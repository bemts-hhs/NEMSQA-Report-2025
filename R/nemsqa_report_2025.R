### IOWA NEMSQAR REPORT 2025 ###################################################

###_____________________________________________________________________________
### PACKAGES ----
###_____________________________________________________________________________

# CRAN versions ----
# install.packages(c("tidyverse", "traumar", "devtools", "remotes", "janitor"))

# dev version of `nemsqar` ----
# remotes::install_github("bemts-hhs/nemsqar")

# load packages ----
library(tidyverse)
library(traumar)
library(devtools)
library(remotes)
library(nemsqar)
library(janitor)

###_____________________________________________________________________________
### DATA ----
###_____________________________________________________________________________

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

      across(matches("date(?!.*time)", perl = TRUE), ~ lubridate::mdy(
        stringr::str_remove_all(string = ., pattern = "\\s\\d+:\\d+(?::\\d+)?\\s[AP]M$")
      )),

      across(matches("time"), ~ lubridate::mdy_hms(
        stringr::str_remove_all(string = ., pattern = "\\s[AP]M$")
      )),
      UNIQUE_EPCR_NUMBER = stringr::str_c(INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                          INCIDENT_DATE
                                          )
      ) |>
    dplyr::filter(
      AGENCY_IS_DEMO_SERVICE == FALSE,
      stringr::str_sub(AGENCY_NUMBER_D_AGENCY_02, 1, 1) %in% c("2", "8", "9"),
      nchar(AGENCY_NUMBER_D_AGENCY_02) == 7
    )

  return(cleaned_df)

}

# we will examine calendar years 2021-2024 of EMS data
# we will ingest various tables from each data section of
# NEMSIS to leverage that approach in the `nemsqar` package

# patient/scene tables ----
# given that patient and scene data are 1-1 relationship, join those tables
# use patient/scene table to get a unique run ID in all other tables, so
# run patient/scene tables first, even though it is out of alphabetical order
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
patient_scene_table <- patient_scene_rbind |>
  clean_names_dates() |>
  dplyr::mutate(UNIQUE_RUN_ID = stringr::str_c(AGENCY_NUMBER_D_AGENCY_02,
                                               INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                               INCIDENT_DATE
                                               ))

# create a data.frame object used to join UNIQUE_RUN_ID to all other data.frames
unique_run_ids <- patient_scene_table |>
  dplyr::select(UNIQUE_EPCR_NUMBER,
                UNIQUE_RUN_ID
                )

# airway tables ----
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
  clean_names_dates() |>
  dplyr::left_join(
    unique_run_ids,
    by = dplyr::join_by(UNIQUE_EPCR_NUMBER)
    )

# arrest tables ----

arrest_2021 <- import_nemsqa_data(table = "arrest", year = 2021)
arrest_2022 <- import_nemsqa_data(table = "arrest", year = 2022)
arrest_2023 <- import_nemsqa_data(table = "arrest", year = 2023)
arrest_2024 <- import_nemsqa_data(table = "arrest", year = 2024)

# disposition tables ----

# exam tables ----

# injury tables ----

# medications tables ----

# procedures tables ----

# response tables ----

# situation tables ----

# vitals tables ----
