### IOWA NEMSQAR REPORT 2025 ###################################################

###_____________________________________________________________________________
### PACKAGES ----
###_____________________________________________________________________________

# CRAN versions ----
# install.packages(c("tidyverse", "traumar", "devtools", "remotes", "janitor",
#                    "gt", "gtsummary", "gtExtras", "zipcodeR"))

# dev version of `nemsqar` ----
# remotes::install_github("bemts-hhs/nemsqar")

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

# get a few handy functions
source(file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/Out-of-Hospital-Indicators/clean_county_names_1.r")

source(file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/Out-of-Hospital-Indicators/clean_county_names_2.r")

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

      across(matches("date(?=.*time)", perl = TRUE), ~ lubridate::mdy_hms(
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
    dplyr::filter(
      AGENCY_IS_DEMO_SERVICE == FALSE,
      stringr::str_sub(AGENCY_NUMBER_D_AGENCY_02, 1, 1) %in% c("2", "8", "9"),
      nchar(AGENCY_NUMBER_D_AGENCY_02) == 7
    )

  return(cleaned_df)

}

###_____________________________________________________________________________
# we will examine calendar years 2021-2024 of EMS data
# we will ingest various tables from each data section of
# NEMSIS to leverage that approach in the `nemsqar` package
###_____________________________________________________________________________

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
  clean_names_dates_data()

# arrest tables ----
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

# disposition tables ----
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

# exam tables ----
exam_2021 <- import_nemsqa_data(table = "exam", year = 2021)
exam_2022 <- import_nemsqa_data(table = "exam", year = 2022)
exam_2023 <- import_nemsqa_data(table = "exam", year = 2023)
exam_2024 <- import_nemsqa_data(table = "exam", year = 2024)

# bind rows for the exam table
exam_rbind <- dplyr::bind_cols(exam_2021,
                                 exam_2022,
                                 exam_2023,
                                 exam_2024
                                 )

# set up the exam table for manipulations
exam_table <- exam_rbind |>
  clean_names_dates_data()

# injury tables ----
injury_2021

# medications tables ----

# patient/scene tables ----
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

# procedures tables ----

# response tables ----

# situation tables ----

# vitals tables ----
