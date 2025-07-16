### IOWA NEMSQA REPORT AIRWAY-18 2025 ------------------------------------------

###_____________________________________________________________________________
# this script will contain all reporting calculations for Airway-18
# use nemsqa_report_prep_2025.R to get critical functions into memory
###_____________________________________________________________________________
# assume that nemsqa_report_prep_2025.R was already ran to load needed packages
# and project-specific custom functions in the project
###_____________________________________________________________________________

### DATA -----------------------------------------------------------------------

# tables imported in alphabetical order
# tables do not need to be loaded again if already in memory

### airway tables ################################################################
airway_2021 <- import_nemsqa_data(table = "airway", year = 2021)
airway_2022 <- import_nemsqa_data(table = "airway", year = 2022)
airway_2023 <- import_nemsqa_data(table = "airway", year = 2023)
airway_2024 <- import_nemsqa_data(table = "airway", year = 2024)

# bind rows for the airway table
airway_rbind <- dplyr::bind_rows(
  airway_2021,
  airway_2022,
  airway_2023,
  airway_2024
)

# set up airway table for manipulations
airway_table <- airway_rbind |>
  clean_names_dates_data()

### patient/scene tables #########################################################
# given that patient and scene data are 1-1 relationship, join those tables
patient_scene_2021 <- import_nemsqa_data(table = "patient_scene", year = 2021)
patient_scene_2022 <- import_nemsqa_data(table = "patient_scene", year = 2022)
patient_scene_2023 <- import_nemsqa_data(table = "patient_scene", year = 2023)
patient_scene_2024 <- import_nemsqa_data(table = "patient_scene", year = 2024)

# bind rows for the patient/scene table
patient_scene_rbind <- dplyr::bind_rows(
  patient_scene_2021,
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
  dplyr::left_join(
    zipcodes,
    by = c("SCENE_INCIDENT_POSTAL_CODE_E_SCENE_19" = "new_zipcode")
  ) |>
  dplyr::left_join(
    location,
    by = c("SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21" = "County")
  ) |>
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
  clean_county_names_1(
    county_column = SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21,
    city_column = SCENE_INCIDENT_CITY_NAME_E_SCENE_17,
    zip_column = SCENE_INCIDENT_POSTAL_CODE_E_SCENE_19
  ) |>
  clean_county_names_1(
    county_column = PATIENT_HOME_COUNTY_NAME_E_PATIENT_07,
    city_column = PATIENT_HOME_CITY_NAME_E_PATIENT_06,
    zip_column = PATIENT_HOME_POSTAL_CODE_E_PATIENT_09
  ) |>
  clean_county_names_2(
    county_column = SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21,
    zip_column = SCENE_INCIDENT_POSTAL_CODE_E_SCENE_19
  ) |>
  clean_county_names_2(
    county_column = PATIENT_HOME_COUNTY_NAME_E_PATIENT_07,
    zip_column = PATIENT_HOME_POSTAL_CODE_E_PATIENT_09
  ) |>
  fix_county_region(
    city_col = SCENE_INCIDENT_CITY_NAME_E_SCENE_17,
    county_col = SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21,
    region_col = `Region: Preparedness`,
    external_city = Iowa_Data_Final$name_city,
    external_county = county_data$County,
    external_region = county_data$`Region: Preparedness`
  )


### procedures tables ############################################################
procedures_2021 <- import_nemsqa_data(table = "procedures", year = 2021)
procedures_2022 <- import_nemsqa_data(table = "procedures", year = 2022)
procedures_2023 <- import_nemsqa_data(table = "procedures", year = 2023)
procedures_2024 <- import_nemsqa_data(table = "procedures", year = 2024)

# bind rows for the procedures table
procedures_rbind <- dplyr::bind_rows(
  procedures_2021,
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
response_rbind <- dplyr::bind_rows(
  response_2021,
  response_2022,
  response_2023,
  response_2024
)

# set up response table for manipulations
response_table <- response_rbind |>
  clean_names_dates_data()

### vitals tables ################################################################
vitals_2021 <- import_nemsqa_data(table = "vitals", year = 2021)
vitals_2022 <- import_nemsqa_data(table = "vitals", year = 2022)
vitals_2023 <- import_nemsqa_data(table = "vitals", year = 2023)
vitals_2024 <- import_nemsqa_data(table = "vitals", year = 2024)

# bind rows for the vitals table
vitals_rbind <- dplyr::bind_rows(
  vitals_2021,
  vitals_2022,
  vitals_2023,
  vitals_2024
)

# set up vitals table for manipulations
vitals_table <- vitals_rbind |>
  clean_names_dates_data()

### CALCULATIONS ---------------------------------------------------------------

### Airway-18 ==================================================================

### airway-18 populations ########################################################

# over all years 2021-2024
airway_18_pop <- airway_18_population(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  procedures_table = procedures_table,
  vitals_table = vitals_table,
  airway_table = airway_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
)

# population results for 2021-2024
airway_18_pop_filter_process <- airway_18_pop$filter_process

# 2021
airway_18_pop_2021 <- airway_18_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2021),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  airway_table = airway_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
)

# population results 2021
airway_18_pop_filter_process_2021 <- airway_18_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
airway_18_pop_2022 <- airway_18_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2022),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  airway_table = airway_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
)

# population results 2022
airway_18_pop_filter_process_2022 <- airway_18_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
airway_18_pop_2023 <- airway_18_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2023),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  airway_table = airway_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
)

# population results 2023
airway_18_pop_filter_process_2023 <- airway_18_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
airway_18_pop_2024 <- airway_18_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2024),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  airway_table = airway_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
)

# population results 2024
airway_18_pop_filter_process_2024 <- airway_18_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
airway_18_pop_years <- dplyr::bind_rows(
  airway_18_pop_filter_process_2021,
  airway_18_pop_filter_process_2022,
  airway_18_pop_filter_process_2023,
  airway_18_pop_filter_process_2024
)

# plot population trends over time
airway_18_pop_years |>
  plot_nemsqa_pops(
    type = "col",
    wrap_width = 25,
    plot_title = "Airway-18",
    facets = TRUE,
    vjust_title = 2,
    vjust_subtitle = 1.5
  )

### airway-18 results ############################################################

# year
airway_18_result_year <- nemsqar::airway_18(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  procedures_table = procedures_table,
  vitals_table = vitals_table,
  airway_table = airway_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = INCIDENT_YEAR
)

# regions and years
airway_18_result_regions_years <- nemsqar::airway_18(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  procedures_table = procedures_table,
  vitals_table = vitals_table,
  airway_table = airway_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = c(INCIDENT_YEAR, `Region: Preparedness`)
) |>
  dplyr::mutate(
    `Region: Preparedness` = dplyr::if_else(
      is.na(`Region: Preparedness`),
      "Missing",
      `Region: Preparedness`
    )
  ) |>
  tidyr::complete(
    INCIDENT_YEAR,
    `Region: Preparedness`,
    measure,
    pop,
    fill = list(
      numerator = 0,
      denominator = 0,
      prop = NA_real_,
      prop_label = NA_character_,
      lower_ci = NA_real_,
      upper_ci = NA_real_
    )
  )

# regions
airway_18_result_regions <- nemsqar::airway_18(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  procedures_table = procedures_table,
  vitals_table = vitals_table,
  airway_table = airway_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = `Region: Preparedness`
) |>
  dplyr::mutate(
    `Region: Preparedness` = dplyr::if_else(
      is.na(`Region: Preparedness`),
      "Missing",
      `Region: Preparedness`
    )
  ) |>
  tidyr::complete(
    `Region: Preparedness`,
    measure,
    pop,
    fill = list(
      numerator = 0,
      denominator = 0,
      prop = NA_real_,
      prop_label = NA_character_,
      lower_ci = NA_real_,
      upper_ci = NA_real_
    )
  )

# counties
airway_18_result_counties <- nemsqar::airway_18(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::mutate(
      SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21 = factor(
        SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21
      )
    ),
  response_table = response_table,
  procedures_table = procedures_table,
  vitals_table = vitals_table,
  airway_table = airway_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21
) |>
  tidyr::complete(
    SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21,
    measure,
    pop,
    fill = list(
      numerator = 0,
      denominator = 0,
      prop = NA_real_,
      prop_label = NA_character_,
      lower_ci = NA_real_,
      upper_ci = NA_real_
    )
  )

# overall
airway_18_result_overall <- nemsqar::airway_18(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  procedures_table = procedures_table,
  vitals_table = vitals_table,
  airway_table = airway_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE
)

# services
airway_18_result_services <- nemsqar::airway_18(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  procedures_table = procedures_table,
  vitals_table = vitals_table,
  airway_table = airway_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
  eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
  eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
  eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
  eairway_02_col = AIRWAY_DEVICE_PLACEMENT_CONFIRMATION_DATE_TIME_E_AIRWAY_02,
  eairway_04_col = PATIENT_AIRWAY_DEVICE_PLACEMENT_CONFIRMED_METHOD_LIST_E_AIRWAY_04,
  evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
  evitals_16_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = c(INCIDENT_YEAR, AGENCY_NAME_D_AGENCY_03)
) |>
  tidyr::complete(
    INCIDENT_YEAR,
    AGENCY_NAME_D_AGENCY_03,
    measure,
    pop,
    fill = list(
      numerator = 0,
      denominator = 0,
      prop = NA_real_,
      prop_label = NA_character_,
      lower_ci = NA_real_,
      upper_ci = NA_real_
    )
  )

### EXPORT =====================================================================

### population exports #########################################################

export_nemsqa_data(
  pattern = "airway_18_pop",
  measure = "Airway-18",
  folder = "population"
)

### results exports ############################################################

export_nemsqa_data(
  pattern = "airway_18_result",
  measure = "Airway-18",
  folder = "result"
)
