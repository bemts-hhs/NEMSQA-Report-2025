### IOWA NEMSQA REPORT TRAUMA-08 2025 ------------------------------------

###_____________________________________________________________________________
# this script will contain all reporting calculations for Trauma-08
# use nemsqa_report_prep_2025.R to get critical functions into memory
###_____________________________________________________________________________
# assume that nemsqa_report_prep_2025.R was already ran to load needed packages
# and project-specific custom functions in the project
###_____________________________________________________________________________

### DATA -----------------------------------------------------------------------

# tables imported in alphabetical order
# tables do not need to be loaded again if already in memory

### disposition tables ###########################################################
disposition_2021 <- import_nemsqa_data(table = "disposition", year = 2021)
disposition_2022 <- import_nemsqa_data(table = "disposition", year = 2022)
disposition_2023 <- import_nemsqa_data(table = "disposition", year = 2023)
disposition_2024 <- import_nemsqa_data(table = "disposition", year = 2024)

# bind rows for the disposition table
disposition_rbind <- dplyr::bind_rows(
  disposition_2021,
  disposition_2022,
  disposition_2023,
  disposition_2024
)

# set up the disposition table for manipulations
disposition_table <- disposition_rbind |>
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

### situation tables #############################################################
situation_2021 <- import_nemsqa_data(table = "situation", year = 2021)
situation_2022 <- import_nemsqa_data(table = "situation", year = 2022)
situation_2023 <- import_nemsqa_data(table = "situation", year = 2023)
situation_2024 <- import_nemsqa_data(table = "situation", year = 2024)

# bind rows for the situation table
situation_rbind <- dplyr::bind_rows(
  situation_2021,
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

### Trauma-08 ==================================================================

### trauma-08 populations ######################################################

# over all years 2021-2024
trauma_08_pop <- trauma_08_population(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  disposition_table = disposition_table,
  vitals_table = vitals_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23
)

# population results for 2021-2024
trauma_08_pop_filter_process <- trauma_08_pop$filter_process

# 2021
trauma_08_pop_2021 <- trauma_08_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2021),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  disposition_table = disposition_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23
)

# population results 2021
trauma_08_pop_filter_process_2021 <- trauma_08_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
trauma_08_pop_2022 <- trauma_08_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2022),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  disposition_table = disposition_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23
)

# population results 2022
trauma_08_pop_filter_process_2022 <- trauma_08_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
trauma_08_pop_2023 <- trauma_08_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2023),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  disposition_table = disposition_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23
)

# population results 2023
trauma_08_pop_filter_process_2023 <- trauma_08_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
trauma_08_pop_2024 <- trauma_08_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2024),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  disposition_table = disposition_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23
)

# population results 2024
trauma_08_pop_filter_process_2024 <- trauma_08_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
trauma_08_pop_years <- dplyr::bind_rows(
  trauma_08_pop_filter_process_2021,
  trauma_08_pop_filter_process_2022,
  trauma_08_pop_filter_process_2023,
  trauma_08_pop_filter_process_2024
)

# plot population trends over time
trauma_08_pop_years |>
  plot_nemsqa_pops(
    type = "col",
    wrap_width = 25,
    plot_title = "Trauma-08",
    facets = TRUE,
    vjust_title = 2,
    vjust_subtitle = 1.5
  )

### trauma-08 results ##########################################################

# year
trauma_08_result_year <- nemsqar::trauma_08(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  disposition_table = disposition_table,
  vitals_table = vitals_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = INCIDENT_YEAR
)

# regions and years
trauma_08_result_regions_years <- nemsqar::trauma_08(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  disposition_table = disposition_table,
  vitals_table = vitals_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
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
trauma_08_result_regions <- nemsqar::trauma_08(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  disposition_table = disposition_table,
  vitals_table = vitals_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
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
trauma_08_result_counties <- nemsqar::trauma_08(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::mutate(
      SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21 = factor(
        SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21
      )
    ),
  response_table = response_table,
  situation_table = situation_table,
  disposition_table = disposition_table,
  vitals_table = vitals_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
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
trauma_08_result_overall <- nemsqar::trauma_08(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  disposition_table = disposition_table,
  vitals_table = vitals_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE
)

# services
trauma_08_result_services <- nemsqar::trauma_08(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  disposition_table = disposition_table,
  vitals_table = vitals_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  transport_disposition_col = c(
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30,
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112
  ),
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
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
  pattern = "trauma_08_pop",
  measure = "Trauma-08",
  folder = "population"
)

### results exports ############################################################

export_nemsqa_data(
  pattern = "trauma_08_result",
  measure = "Trauma-08",
  folder = "result"
)
