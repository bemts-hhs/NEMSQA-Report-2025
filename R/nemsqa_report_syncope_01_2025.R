### IOWA NEMSQA REPORT SYNCOPE-01 2025 ------------------------------------

###_____________________________________________________________________________
# this script will contain all reporting calculations for Syncope-01
# use nemsqa_report_prep_2025.R to get critical functions into memory
###_____________________________________________________________________________
# assume that nemsqa_report_prep_2025.R was already ran to load needed packages
# and project-specific custom functions in the project
###_____________________________________________________________________________

### DATA -----------------------------------------------------------------------

# tables imported in alphabetical order
# tables do not need to be loaded again if already in memory

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

### Syncope-01 =================================================================

### syncope-01 populations #####################################################

# over all years 2021-2024
syncope_01_pop <- syncope_01_population(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04
)

# population results for 2021-2024
syncope_01_pop_filter_process <- syncope_01_pop$filter_process

# 2021
syncope_01_pop_2021 <- syncope_01_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2021),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04
)

# population results 2021
syncope_01_pop_filter_process_2021 <- syncope_01_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
syncope_01_pop_2022 <- syncope_01_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2022),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04
)

# population results 2022
syncope_01_pop_filter_process_2022 <- syncope_01_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
syncope_01_pop_2023 <- syncope_01_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2023),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04
)

# population results 2023
syncope_01_pop_filter_process_2023 <- syncope_01_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
syncope_01_pop_2024 <- syncope_01_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2024),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04
)

# population results 2024
syncope_01_pop_filter_process_2024 <- syncope_01_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
syncope_01_pop_years <- dplyr::bind_rows(
  syncope_01_pop_filter_process_2021,
  syncope_01_pop_filter_process_2022,
  syncope_01_pop_filter_process_2023,
  syncope_01_pop_filter_process_2024
)

# plot population trends over time
syncope_01_pop_years |>
  plot_nemsqa_pops(
    type = "col",
    wrap_width = 25,
    plot_title = "Syncope-01",
    facets = TRUE,
    vjust_title = 2,
    vjust_subtitle = 1.5
  )

### syncope-01 results #########################################################

# year
syncope_01_result_year <- nemsqar::syncope_01(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = INCIDENT_YEAR
)

# regions and years
syncope_01_result_regions_years <- nemsqar::syncope_01(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04,
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
syncope_01_result_regions <- nemsqar::syncope_01(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04,
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
syncope_01_result_counties <- nemsqar::syncope_01(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::mutate(
      SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21 = factor(
        SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21
      )
    ),
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04,
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
syncope_01_result_overall <- nemsqar::syncope_01(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE
)

# services
syncope_01_result_services <- nemsqar::syncope_01(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
  esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_04_col = VITALS_ECG_TYPE_E_VITALS_04,
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
  pattern = "syncope_01_pop",
  measure = "Syncope-01",
  folder = "population"
)

### results exports ############################################################

export_nemsqa_data(
  pattern = "syncope_01_result",
  measure = "Syncope-01",
  folder = "result"
)
