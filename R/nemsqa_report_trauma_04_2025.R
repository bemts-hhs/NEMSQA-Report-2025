### IOWA NEMSQA REPORT TRAUMA-04 2025 ------------------------------------

###_____________________________________________________________________________
# this script will contain all reporting calculations for Trauma-04
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
exam_table <- dplyr::bind_rows(
  exam_2021_clean,
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
injury_rbind <- dplyr::bind_rows(
  injury_2021,
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
medications_rbind <- dplyr::bind_rows(
  medications_2021,
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

### Trauma-04 ==================================================================

### trauma-04 populations ######################################################

# over all years 2021-2024
trauma_04_pop <- nemsqar::trauma_04_population(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  exam_table = exam_table,
  procedures_table = procedures_table,
  injury_table = injury_table,
  disposition_table = disposition_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09
)

# population results for 2021-2024
trauma_04_pop_filter_process <- trauma_04_pop$filter_process

# 2021
trauma_04_pop_2021 <- nemsqar::trauma_04_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2021),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  exam_table = exam_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  injury_table = injury_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  disposition_table = disposition_table |> dplyr::filter(INCIDENT_YEAR == 2021),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09
)

# population results 2021
trauma_04_pop_filter_process_2021 <- trauma_04_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
trauma_04_pop_2022 <- nemsqar::trauma_04_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2022),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  exam_table = exam_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  injury_table = injury_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  disposition_table = disposition_table |> dplyr::filter(INCIDENT_YEAR == 2022),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09
)

# population results 2022
trauma_04_pop_filter_process_2022 <- trauma_04_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
trauma_04_pop_2023 <- nemsqar::trauma_04_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2023),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  exam_table = exam_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  injury_table = injury_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  disposition_table = disposition_table |> dplyr::filter(INCIDENT_YEAR == 2023),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09
)

# population results 2023
trauma_04_pop_filter_process_2023 <- trauma_04_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
trauma_04_pop_2024 <- nemsqar::trauma_04_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2024),
  response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  exam_table = exam_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  injury_table = injury_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  disposition_table = disposition_table |> dplyr::filter(INCIDENT_YEAR == 2024),
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09
)

# population results 2024
trauma_04_pop_filter_process_2024 <- trauma_04_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
trauma_04_pop_years <- dplyr::bind_rows(
  trauma_04_pop_filter_process_2021,
  trauma_04_pop_filter_process_2022,
  trauma_04_pop_filter_process_2023,
  trauma_04_pop_filter_process_2024
)

# plot population trends over time
trauma_04_pop_years |>
  plot_nemsqa_pops(
    type = "col",
    wrap_width = 25,
    plot_title = "Trauma-04",
    facets = TRUE,
    vjust_title = 2,
    vjust_subtitle = 1.5
  )

### trauma-04 results ##########################################################

# year
trauma_04_result_year <- nemsqar::trauma_04(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  exam_table = exam_table,
  procedures_table = procedures_table,
  injury_table = injury_table,
  disposition_table = disposition_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = INCIDENT_YEAR
)

# regions and years
trauma_04_result_regions_years <- nemsqar::trauma_04(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  exam_table = exam_table,
  procedures_table = procedures_table,
  injury_table = injury_table,
  disposition_table = disposition_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09,
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
trauma_04_result_regions <- nemsqar::trauma_04(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  exam_table = exam_table,
  procedures_table = procedures_table,
  injury_table = injury_table,
  disposition_table = disposition_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09,
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
trauma_04_result_counties <- nemsqar::trauma_04(
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
  exam_table = exam_table,
  procedures_table = procedures_table,
  injury_table = injury_table,
  disposition_table = disposition_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09,
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
trauma_04_result_overall <- nemsqar::trauma_04(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  exam_table = exam_table,
  procedures_table = procedures_table,
  injury_table = injury_table,
  disposition_table = disposition_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09,
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE
)

# regions and years
trauma_04_result_regions_years <- nemsqar::trauma_04(
  df = NULL,
  patient_scene_table = patient_scene_table,
  response_table = response_table,
  situation_table = situation_table,
  vitals_table = vitals_table,
  exam_table = exam_table,
  procedures_table = procedures_table,
  injury_table = injury_table,
  disposition_table = disposition_table,
  erecord_01_col = FACT_INCIDENT_PK,
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
  esituation_02_col = SITUATION_POSSIBLE_INJURY_WITH_CODE_E_SITUATION_02,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  eresponse_10_col = RESPONSE_TYPE_OF_SCENE_DELAY_LIST_E_RESPONSE_10,
  transport_disposition_col = c(
    DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112,
    TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30
  ),
  edisposition_23_col = HOSPITAL_CAPABILITY_LIST_3_4_IT_DISPOSITION_105_3_5_E_DISPOSITION_23,
  evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
  evitals_10_col = VITALS_HEART_RATE_E_VITALS_10,
  evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = VITALS_RESPIRATORY_RATE_E_VITALS_14,
  evitals_15_col = VITALS_RESPIRATORY_EFFORT_E_VITALS_15,
  evitals_21_col = VITALS_GLASGOW_COMA_SCORE_GCS_MOTOR_E_VITALS_21,
  eexam_16_col = PATIENT_EXTREMITY_ASSESSMENT_FINDINGS_LIST_E_EXAM_16,
  eexam_20_col = PATIENT_NEUROLOGICAL_ASSESSMENT_FINDINGS_LIST_E_EXAM_20,
  eexam_23_col = PATIENT_LUNG_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_100_3_5_E_EXAM_23,
  eexam_25_col = PATIENT_CHEST_EXCLUSIVE_ASSESSMENT_FINDINGS_LIST_3_4_IT_EXAM_102_3_5_E_EXAM_25,
  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
  einjury_01_col = INJURY_CAUSE_OF_INJURY_DESCRIPTION_AND_CODE_LIST_E_INJURY_01,
  einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
  einjury_04_col = INJURY_VEHICULAR_PEDESTRIAN_OR_OTHER_INJURY_RISK_FACTOR_TRIAGE_CRITERIA_STEPS_3_AND_4_LIST_E_INJURY_04,
  einjury_09_col = INJURY_HEIGHT_OF_FALL_IN_FEET_E_INJURY_09,
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
  pattern = "trauma_04_pop",
  measure = "Trauma-04",
  folder = "population"
)

### results exports ############################################################

export_nemsqa_data(
  pattern = "trauma_04_result",
  measure = "Trauma-04",
  folder = "result"
)
