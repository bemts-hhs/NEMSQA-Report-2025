### IOWA NEMSQA REPORT NUMBERS 2025 ############################################

###_____________________________________________________________________________
# this script will contain all reporting calculations
# use nemsqa_report_data_2025.R to get data into memory
###_____________________________________________________________________________
# report assumes that nemsqa_report_data_2025 was already ran to
# load needed packages in the project and to get data into memory
###_____________________________________________________________________________

###_____________________________________________________________________________
### CALCULATIONS ----
###_____________________________________________________________________________

# projects sorted in alphabetical order

# airway-01 populations ----

# over all years 2021-2024
airway_01_pop <- nemsqar::airway_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table,
                                           response_table = response_table,
                                           arrest_table = arrest_table,
                                           procedures_table = procedures_table,
                                           vitals_table = vitals_table,
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                           evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                                           evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                           eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                           eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                           eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                           eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                                           eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06
                                           )

# population results for 2021-2024
airway_01_pop_filter_process <- airway_01_pop$filter_process

# 2021
airway_01_pop_2021 <- nemsqar::airway_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           arrest_table = arrest_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                           evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                                           evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                           eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                           eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                           eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                           eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                                           eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06
                                           )

# population results 2021
airway_01_pop_2021_filter_process <- airway_01_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
airway_01_pop_2022 <- nemsqar::airway_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           arrest_table = arrest_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                           evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                                           evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                           eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                           eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                           eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                           eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                                           eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06
                                           )

# population results 2022
airway_01_pop_2022_filter_process <- airway_01_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
airway_01_pop_2023 <- nemsqar::airway_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           arrest_table = arrest_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                           evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                                           evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                           eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                           eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                           eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                           eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                                           eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06
                                           )

# population results 2023
airway_01_pop_2023_filter_process <- airway_01_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
airway_01_pop_2024 <- nemsqar::airway_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           arrest_table = arrest_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                           evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                                           evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                           eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                           eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                           eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                           eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                                           eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06
                                           )

# population results 2024
airway_01_pop_2024_filter_process <- airway_01_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-01 populations over the years
airway_01_pop_years <- dplyr::bind_rows(airway_01_pop_2021_filter_process,
                                        airway_01_pop_2022_filter_process,
                                        airway_01_pop_2023_filter_process,
                                        airway_01_pop_2024_filter_process
                                        )

# airway-01 results ----

# year
airway_01_result_year <- nemsqar::airway_01(df = NULL,
                              patient_scene_table = patient_scene_table,
                              response_table = response_table,
                              arrest_table = arrest_table,
                              procedures_table = procedures_table,
                              vitals_table = vitals_table,
                              erecord_01_col = FACT_INCIDENT_PK,
                              incident_date_col = INCIDENT_DATE,
                              patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                              epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                              epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                              earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                              eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                              evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                              evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                              evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                              eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                              eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                              eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                              eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                              eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
                              .by = INCIDENT_YEAR
                              )

# regions and years
airway_01_result_regions_years <- nemsqar::airway_01(df = NULL,
                              patient_scene_table = patient_scene_table,
                              response_table = response_table,
                              arrest_table = arrest_table,
                              procedures_table = procedures_table,
                              vitals_table = vitals_table,
                              erecord_01_col = FACT_INCIDENT_PK,
                              incident_date_col = INCIDENT_DATE,
                              patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                              epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                              epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                              earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                              eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                              evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                              evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                              evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                              eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                              eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                              eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                              eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                              eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
                              .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                              ) |>
    dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
    tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# regions
airway_01_result_regions <- nemsqar::airway_01(df = NULL,
                              patient_scene_table = patient_scene_table,
                              response_table = response_table,
                              arrest_table = arrest_table,
                              procedures_table = procedures_table,
                              vitals_table = vitals_table,
                              erecord_01_col = FACT_INCIDENT_PK,
                              incident_date_col = INCIDENT_DATE,
                              patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                              epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                              epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                              earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                              eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                              evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                              evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                              evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                              eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                              eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                              eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                              eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                              eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06,
                              .by = `Region: Preparedness`
                              ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# overall
airway_01_result_overall <- nemsqar::airway_01(df = NULL,
                              patient_scene_table = patient_scene_table,
                              response_table = response_table,
                              arrest_table = arrest_table,
                              procedures_table = procedures_table,
                              vitals_table = vitals_table,
                              erecord_01_col = FACT_INCIDENT_PK,
                              incident_date_col = INCIDENT_DATE,
                              patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                              epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                              epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                              earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                              eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                              evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                              evitals_06_col = VITALS_SYSTOLIC_BLOOD_PRESSURE_SBP_E_VITALS_06,
                              evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                              eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                              eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                              eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                              eprocedures_05_col = PROCEDURE_NUMBER_OF_ATTEMPTS_E_PROCEDURES_05,
                              eprocedures_06_col = PROCEDURE_SUCCESSFUL_E_PROCEDURES_06
                              )

# airway-05 populations ----

# over all years 2021-2024
airway_05_pop <- nemsqar::airway_05_population(df = NULL,
                                      patient_scene_table = patient_scene_table,
                                      response_table = response_table,
                                      arrest_table = arrest_table,
                                      procedures_table = procedures_table,
                                      vitals_table = vitals_table,
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                      eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                      eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                      eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                      )

# population results for 2021-2024
airway_05_pop_filter_process <- airway_05_pop$filter_process

# 2021
airway_05_pop_2021 <- nemsqar::airway_05_population(df = NULL,
                                      patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                      response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                      arrest_table = arrest_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                      procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                      vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                      eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                      eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                      eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                      )

# population results for 2021
airway_05_pop_filter_process_2021 <- airway_05_pop_2021$filter_process

# 2022
airway_05_pop_2022 <- nemsqar::airway_05_population(df = NULL,
                                      patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                      response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                      arrest_table = arrest_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                      procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                      vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                      eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                      eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                      eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                      )

# population results for 2022
airway_05_pop_filter_process_2022 <- airway_05_pop_2022$filter_process

# 2023
airway_05_pop_2023 <- nemsqar::airway_05_population(df = NULL,
                                      patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                      response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                      arrest_table = arrest_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                      procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                      vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                      eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                      eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                      eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                      )

# population results for 2023
airway_05_pop_filter_process_2023 <- airway_05_pop_2023$filter_process

# 2024
airway_05_pop_2024 <- nemsqar::airway_05_population(df = NULL,
                                      patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                      response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                      arrest_table = arrest_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                      procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                      vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                      eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                      eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                      eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                      )

# population results for 2024
airway_05_pop_filter_process_2024 <- airway_05_pop_2024$filter_process

# airway-01 populations over the years
airway_05_pop_years <- dplyr::bind_rows(airway_05_pop_filter_process_2021,
                                        airway_05_pop_filter_process_2022,
                                        airway_05_pop_filter_process_2023,
                                        airway_05_pop_filter_process_2024
                                        )

# airway-05 results ----

# year
airway_05_result_year <- nemsqar::airway_05(df = NULL,
                                   patient_scene_table = patient_scene_table,
                                   response_table = response_table,
                                   arrest_table = arrest_table,
                                   procedures_table = procedures_table,
                                   vitals_table = vitals_table,
                                   erecord_01_col = UNIQUE_RUN_ID,
                                   incident_date_col = INCIDENT_DATE,
                                   patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                   epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                   epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                   earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                   eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                   evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                   evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                   eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                   eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                   eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                   .by = INCIDENT_YEAR
                                   )

# regions and years
airway_05_result_regions_years <- nemsqar::airway_05(df = NULL,
                                   patient_scene_table = patient_scene_table,
                                   response_table = response_table,
                                   arrest_table = arrest_table,
                                   procedures_table = procedures_table,
                                   vitals_table = vitals_table,
                                   erecord_01_col = FACT_INCIDENT_PK,
                                   incident_date_col = INCIDENT_DATE,
                                   patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                   epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                   epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                   earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                   eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                   evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                   evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                   eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                   eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                   eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                   .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                                   ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# regions
airway_05_result_regions <- nemsqar::airway_05(df = NULL,
                                   patient_scene_table = patient_scene_table,
                                   response_table = response_table,
                                   arrest_table = arrest_table,
                                   procedures_table = procedures_table,
                                   vitals_table = vitals_table,
                                   erecord_01_col = FACT_INCIDENT_PK,
                                   incident_date_col = INCIDENT_DATE,
                                   patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                   epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                   epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                   earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                   eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                   evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                   evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                   eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                   eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                   eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                   .by = `Region: Preparedness`
                                   ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# overall
airway_05_result_overall <- nemsqar::airway_05(df = NULL,
                                   patient_scene_table = patient_scene_table,
                                   response_table = response_table,
                                   arrest_table = arrest_table,
                                   procedures_table = procedures_table,
                                   vitals_table = vitals_table,
                                   erecord_01_col = FACT_INCIDENT_PK,
                                   incident_date_col = INCIDENT_DATE,
                                   patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                   epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                   epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                   earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
                                   eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                   evitals_01_col = VITALS_SIGNS_TAKEN_DATE_TIME_E_VITALS_01,
                                   evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                   eprocedures_01_col = PROCEDURE_PERFORMED_DATE_TIME_E_PROCEDURES_01,
                                   eprocedures_02_col = PROCEDURE_PERFORMED_PRIOR_TO_EMS_CARE_E_PROCEDURES_02,
                                   eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                   )

# airway-18 populations ----

# over all years 2021-2024
airway_18_pop <- airway_18_population(df = NULL,
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
airway_18_pop_2021 <- airway_18_population(df = NULL,
                                      patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
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
airway_18_pop_2022 <- airway_18_population(df = NULL,
                                      patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
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
airway_18_pop_2023 <- airway_18_population(df = NULL,
                                      patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
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
airway_18_pop_2024 <- airway_18_population(df = NULL,
                                      patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
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
airway_18_pop_years <- dplyr::bind_rows(airway_18_pop_filter_process_2021,
                                        airway_18_pop_filter_process_2022,
                                        airway_18_pop_filter_process_2023,
                                        airway_18_pop_filter_process_2024
                                        )

# airway-18 results ----

# year
airway_18_result_year <- nemsqar::airway_18(df = NULL,
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
                                            .by = INCIDENT_YEAR
                                            )

# regions and years
airway_18_result_regions_years <- nemsqar::airway_18(df = NULL,
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
                                                     .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                                                     ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# regions
airway_18_result_regions <- nemsqar::airway_18(df = NULL,
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
                                               .by = `Region: Preparedness`
                                               ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# overall
airway_18_result_overall <- nemsqar::airway_18(df = NULL,
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

# asthma-01 populations ----

# over all years 2021-2024
asthma_01_pop <- asthma_01_population(df = NULL,
                                      patient_scene_table = patient_scene_table,
                                      response_table = response_table,
                                      situation_table = situation_table,
                                      medications_table = medications_table,
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                      esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                      emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                      )

# population results for 2021-2024
asthma_01_pop_filter_process <- asthma_01_pop$filter_process

# 2021
asthma_01_pop_2021 <- asthma_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                           esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                           emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                           )

# population results 2021
asthma_01_pop_filter_process_2021 <- asthma_01_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
asthma_01_pop_2022 <- asthma_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                           esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                           emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                           )

# population results 2022
asthma_01_pop_filter_process_2022 <- asthma_01_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
asthma_01_pop_2023 <- asthma_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                           esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                           emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                           )

# population results 2023
asthma_01_pop_filter_process_2023 <- asthma_01_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
asthma_01_pop_2024 <- asthma_01_population(df = NULL,
                                           patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                           erecord_01_col = FACT_INCIDENT_PK,
                                           incident_date_col = INCIDENT_DATE,
                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                           esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                           esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                           emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                           )

# population results 2024
asthma_01_pop_filter_process_2024 <- asthma_01_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
asthma_01_pop_years <- dplyr::bind_rows(asthma_01_pop_filter_process_2021,
                                        asthma_01_pop_filter_process_2022,
                                        asthma_01_pop_filter_process_2023,
                                        asthma_01_pop_filter_process_2024
                                        )

# asthma-01 results ----

# year
asthma_01_result_year <- nemsqar::asthma_01(df = NULL,
                                            patient_scene_table = patient_scene_table,
                                            response_table = response_table,
                                            situation_table = situation_table,
                                            medications_table = medications_table,
                                            erecord_01_col = FACT_INCIDENT_PK,
                                            incident_date_col = INCIDENT_DATE,
                                            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                            epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                            epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                            eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                            esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                            esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                            emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                            .by = INCIDENT_YEAR
                                            )

# regions and years
asthma_01_result_regions_years <- nemsqar::asthma_01(df = NULL,
                                                     patient_scene_table = patient_scene_table,
                                                     response_table = response_table,
                                                     situation_table = situation_table,
                                                     medications_table = medications_table,
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                     .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                                                     ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# regions
asthma_01_result_regions <- nemsqar::asthma_01(df = NULL,
                                               patient_scene_table = patient_scene_table,
                                               response_table = response_table,
                                               situation_table = situation_table,
                                               medications_table = medications_table,
                                               erecord_01_col = FACT_INCIDENT_PK,
                                               incident_date_col = INCIDENT_DATE,
                                               patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                               epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                               epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                               eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                               esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                               esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                               emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                               .by = `Region: Preparedness`
                                               ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# overall
asthma_01_result_overall <- nemsqar::asthma_01(df = NULL,
                                               patient_scene_table = patient_scene_table,
                                               response_table = response_table,
                                               situation_table = situation_table,
                                               medications_table = medications_table,
                                               erecord_01_col = FACT_INCIDENT_PK,
                                               incident_date_col = INCIDENT_DATE,
                                               patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                               epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                               epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                               eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                               esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                               esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                               emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                               )
