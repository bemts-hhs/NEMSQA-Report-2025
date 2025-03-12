### IOWA NEMSQA REPORT NUMBERS 2025 --------------------------------------------

###_____________________________________________________________________________
# this script will contain all reporting calculations
# use nemsqa_report_data_2025.R to get data into memory
###_____________________________________________________________________________
# report assumes that nemsqa_report_data_2025 was already ran to
# load needed packages in the project and to get data into memory
###_____________________________________________________________________________

### CALCULATIONS ---------------------------------------------------------------

# projects sorted in alphabetical order

### Airway-01 ==================================================================

### airway-01 populations ########################################################

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

# plot population trends over time
airway_01_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 30,
                   plot_title = "Airway-01",
                   vjust_title = 2,
                   vjust_subtitle = 1.5,
                   facets = TRUE
                   )

### airway-01 results ############################################################

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

# get confidence intervals
airway_01_result_year <- airway_01_result_year |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
airway_01_result_regions_years <- airway_01_result_regions_years |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
airway_01_result_overall <- airway_01_result_overall |>
  nemsqa_binomial_confint(method = "wilson")

### Airway-05 ==================================================================

### airway-05 populations ########################################################

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
airway_05_pop_filter_process_2021 <- airway_05_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

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
airway_05_pop_filter_process_2022 <- airway_05_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

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
airway_05_pop_filter_process_2023 <- airway_05_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

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
airway_05_pop_filter_process_2024 <- airway_05_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-01 populations over the years
airway_05_pop_years <- dplyr::bind_rows(airway_05_pop_filter_process_2021,
                                        airway_05_pop_filter_process_2022,
                                        airway_05_pop_filter_process_2023,
                                        airway_05_pop_filter_process_2024
                                        )

# plot population trends over time
airway_05_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 25,
                   plot_title = "Airway-05",
                   facets = TRUE,
                   vjust_title = 2,
                   vjust_subtitle = 1.5
                   )

### airway-05 results ############################################################

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

# get confidence intervals
airway_05_result_year <- airway_05_result_year |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
airway_05_result_regions_years <- airway_05_result_regions_years |>
  nemsqa_binomial_confint(method = "wilson")

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

# get the confidence intervals
airway_05_result_regions <- airway_05_result_regions |>
  nemsqa_binomial_confint(method = "wilson")

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

# get the confidence intervals
airway_05_result_overall <- airway_05_result_overall |>
  nemsqa_binomial_confint(method = "wilson")

### Airway-18 ==================================================================

### airway-18 populations ########################################################

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

# plot population trends over time
airway_18_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 25,
                   plot_title = "Airway-18",
                   facets = TRUE,
                   vjust_title = 2,
                   vjust_subtitle = 1.5
                   )

### airway-18 results ############################################################

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

# get confidence intervals
airway_18_result_year <- airway_18_result_year |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
airway_18_result_regions_years <- airway_18_result_regions_years |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
airway_18_result_regions <- airway_18_result_regions |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
airway_18_result_overall <- airway_18_result_overall |>
  nemsqa_binomial_confint(method = "wilson")

### Asthma-01 ==================================================================

### asthma-01 populations ########################################################

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

# plot population trends over time
asthma_01_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 25,
                   plot_title = "Asthma-01",
                   facets = TRUE,
                   vjust_title = 2,
                   vjust_subtitle = 1.5
                   )

### asthma-01 results ############################################################

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

# get confidence intervals
asthma_01_result_year <- asthma_01_result_year |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
asthma_01_result_regions_years <- asthma_01_result_regions_years |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
asthma_01_result_regions <- asthma_01_result_regions |>
  nemsqa_binomial_confint(method = "wilson")

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

# get confidence intervals
asthma_01_result_overall <- asthma_01_result_overall |>
  nemsqa_binomial_confint(method = "wilson")

### Hypoglycemia-01 ============================================================

### hypoglycemia-01 populations ########################################################

# over all years 2021-2024
hypoglycemia_01_pop <- hypoglycemia_01_population(df = NULL,
                                      patient_scene_table = patient_scene_table,
                                      response_table = response_table,
                                      situation_table = situation_table,
                                      medications_table = medications_table,
                                      vitals_table = vitals_table,
                                      procedures_table = procedures_table,
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                      esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                      evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                      evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                      evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                      emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                      eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                      )

# population results for 2021-2024
hypoglycemia_01_pop_filter_process <- hypoglycemia_01_pop$filter_process

# 2021
hypoglycemia_01_pop_2021 <- hypoglycemia_01_population(df = NULL,
                                                       patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                       response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                       situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                       medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                       vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                       procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                       erecord_01_col = FACT_INCIDENT_PK,
                                                       incident_date_col = INCIDENT_DATE,
                                                       patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                       epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                       epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                       eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                       esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                       esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                       evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                                       evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                                       evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                                       emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                       eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                                       )

# population results 2021
hypoglycemia_01_pop_filter_process_2021 <- hypoglycemia_01_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
hypoglycemia_01_pop_2022 <- hypoglycemia_01_population(df = NULL,
                                                       patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                       response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                       situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                       medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                       vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                       procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                       erecord_01_col = FACT_INCIDENT_PK,
                                                       incident_date_col = INCIDENT_DATE,
                                                       patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                       epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                       epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                       eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                       esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                       esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                       evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                                       evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                                       evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                                       emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                       eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                                       )

# population results 2022
hypoglycemia_01_pop_filter_process_2022 <- hypoglycemia_01_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
hypoglycemia_01_pop_2023 <- hypoglycemia_01_population(df = NULL,
                                                       patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                       response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                       situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                       medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                       vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                       procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                       erecord_01_col = FACT_INCIDENT_PK,
                                                       incident_date_col = INCIDENT_DATE,
                                                       patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                       epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                       epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                       eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                       esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                       esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                       evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                                       evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                                       evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                                       emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                       eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                                       )

# population results 2023
hypoglycemia_01_pop_filter_process_2023 <- hypoglycemia_01_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
hypoglycemia_01_pop_2024 <- hypoglycemia_01_population(df = NULL,
                                                       patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                       response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                       situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                       medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                       vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                       procedures_table = procedures_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                       erecord_01_col = FACT_INCIDENT_PK,
                                                       incident_date_col = INCIDENT_DATE,
                                                       patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                       epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                       epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                       eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                       esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                       esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                       evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                                       evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                                       evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                                       emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                       eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                                       )

# population results 2024
hypoglycemia_01_pop_filter_process_2024 <- hypoglycemia_01_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
hypoglycemia_01_pop_years <- dplyr::bind_rows(hypoglycemia_01_pop_filter_process_2021,
                                        hypoglycemia_01_pop_filter_process_2022,
                                        hypoglycemia_01_pop_filter_process_2023,
                                        hypoglycemia_01_pop_filter_process_2024
                                        )

# plot population trends over time
hypoglycemia_01_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 25,
                   plot_title = "Hypoglycemia-01",
                   facets = TRUE,
                   vjust_title = 2,
                   vjust_subtitle = 1.5
                   )

### hypoglycemia-01 results ####################################################

# year
hypoglycemia_01_result_year <- nemsqar::hypoglycemia_01(df = NULL,
                                                        patient_scene_table = patient_scene_table,
                                                        response_table = response_table,
                                                        situation_table = situation_table,
                                                        medications_table = medications_table,
                                                        vitals_table = vitals_table,
                                                        procedures_table = procedures_table,
                                                        erecord_01_col = FACT_INCIDENT_PK,
                                                        incident_date_col = INCIDENT_DATE,
                                                        patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                        epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                        epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                        eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                        esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                        esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                        evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                                        evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                                        evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                                        emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                        eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                                        .by = INCIDENT_YEAR
                                                        )

# get confidence intervals
hypoglycemia_01_result_year <- hypoglycemia_01_result_year |>
  nemsqa_binomial_confint(method = "wilson")

# regions and years
hypoglycemia_01_result_regions_years <- nemsqar::hypoglycemia_01(df = NULL,
                                                                 patient_scene_table = patient_scene_table,
                                                                 response_table = response_table,
                                                                 situation_table = situation_table,
                                                                 medications_table = medications_table,
                                                                 vitals_table = vitals_table,
                                                                 procedures_table = procedures_table,
                                                                 erecord_01_col = FACT_INCIDENT_PK,
                                                                 incident_date_col = INCIDENT_DATE,
                                                                 patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                                 epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                                 epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                                 eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                                 esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                                 esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                                 evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                                                 evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                                                 evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                                                 emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                                 eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                                                 .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                                                                 ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
hypoglycemia_01_result_regions_years <- hypoglycemia_01_result_regions_years |>
  nemsqa_binomial_confint(method = "wilson")

# regions
hypoglycemia_01_result_regions <- nemsqar::hypoglycemia_01(df = NULL,
                                                           patient_scene_table = patient_scene_table,
                                                           response_table = response_table,
                                                           situation_table = situation_table,
                                                           medications_table = medications_table,
                                                           vitals_table = vitals_table,
                                                           procedures_table = procedures_table,
                                                           erecord_01_col = FACT_INCIDENT_PK,
                                                           incident_date_col = INCIDENT_DATE,
                                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                           esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                           esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                           evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                                           evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                                           evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                                           emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                           eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03,
                                                           .by = `Region: Preparedness`
                                                           ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
hypoglycemia_01_result_regions <- hypoglycemia_01_result_regions |>
  nemsqa_binomial_confint(method = "wilson")

# overall
hypoglycemia_01_result_overall <- nemsqar::hypoglycemia_01(df = NULL,
                                                           patient_scene_table = patient_scene_table,
                                                           response_table = response_table,
                                                           situation_table = situation_table,
                                                           medications_table = medications_table,
                                                           vitals_table = vitals_table,
                                                           procedures_table = procedures_table,
                                                           erecord_01_col = FACT_INCIDENT_PK,
                                                           incident_date_col = INCIDENT_DATE,
                                                           patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                           epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                           epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                           eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                           esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                           esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                           evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                                                           evitals_23_col = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                                                           evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                                                           emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                           eprocedures_03_col = PROCEDURE_PERFORMED_DESCRIPTION_AND_CODE_E_PROCEDURES_03
                                                           )

# get confidence intervals
hypoglycemia_01_result_overall <- hypoglycemia_01_result_overall |>
  nemsqa_binomial_confint(method = "wilson")

### Pediatrics-03b =============================================================

### pediatrics-03b populations #################################################

# over all years 2021-2024
pediatrics_03b_pop <- pediatrics_03b_population(df = NULL,
                                      patient_scene_table = patient_scene_table,
                                      response_table = response_table,
                                      exam_table = exam_table,
                                      medications_table = medications_table,
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                      eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                      emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                      emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04
                                      )

# population results for 2021-2024
pediatrics_03b_pop_filter_process <- pediatrics_03b_pop$filter_process

# 2021
pediatrics_03b_pop_2021 <- pediatrics_03b_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     exam_table = exam_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                                     eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                                     emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                     emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04
                                                     )

# population results 2021
pediatrics_03b_pop_filter_process_2021 <- pediatrics_03b_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
pediatrics_03b_pop_2022 <- pediatrics_03b_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     exam_table = exam_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                                     eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                                     emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                     emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04
                                                     )

# population results 2022
pediatrics_03b_pop_filter_process_2022 <- pediatrics_03b_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
pediatrics_03b_pop_2023 <- pediatrics_03b_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     exam_table = exam_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                                     eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                                     emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                     emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04
                                                     )

# population results 2023
pediatrics_03b_pop_filter_process_2023 <- pediatrics_03b_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
pediatrics_03b_pop_2024 <- pediatrics_03b_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     exam_table = exam_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     medications_table = medications_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                                     eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                                     emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                     emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04
                                                     )

# population results 2024
pediatrics_03b_pop_filter_process_2024 <- pediatrics_03b_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
pediatrics_03b_pop_years <- dplyr::bind_rows(pediatrics_03b_pop_filter_process_2021,
                                        pediatrics_03b_pop_filter_process_2022,
                                        pediatrics_03b_pop_filter_process_2023,
                                        pediatrics_03b_pop_filter_process_2024
                                        )

# plot population trends over time
pediatrics_03b_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 25,
                   plot_title = "Pediatrics-03b",
                   facets = TRUE,
                   vjust_title = 2,
                   vjust_subtitle = 1.5
                   )

### pediatrics-03b results #####################################################

# year
pediatrics_03b_result_year <- nemsqar::pediatrics_03b(df = NULL,
                                                      patient_scene_table = patient_scene_table,
                                                      response_table = response_table,
                                                      exam_table = exam_table,
                                                      medications_table = medications_table,
                                                      erecord_01_col = FACT_INCIDENT_PK,
                                                      incident_date_col = INCIDENT_DATE,
                                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                      eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                                      eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                                      emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                      emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04,
                                                      .by = INCIDENT_YEAR
                                                      )

# get confidence intervals
pediatrics_03b_result_year <- pediatrics_03b_result_year |>
  nemsqa_binomial_confint(method = "wilson")

# regions and years
pediatrics_03b_result_regions_years <- nemsqar::pediatrics_03b(df = NULL,
                                                               patient_scene_table = patient_scene_table,
                                                               response_table = response_table,
                                                               exam_table = exam_table,
                                                               medications_table = medications_table,
                                                               erecord_01_col = FACT_INCIDENT_PK,
                                                               incident_date_col = INCIDENT_DATE,
                                                               patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                               epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                               epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                               eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                               eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                                               eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                                               emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                               emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04,
                                                               .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                                                               ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
pediatrics_03b_result_regions_years <- pediatrics_03b_result_regions_years |>
  nemsqa_binomial_confint(method = "wilson")

# regions
pediatrics_03b_result_regions <- nemsqar::pediatrics_03b(df = NULL,
                                                         patient_scene_table = patient_scene_table,
                                                         response_table = response_table,
                                                         exam_table = exam_table,
                                                         medications_table = medications_table,
                                                         erecord_01_col = FACT_INCIDENT_PK,
                                                         incident_date_col = INCIDENT_DATE,
                                                         patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                         epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                         epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                         eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                         eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                                         eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                                         emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                         emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04,
                                                         .by = `Region: Preparedness`
                                                         ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
pediatrics_03b_result_regions <- pediatrics_03b_result_regions |>
  nemsqa_binomial_confint(method = "wilson")

# overall
pediatrics_03b_result_overall <- nemsqar::pediatrics_03b(df = NULL,
                                                         patient_scene_table = patient_scene_table,
                                                         response_table = response_table,
                                                         exam_table = exam_table,
                                                         medications_table = medications_table,
                                                         erecord_01_col = FACT_INCIDENT_PK,
                                                         incident_date_col = INCIDENT_DATE,
                                                         patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                         epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                         epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                         eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                         eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
                                                         eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
                                                         emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                         emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04
                                                         )

# get confidence intervals
pediatrics_03b_result_overall <- pediatrics_03b_result_overall |>
  nemsqa_binomial_confint(method = "wilson")

### Respiratory-01 =============================================================

### respiratory-01 populations #################################################

# over all years 2021-2024
respiratory_01_pop <- respiratory_01_population(df = NULL,
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
                                      esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                      esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                      evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                      )

# population results for 2021-2024
respiratory_01_pop_filter_process <- respiratory_01_pop$filter_process

# 2021
respiratory_01_pop_2021 <- respiratory_01_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                     evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                     )

# population results 2021
respiratory_01_pop_filter_process_2021 <- respiratory_01_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
respiratory_01_pop_2022 <- respiratory_01_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                     evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                     )

# population results 2022
respiratory_01_pop_filter_process_2022 <- respiratory_01_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
respiratory_01_pop_2023 <- respiratory_01_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                     evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                     )

# population results 2023
respiratory_01_pop_filter_process_2023 <- respiratory_01_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
respiratory_01_pop_2024 <- respiratory_01_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                     evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                     )

# population results 2024
respiratory_01_pop_filter_process_2024 <- respiratory_01_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
respiratory_01_pop_years <- dplyr::bind_rows(respiratory_01_pop_filter_process_2021,
                                        respiratory_01_pop_filter_process_2022,
                                        respiratory_01_pop_filter_process_2023,
                                        respiratory_01_pop_filter_process_2024
                                        )

# plot population trends over time
respiratory_01_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 25,
                   plot_title = "Respiratory-01",
                   facets = TRUE,
                   vjust_title = 2,
                   vjust_subtitle = 1.5
                   )

### respiratory-01 results #####################################################

# year
respiratory_01_result_year <- nemsqar::respiratory_01(df = NULL,
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
                                                      esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                      esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                      evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
                                                      .by = INCIDENT_YEAR
                                                      )

# get confidence intervals
respiratory_01_result_year <- respiratory_01_result_year |>
  nemsqa_binomial_confint(method = "wilson")

# regions and years
respiratory_01_result_regions_years <- nemsqar::respiratory_01(df = NULL,
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
                                                               esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                               esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                               evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                               evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
                                                               .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                                                               ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
respiratory_01_result_regions_years <- respiratory_01_result_regions_years |>
  nemsqa_binomial_confint(method = "wilson")

# regions
respiratory_01_result_regions <- nemsqar::respiratory_01(df = NULL,
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
                                                         esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                         esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                         evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                         evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
                                                         .by = `Region: Preparedness`
                                                         ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
respiratory_01_result_regions <- respiratory_01_result_regions |>
  nemsqa_binomial_confint(method = "wilson")

# overall
respiratory_01_result_overall <- nemsqar::respiratory_01(df = NULL,
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
                                                         esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                         esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                         evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                         evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                         )

# get confidence intervals
respiratory_01_result_overall <- respiratory_01_result_overall |>
  nemsqa_binomial_confint(method = "wilson")

### Respiratory-02 =============================================================

### respirator-02 populations ##################################################

# over all years 2021-2024
respiratory_02_pop <- respiratory_02_population(df = NULL,
                                      patient_scene_table = patient_scene_table,
                                      response_table = response_table,
                                      vitals_table = vitals_table,
                                      medications_table = medications_table,
                                      procedures_table = procedures_table,
                                      erecord_01_col = FACT_INCIDENT_PK,
                                      incident_date_col = INCIDENT_DATE,
                                      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                      evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                      )

# population results for 2021-2024
respiratory_02_pop_filter_process <- respiratory_02_pop$filter_process

# 2021
respiratory_02_pop_2021 <- respiratory_02_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                     evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                     )

# population results 2021
respiratory_02_pop_filter_process_2021 <- respiratory_02_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
respiratory_02_pop_2022 <- respiratory_02_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                     evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                     )

# population results 2022
respiratory_02_pop_filter_process_2022 <- respiratory_02_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
respiratory_02_pop_2023 <- respiratory_02_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                     evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                     )

# population results 2023
respiratory_02_pop_filter_process_2023 <- respiratory_02_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
respiratory_02_pop_2024 <- respiratory_02_population(df = NULL,
                                                     patient_scene_table = patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     response_table = response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     situation_table = situation_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     vitals_table = vitals_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                                     erecord_01_col = FACT_INCIDENT_PK,
                                                     incident_date_col = INCIDENT_DATE,
                                                     patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                     epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                     epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                     eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                     esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                     esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                     evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                     evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                     )

# population results 2024
respiratory_02_pop_filter_process_2024 <- respiratory_02_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
respiratory_02_pop_years <- dplyr::bind_rows(respiratory_02_pop_filter_process_2021,
                                        respiratory_02_pop_filter_process_2022,
                                        respiratory_02_pop_filter_process_2023,
                                        respiratory_02_pop_filter_process_2024
                                        )

# plot population trends over time
respiratory_02_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 25,
                   plot_title = "Respiratory-01",
                   facets = TRUE,
                   vjust_title = 2,
                   vjust_subtitle = 1.5
                   )

### respiratory-02 results #####################################################

# year
respiratory_02_result_year <- nemsqar::respiratory_02(df = NULL,
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
                                                      esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                      esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                      evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                      evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
                                                      .by = INCIDENT_YEAR
                                                      )

# get confidence intervals
respiratory_02_result_year <- respiratory_02_result_year |>
  nemsqa_binomial_confint(method = "wilson")

# regions and years
respiratory_02_result_regions_years <- nemsqar::respiratory_02(df = NULL,
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
                                                               esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                               esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                               evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                               evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
                                                               .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                                                               ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
respiratory_02_result_regions_years <- respiratory_02_result_regions_years |>
  nemsqa_binomial_confint(method = "wilson")

# regions
respiratory_02_result_regions <- nemsqar::respiratory_02(df = NULL,
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
                                                         esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                         esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                         evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                         evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16,
                                                         .by = `Region: Preparedness`
                                                         ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
respiratory_02_result_regions <- respiratory_02_result_regions |>
  nemsqa_binomial_confint(method = "wilson")

# overall
respiratory_02_result_overall <- nemsqar::respiratory_02(df = NULL,
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
                                                         esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                         esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_E_SITUATION_12,
                                                         evitals_12_col = VITALS_PULSE_OXIMETRY_E_VITALS_12,
                                                         evitals_14_col = VITALS_CARBON_DIOXIDE_CO2_E_VITALS_16
                                                         )

# get confidence intervals
respiratory_02_result_overall <- respiratory_02_result_overall |>
  nemsqa_binomial_confint(method = "wilson")


### Safety-01 ==================================================================

### safety-01 populations ######################################################

### safety-01 results ##########################################################
