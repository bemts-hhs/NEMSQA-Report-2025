### IOWA NEMSQA REPORT SEIZURE-02 2025 ------------------------------------

###_____________________________________________________________________________
# this script will contain all reporting calculations for Seizure-02
# use nemsqa_report_prep_2025.R to get critical functions into memory
###_____________________________________________________________________________
# assume that nemsqa_report_prep_2025.R was already ran to
# load needed packages in the project
###_____________________________________________________________________________

### DATA -----------------------------------------------------------------------

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


# tables imported in alphabetical order

### CALCULATIONS ---------------------------------------------------------------

### Seizure-02 =================================================================

### seizure-02 populations #####################################################

# over all years 2021-2024
seizure_02_pop <- seizure_02_population(df = NULL,
                                        patient_scene_table = seizure_02_patient_scene_table,
                                        response_table = seizure_02_response_table,
                                        situation_table = seizure_02_situation_table,
                                        medications_table = seizure_02_medications_table,
                                        erecord_01_col = FACT_INCIDENT_PK,
                                        incident_date_col = INCIDENT_DATE,
                                        patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                        epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                        epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                        eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                        esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                        esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                        emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                      )

# population results for 2021-2024
seizure_02_pop_filter_process <- seizure_02_pop$filter_process

# 2021
seizure_02_pop_2021 <- seizure_02_population(df = NULL,
                                             patient_scene_table = seizure_02_patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                             response_table = seizure_02_response_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                             situation_table = seizure_02_situation_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                             medications_table = seizure_02_medications_table |> dplyr::filter(INCIDENT_YEAR == 2021),
                                             erecord_01_col = FACT_INCIDENT_PK,
                                             incident_date_col = INCIDENT_DATE,
                                             patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                             epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                             epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                             eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                             esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                             esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                             emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                           )

# population results 2021
seizure_02_pop_filter_process_2021 <- seizure_02_pop_2021$filter_process |>
  dplyr::mutate(YEAR = 2021)

# 2022
seizure_02_pop_2022 <- seizure_02_population(df = NULL,
                                             patient_scene_table = seizure_02_patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                             response_table = seizure_02_response_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                             situation_table = seizure_02_situation_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                             medications_table = seizure_02_medications_table |> dplyr::filter(INCIDENT_YEAR == 2022),
                                             erecord_01_col = FACT_INCIDENT_PK,
                                             incident_date_col = INCIDENT_DATE,
                                             patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                             epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                             epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                             eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                             esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                             esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                             emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                           )

# population results 2022
seizure_02_pop_filter_process_2022 <- seizure_02_pop_2022$filter_process |>
  dplyr::mutate(YEAR = 2022)

# 2023
seizure_02_pop_2023 <- seizure_02_population(df = NULL,
                                             patient_scene_table = seizure_02_patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                             response_table = seizure_02_response_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                             situation_table = seizure_02_situation_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                             medications_table = seizure_02_medications_table |> dplyr::filter(INCIDENT_YEAR == 2023),
                                             erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                             incident_date_col = INCIDENT_DATE,
                                             patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                             epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                             epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                             eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                             esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                             esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                             emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                           )

# population results 2023
seizure_02_pop_filter_process_2023 <- seizure_02_pop_2023$filter_process |>
  dplyr::mutate(YEAR = 2023)

# 2024
seizure_02_pop_2024 <- seizure_02_population(df = NULL,
                                             patient_scene_table = seizure_02_patient_scene_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                             response_table = seizure_02_response_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                             situation_table = seizure_02_situation_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                             medications_table = seizure_02_medications_table |> dplyr::filter(INCIDENT_YEAR == 2024),
                                             erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                             incident_date_col = INCIDENT_DATE,
                                             patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                             epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                             epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                             eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                             esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                             esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                             emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                           )

# population results 2024
seizure_02_pop_filter_process_2024 <- seizure_02_pop_2024$filter_process |>
  dplyr::mutate(YEAR = 2024)

# airway-18 populations over the years
seizure_02_pop_years <- dplyr::bind_rows(seizure_02_pop_filter_process_2021,
                                        seizure_02_pop_filter_process_2022,
                                        seizure_02_pop_filter_process_2023,
                                        seizure_02_pop_filter_process_2024
                                        )

# plot population trends over time
seizure_02_pop_years |>
  plot_nemsqa_pops(type = "col",
                   wrap_width = 25,
                   plot_title = "Seizure-02",
                   facets = TRUE,
                   vjust_title = 2,
                   vjust_subtitle = 1.5
                   )

### seizure-02 results #########################################################

# year
seizure_02_result_year <- nemsqar::seizure_02(df = NULL,
                                              patient_scene_table = seizure_02_patient_scene_table,
                                              response_table = seizure_02_response_table,
                                              situation_table = seizure_02_situation_table,
                                              medications_table = seizure_02_medications_table,
                                              erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                              incident_date_col = INCIDENT_DATE,
                                              patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                              epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                              epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                              eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                              esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                              esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                              emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                            .by = INCIDENT_YEAR
                                            )

# get confidence intervals
seizure_02_result_year <- seizure_02_result_year |>
  nemsqa_binomial_confint(x = numerator, n = denominator, method = "wilson")

# regions and years
seizure_02_result_regions_years <- nemsqar::seizure_02(df = NULL,
                                                       patient_scene_table = seizure_02_patient_scene_table,
                                                       response_table = seizure_02_response_table,
                                                       situation_table = seizure_02_situation_table,
                                                       medications_table = seizure_02_medications_table,
                                                       erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                                       incident_date_col = INCIDENT_DATE,
                                                       patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                       epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                       epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                       eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                       esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                       esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                                       emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                                     .by = c(INCIDENT_YEAR, `Region: Preparedness`)
                                                     ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(INCIDENT_YEAR, `Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
seizure_02_result_regions_years <- seizure_02_result_regions_years |>
  nemsqa_binomial_confint(x = numerator, n = denominator, method = "wilson")

# regions
seizure_02_result_regions <- nemsqar::seizure_02(df = NULL,
                                                 patient_scene_table = seizure_02_patient_scene_table,
                                                 response_table = seizure_02_response_table,
                                                 situation_table = seizure_02_situation_table,
                                                 medications_table = seizure_02_medications_table,
                                                 erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                                 incident_date_col = INCIDENT_DATE,
                                                 patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                 epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                 epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                 eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                 esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                 esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                                 emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03,
                                               .by = `Region: Preparedness`
                                               ) |>
  dplyr::mutate(`Region: Preparedness` = dplyr::if_else(is.na(`Region: Preparedness`), "Missing", `Region: Preparedness`)) |>
  tidyr::complete(`Region: Preparedness`, measure, pop, fill = list(numerator = 0, denominator = 0, prop = 0, prop_label = "0%"))

# get confidence intervals
seizure_02_result_regions <- seizure_02_result_regions |>
  nemsqa_binomial_confint(x = numerator, n = denominator, method = "wilson")

# overall
seizure_02_result_overall <- nemsqar::seizure_02(df = NULL,
                                                 patient_scene_table = seizure_02_patient_scene_table,
                                                 response_table = seizure_02_response_table,
                                                 situation_table = seizure_02_situation_table,
                                                 medications_table = seizure_02_medications_table,
                                                 erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                                                 incident_date_col = INCIDENT_DATE,
                                                 patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                                                 epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                                                 epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                                                 eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                                                 esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                                                 esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                                                 emedications_03_col = MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODE_E_MEDICATIONS_03
                                               )

# get confidence intervals
seizure_02_result_overall <- seizure_02_result_overall |>
  nemsqa_binomial_confint(x = numerator, n = denominator, method = "wilson")
