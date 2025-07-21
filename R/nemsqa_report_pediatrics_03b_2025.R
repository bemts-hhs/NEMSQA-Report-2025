### IOWA NEMSQA REPORT PEDIATRICS-03B 2025 -------------------------------------

###_____________________________________________________________________________
# this script will contain all reporting calculations for Pediatrics-03b
# use nemsqa_report_prep_2025.R to get critical functions into memory
###_____________________________________________________________________________
# assume that nemsqa_report_prep_2025.R was already ran to load needed packages
# and project-specific custom functions in the project
###_____________________________________________________________________________

### DATA -----------------------------------------------------------------------

# tables imported in alphabetical order
# tables do not need to be loaded again if already in memory

### exam tables ##################################################################

###_____________________________________________________________________________
# handle the exam tables differently due to size
# workflow will change from going through the dplyr::bind_rows() set to
# manipulations to manipulations before dplyr::bind_rows()
# import and clean each file
# break up the 2024 file into its 2 month parts (6) and clean each
# then bind all together at the end
###_____________________________________________________________________________

# Parallel process
exam_table <- load_nemsqa_parallel(
  table = "exam",
  years = c(
    "2021",
    "2022",
    "2023",
    "2024_1",
    "2024_2",
    "2024_3",
    "2024_4",
    "2024_5",
    "2024_6"
  ),
  cores = 10
)
###_____________________________________________________________________________
# Keep sequential process for reference
# # 2021
# exam_2021 <- import_nemsqa_data(table = "exam", year = 2021)
#
# exam_2021_clean <- exam_2021 |>
#   clean_names_dates_data()
#
# # 2022
# exam_2022 <- import_nemsqa_data(table = "exam", year = 2022)
#
# exam_2022_clean <- exam_2022 |>
#   clean_names_dates_data()
#
# # 2023
# exam_2023 <- import_nemsqa_data(table = "exam", year = 2023)
#
# exam_2023_clean <- exam_2023 |>
#   clean_names_dates_data()
#
# # 2024 is broken up into several tables due to its size, ~ 40m rows
#
# # 2024 jan-feb
# exam_2024_1 <- import_nemsqa_data(table = "exam", year = "2024_1")
#
# exam_2024_1_clean <- exam_2024_1 |>
#   clean_names_dates_data()
#
# # 2024 mar-apr
# exam_2024_2 <- import_nemsqa_data(table = "exam", year = "2024_2")
#
# exam_2024_2_clean <- exam_2024_2 |>
#   clean_names_dates_data()
#
# # 2024 may-june
# exam_2024_3 <- import_nemsqa_data(table = "exam", year = "2024_3")
#
# exam_2024_3_clean <- exam_2024_3 |>
#   clean_names_dates_data()
#
# # 2024 july-aug
# exam_2024_4 <- import_nemsqa_data(table = "exam", year = "2024_4")
#
# exam_2024_4_clean <- exam_2024_4 |>
#   clean_names_dates_data()
#
# # 2024 sept-oct
# exam_2024_5 <- import_nemsqa_data(table = "exam", year = "2024_5")
#
# exam_2024_5_clean <- exam_2024_5 |>
#   clean_names_dates_data()
#
# # 2024 nov-dec
# exam_2024_6 <- import_nemsqa_data(table = "exam", year = "2024_6")
#
# exam_2024_6_clean <- exam_2024_6 |>
#   clean_names_dates_data()
#
# # bind rows for the exam table
# exam_table <- dplyr::bind_rows(
#   exam_2021_clean,
#   exam_2022_clean,
#   exam_2023_clean,
#   exam_2024_1_clean,
#   exam_2024_2_clean,
#   exam_2024_3_clean,
#   exam_2024_4_clean,
#   exam_2024_5_clean,
#   exam_2024_6_clean
# )
###_____________________________________________________________________________

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

# parallel process
patient_scene_table <- load_nemsqa_parallel(
  table = "patient_scene",
  years = 2021:2024,
  cores = 10
)

# Keep sequential processing for posterity
# # given that patient and scene data are 1-1 relationship, join those tables
# patient_scene_2021 <- import_nemsqa_data(table = "patient_scene", year = 2021)
# patient_scene_2022 <- import_nemsqa_data(table = "patient_scene", year = 2022)
# patient_scene_2023 <- import_nemsqa_data(table = "patient_scene", year = 2023)
# patient_scene_2024 <- import_nemsqa_data(table = "patient_scene", year = 2024)
#
# # bind rows for the patient/scene table
# patient_scene_rbind <- dplyr::bind_rows(
#   patient_scene_2021,
#   patient_scene_2022,
#   patient_scene_2023,
#   patient_scene_2024
# )
#
# # set up patient/scene table for manipulations
# patient_scene_clean <- patient_scene_rbind |>
#   clean_names_dates_data()

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


### CALCULATIONS ---------------------------------------------------------------

### Pediatrics-03b =============================================================

### pediatrics-03b populations #################################################

# over all years 2021-2024
pediatrics_03b_pop <- pediatrics_03b_population(
  df = NULL,
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
pediatrics_03b_pop_2021 <- pediatrics_03b_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2021),
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
pediatrics_03b_pop_2022 <- pediatrics_03b_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2022),
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
pediatrics_03b_pop_2023 <- pediatrics_03b_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2023),
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
pediatrics_03b_pop_2024 <- pediatrics_03b_population(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::filter(INCIDENT_YEAR == 2024),
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

# pediatrics_03b populations over the years
pediatrics_03b_pop_years <- dplyr::bind_rows(
  pediatrics_03b_pop_filter_process_2021,
  pediatrics_03b_pop_filter_process_2022,
  pediatrics_03b_pop_filter_process_2023,
  pediatrics_03b_pop_filter_process_2024
)

# plot population trends over time
pediatrics_03b_pop_years |>
  plot_nemsqa_pops(
    type = "col",
    wrap_width = 25,
    plot_title = "Pediatrics-03b",
    facets = TRUE,
    vjust_title = 2,
    vjust_subtitle = 1.5
  )

### pediatrics-03b results #####################################################

# year
pediatrics_03b_result_year <- nemsqar::pediatrics_03b(
  df = NULL,
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
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE,
  .by = INCIDENT_YEAR
)

# regions and years
pediatrics_03b_result_regions_years <- nemsqar::pediatrics_03b(
  df = NULL,
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
pediatrics_03b_result_regions <- nemsqar::pediatrics_03b(
  df = NULL,
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
pediatrics_03b_result_counties <- nemsqar::pediatrics_03b(
  df = NULL,
  patient_scene_table = patient_scene_table |>
    dplyr::mutate(
      SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21 = factor(
        SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21
      )
    ),
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
pediatrics_03b_result_overall <- nemsqar::pediatrics_03b(
  df = NULL,
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
  confidence_interval = TRUE,
  method = "w",
  conf.level = 0.95,
  correct = TRUE
)

# services
pediatrics_03b_result_services <- nemsqar::pediatrics_03b(
  df = NULL,
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
  pattern = "pediatrics_03b_pop",
  measure = "Pediatrics-03b",
  folder = "population"
)

### results exports ############################################################

export_nemsqa_data(
  pattern = "pediatrics_03b_result",
  measure = "Pediatrics-03b",
  folder = "result"
)
