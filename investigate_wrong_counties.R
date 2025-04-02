investigate_wrong_counties <- patient_scene_table |>
  dplyr::distinct(FACT_INCIDENT_PK, SCENE_INCIDENT_CITY_NAME_E_SCENE_17, SCENE_INCIDENT_STATE_NAME_E_SCENE_18, SCENE_INCIDENT_POSTAL_CODE_E_SCENE_19, SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21) |>
  dplyr::filter(!SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21 %in% unique(county_data$County)) |>
  dplyr::mutate(SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21 = stringr::str_to_upper(SCENE_INCIDENT_COUNTY_NAME_E_SCENE_21))

investigate_wrong_counties_cities <- investigate_wrong_counties |>
  dplyr::select(SCENE_INCIDENT_CITY_NAME_E_SCENE_17) |>
  dplyr::mutate(SCENE_INCIDENT_CITY_NAME_E_SCENE_17 = stringr::str_to_title(SCENE_INCIDENT_CITY_NAME_E_SCENE_17),
                SCENE_INCIDENT_CITY_NAME_E_SCENE_17 = stringr::str_remove_all(SCENE_INCIDENT_CITY_NAME_E_SCENE_17, pattern = "\\w+\\s[Oo]f\\s")
  ) |>
  dplyr::distinct()
