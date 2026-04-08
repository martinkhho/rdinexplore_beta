# ------------------------------------------------------------------------------
# Extracts DINs in DPD
# ------------------------------------------------------------------------------

# Extracts relevant information from DPD
dpd_extract_all <- function(dpd) {
  
  # Select relevant columns
  drug <- dpd$drug  %>%
    dplyr::select(code = DRUG_CODE,
                  din = DRUG_IDENTIFICATION_NUMBER,
                  brand_name = BRAND_NAME)
  form <- dpd$form %>%
    dplyr::select(code = DRUG_CODE,
                  formulation = PHARMACEUTICAL_FORM)
  ingred <- dpd$ingred %>%
    dplyr::select(code = DRUG_CODE,
                  api = INGREDIENT,
                  strength = STRENGTH,
                  strength_unit = STRENGTH_UNIT,
                  dosage_value = DOSAGE_VALUE,
                  dosage_unit = DOSAGE_UNIT) %>%
    dplyr::mutate(
      dosage_missing = (is.na(dosage_value) | dosage_value == "NIL") & (is.na(dosage_unit) | dosage_unit == "NIL"),
      strength = paste0(dplyr::if_else((is.na(strength) | strength == "NIL"),
                                       "",
                                       strength),
                      dplyr::if_else((is.na(strength_unit) | strength_unit == "NIL"),
                                     "",
                                     dplyr::if_else(!is.na(strength) & strength != "NIL",
                                                    paste0(" ", strength_unit),
                                                    strength_unit)),
                      dplyr::if_else((is.na(dosage_value) | dosage_value == "NIL") & (is.na(dosage_unit) | dosage_unit == "NIL"),
                                     "",
                                     paste0(" /",
                                            dplyr::if_else(!is.na(dosage_value) & dosage_value != "NIL",
                                                           paste0(" ", dosage_value),
                                                           ""),
                                            dplyr::if_else(!is.na(dosage_unit) & dosage_unit != "NIL",
                                                           paste0(" ", dosage_unit),
                                                           "")))),
      strength = dplyr::na_if(strength, ""),
      .keep = "unused"
    ) %>%
    dplyr::select(-dosage_missing)
  route <- dpd$route %>%
    dplyr::select(code = DRUG_CODE,
                  route = ROUTE_OF_ADMINISTRATION)
  schedule <- dpd$schedule %>%
    dplyr::select(code = DRUG_CODE,
                  schedule = SCHEDULE)
  ther <- dpd$ther %>%
    dplyr::select(code = DRUG_CODE,
                  atc5 = TC_ATC_NUMBER) %>%
    dplyr::mutate(atc4 = substr(atc5, 1, 5), .keep = "unused")
  
  # Combine into one tibble
  merged <- list(ingred, drug, form, route, schedule, ther) %>%
    purrr::reduce(dplyr::full_join, by = "code") %>%
    dplyr::distinct() %>%
    dplyr::filter(din != "Not Applicable/non applicable") %>%
    dplyr::mutate(
      flag_biosimilar = as.integer(code %in% dpd$biosimilar$DRUG_CODE),
      flag_vet = as.integer(code %in% dpd$vet$DRUG_CODE)
    )
  
  return(merged)
}

# Filters a pre-extracted DPD table on ATC
dpd_filter_atc <- function(dpd_flat, atc) {
  atc_pattern <- .build_prefix_regex(atc)
  dpd_flat <- if (is.na(atc_pattern)) {
    dplyr::filter(dpd_flat, FALSE)
  } else {
    dplyr::filter(dpd_flat, stringr::str_detect(atc4, atc_pattern))
  }
  return(dpd_flat)
}

# Keeps only drugs marketed during the study period
dpd_filter_marketed <- function(status, start, end) {
  
  codes_marketed <- status %>%
    dplyr::select(code = DRUG_CODE,
                  current_status_flag = CURRENT_STATUS_FLAG,
                  status = STATUS,
                  history_date = HISTORY_DATE) %>%
    dplyr::mutate(history_date = as.Date(history_date, "%d-%b-%Y")) %>%
    
    # Consider "APPROVED" to be just a placeholder status. I.e., for cases where
    # a product is "MARKETED" but then "APPROVED" before the study start date,
    # still consider it marketed. However, if a drug is simply "APPROVED" and
    # never "MARKETED" then it should be excluded.
    dplyr::filter(status != "APPROVED") %>%
    
    # When multiple statuses occur on the same day, the one with the strongest
    # regulatory state should win. Higher number = higher priority.
    dplyr::mutate(
      priority = dplyr::case_when(
        status == "CANCELLED (SAFETY ISSUE)" ~ 10,
        status == "CANCELLED POST MARKET" ~ 9,
        status == "CANCELLED (UNRETURNED ANNUAL)" ~ 8,
        status == "AUTHORIZATION BY INTERIM ORDER REVOKED" ~ 7,
        status == "AUTHORIZATION BY INTERIM ORDER EXPIRED" ~ 6,
        status == "CANCELLED PRE MARKET" ~ 5,
        status == "DORMANT" ~ 4,
        status == "RESTRICTED ACCESS" ~ 3,
        status == "AUTHORIZED BY INTERIM ORDER" ~ 2,
        status == "MARKETED" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    
    # If the status is a CURRENT_STATUS_FLAG, then boost it, say, by 100
    dplyr::mutate(score = priority + dplyr::if_else(current_status_flag == "Y", 100, 0)) %>%
    
    # Select highest priority score
    dplyr::group_by(code, history_date) %>%
    dplyr::slice_max(score, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-priority, -score) %>%
    
    # Remove redundant repeated statuses (e.g., if "MARKETED" on 2020-01-01 then
    # next status is "MARKETED" again on 2020-12-31)
    dplyr::arrange(code, history_date) %>%
    dplyr::group_by(code) %>%
    dplyr::filter(status != dplyr::lag(status) | is.na(dplyr::lag(status))) %>%
    dplyr::ungroup() %>%
    
    # Identify next status change
    dplyr::arrange(code, history_date) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(next_date = dplyr::lead(history_date)) %>%
    dplyr::ungroup() %>%
    
    # Determine effective end of status period
    dplyr::mutate(effective_next_date = dplyr::coalesce(next_date, Sys.Date())) %>%
    
    # Detect overlap with study window. Border cases: (1) if the status starts
    # on the study end date, it is included; (2) if the status ends on the study
    # start date, it is excluded
    dplyr::mutate(
      overlap =
        status == "MARKETED" &
        history_date <= end &
        effective_next_date > start
    ) %>%
    
    # Summarise per drug
    dplyr::group_by(code) %>%
    dplyr::summarise(flag_marketed_any = any(overlap), .groups = "drop") %>%
    dplyr::filter(flag_marketed_any) %>%
    dplyr::select(code)
  
  return(codes_marketed)
}

# Keeps only therapeutic drugs
dpd_filter_therapeutic <- function(dpd_flat, dict_route, dict_schedule, dict_atc) {
  
  # Extract non-drug values
  exclude_routes <- dict_route %>%
    dplyr::filter(route_category == "non-drug") %>%
    dplyr::pull(route)
  exclude_schedules <- dict_schedule %>%
    dplyr::filter(schedule_category == "non-drug") %>%
    dplyr::pull(schedule)
  exclude_atc <- dict_atc %>%
    dplyr::filter(atc_category == "non-drug") %>%
    dplyr::pull(atc)
  exclude_atc_regex <- .build_prefix_regex(exclude_atc)
  
  # Apply filters
  dpd_flat <- dpd_flat %>%
    dplyr::filter(
      flag_vet == 0,
      !(route %in% exclude_routes),
      !(schedule %in% exclude_schedules)
    )
  if (!is.na(exclude_atc_regex)) {
    dpd_flat <- dpd_flat %>%
      dplyr::filter(!stringr::str_detect(atc4, exclude_atc_regex))
  }
  
  return(dpd_flat)
}

# Keeps only prescription drugs
dpd_filter_rx <- function(dpd_flat, dict_schedule) {
  
  # Extract OTC values
  exclude_otc <- dict_schedule %>%
    dplyr::filter(schedule_category == "otc") %>%
    dplyr::pull(schedule)
  
  # Apply filter
  dpd_flat <- dpd_flat %>%
    dplyr::filter(!(schedule %in% exclude_otc))
  
  return(dpd_flat)
}

# Rolls up by DIN
dpd_rollup_din <- function(dpd_flat) {
  # Roll up `api` and `strength` for the same DIN - these map to each other
  dpd_api_strength <- dpd_flat %>%
    dplyr::distinct(din, api, strength) %>%
    dplyr::arrange(din, api, strength) %>%
    dplyr::group_by(din) %>%
    dplyr::summarise(api = paste(api, collapse = " ! "),
                     strength = paste(strength, collapse = " ! "),
                     .groups = "drop")
  
  # Roll up other variables for the same DIN - these do not map together; drop
  # `code` and 'flag_vet'
  dpd_other <- dpd_flat %>%
    dplyr::group_by(din) %>%
    dplyr::summarise(atc4 = dplyr::na_if(paste(sort(unique(stats::na.omit(atc4))), collapse = " ! "), ""),
                     brand_name = paste(sort(unique(brand_name)), collapse = " ! "),
                     formulation = paste(sort(unique(formulation)), collapse = " ! "),
                     route = paste(sort(unique(route)), collapse = " ! "),
                     schedule = paste(sort(unique(schedule)), collapse = " ! "),
                     flag_biosimilar = max(flag_biosimilar, na.rm = TRUE),
                     .groups = "drop")
  
  # Combine into one tibble
  merged <- dpd_api_strength %>%
    dplyr::left_join(dpd_other, by = "din")
  
  return(merged)
}
