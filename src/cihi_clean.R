# ------------------------------------------------------------------------------
# Extracts DINs covered by public drug formularies in Canada
# ------------------------------------------------------------------------------

# Extracts relevant information from CIHI
cihi_extract <- function(cihi, selected_benefit, selected_formulary, atc) {
  last_updated <- as.Date(paste0(cihi$last_updated, "-01"))
  atc_pattern <- .build_prefix_regex(atc)
  selected_benefit <- unique(trimws(as.character(selected_benefit)))
  selected_benefit <- selected_benefit[nzchar(selected_benefit)]
  selected_formulary <- unique(trimws(as.character(selected_formulary)))
  selected_formulary <- selected_formulary[nzchar(selected_formulary)]
  filter_benefit <- length(selected_benefit) > 0
  filter_formulary <- length(selected_formulary) > 0
  if (is.na(atc_pattern)) {
    return(tibble::tibble(
      api = character(0),
      din = character(0),
      flag_pdin = integer(0),
      brand_name = character(0),
      atc4 = character(0),
      drug_type = character(0),
      start_coverage = as.Date(character(0)),
      end_coverage = as.Date(character(0))
    ))
  }
  
  data <- cihi$data %>%
    dplyr::mutate(formulary = paste0(Jurisdiction, "::: ", `Drug program`)) %>%
    
    # Filter
    dplyr::filter(
      (!filter_benefit | `Benefit status` %in% selected_benefit),
      (!filter_formulary | formulary %in% selected_formulary),
      (stringr::str_detect(`ATC4 code`, atc_pattern) | stringr::str_detect(`ATC4 code`, "^Y")),
      !stringr::str_detect(`ATC4 code`, "^Z")
    ) %>%
    
    # Clean
    dplyr::mutate(
      dplyr::across(where(is.character), ~ na_if(., "n/a")),
      api = dplyr::if_else(
        !is.na(`Active ingredients`),
        stringr::str_replace_all(stringr::str_to_upper(`Active ingredients`), ",", " !"),
        stringr::str_to_upper(`ATC5 description`)
      ),
      flag_pdin = dplyr::if_else(`PDIN flag` == "Y", 1, 0),
      brand_name = stringr::str_to_upper(`Brand name`),
      start_coverage = as.Date(`Coverage start date`),
      end_coverage = dplyr::coalesce(as.Date(as.numeric(`Coverage end date`), origin = "1899-12-30"), last_updated),
      end_coverage = pmin(end_coverage, last_updated, na.rm = TRUE)
    ) %>%
    dplyr::select(
      api,
      din = DIN,
      flag_pdin,
      brand_name,
      atc4 = `ATC4 code`,
      drug_type = `Drug type`,
      start_coverage,
      end_coverage
    ) %>%
    dplyr::distinct()
  
  return(data)
}

# Collects DINs that were covered during the study period
cihi_filter_covered <- function(cihi_flat, start, end) {
  cihi_flat <- cihi_flat %>%
    dplyr::filter(start_coverage <= end & end_coverage >= start) %>%
    dplyr::distinct(din)
  return(cihi_flat)
}

# Keeps only therapeutic drugs
cihi_filter_therapeutic <- function(cihi_flat, dict_atc) {
  
  # Extract non-drug values
  exclude_atc <- dict_atc %>%
    dplyr::filter(atc_category == "non-drug") %>%
    dplyr::pull(atc)
  exclude_atc_regex <- .build_prefix_regex(exclude_atc)
  
  # Apply filters
  cihi_flat <- cihi_flat %>%
    dplyr::filter(
      drug_type != "Not applicable"
    )
  if (!is.na(exclude_atc_regex)) {
    cihi_flat <- cihi_flat %>%
      dplyr::filter(!stringr::str_detect(atc4, exclude_atc_regex))
  }
  
  return(cihi_flat)
}

# Keeps only prescription drugs
cihi_filter_rx <- function(cihi_flat) {
  cihi_flat <- cihi_flat %>%
    dplyr::filter(drug_type != "Over the counter")
  return(cihi_flat)
}

# Rolls up by DIN
cihi_rollup_din <- function(cihi_flat) {
  cihi_flat <- cihi_flat %>%
    dplyr::filter(!is.na(din)) %>%
    
    # Roll up variables for the same DIN; drop 'drug_type', `start_coverage`,
    # and 'end_coverage'
    dplyr::group_by(din) %>%
    dplyr::summarise(atc4 = paste(sort(unique(atc4)), collapse = " ! "),
                     api = paste(sort(unique(api)), collapse = " ! "),
                     brand_name = paste(sort(unique(brand_name)), collapse = " ! "),
                     flag_pdin = max(flag_pdin, na.rm = TRUE),
                     .groups = "drop")
  
  return(cihi_flat)
}
