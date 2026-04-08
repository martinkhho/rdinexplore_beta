# ------------------------------------------------------------------------------
# Reads files
# ------------------------------------------------------------------------------

# Reads date in provenance file
read_date_field <- function(path, key) {
  lines <- readLines(path, warn = FALSE)
  matched <- lines[grepl(paste0("^", key, ":"), lines)]
  if (length(matched) == 0) {
    return(NA_character_)
  }
  key_date <- sub(paste0("^", key, ":\\s*"), "", matched[[1]])
  return(key_date)
}

# Builds a safe prefix regex from a character vector
.build_prefix_regex <- function(values) {
  values <- as.character(values)
  values <- unique(trimws(values[!is.na(values)]))
  if (length(values) == 0) {
    return(NA_character_)
  }
  escaped <- stringr::str_replace_all(
    values,
    "([\\\\^$.|?*+(){}\\[\\]])",
    "\\\\\\1"
  )
  paste0("^(", stringr::str_c(escaped, collapse = "|"), ")")
}

# Reads user-provided DIN list
.read_din_list <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }
  
  # Read file and identify DIN column (case-insensitive)
  df <- readr::read_csv(path, show_col_types = FALSE)
  din_col <- names(df)[tolower(names(df)) == "din"]
  if (length(din_col) == 0) {
    stop("No column named 'din' (case-insensitive) found in file.")
  }
  if (length(din_col) > 1) {
    stop("Multiple columns called 'din' (case-insensitive) found in file.")
  }
  
  # Select DIN column and format
  din_df <- tibble::tibble(
    din = stringr::str_pad(
      as.character(df[[din_col]]),
      width = 8,
      side = "left",
      pad = "0"
    )
  )
  return(din_df)
}

# Gets user-provided DIN list and associated ATC4
read_din_atc <- function(path, dpd, cihi = NULL, dict_who_atc = NULL) {
  
  # Input DINs
  input_dins <- .read_din_list(path) %>%
    dplyr::mutate(din = trimws(as.character(din))) %>%
    dplyr::filter(!is.na(din), nzchar(din)) %>%
    dplyr::distinct(din)
  
  # DPD lookup tables
  dpd_drug <- dpd$drug %>%
    dplyr::mutate(
      din = stringr::str_pad(as.character(DRUG_IDENTIFICATION_NUMBER), width = 8, side = "left", pad = "0"),
      DRUG_CODE,
      .keep = "none"
    ) %>%
    dplyr::distinct()
  
  dpd_ther <- dpd$ther %>%
    dplyr::mutate(
      DRUG_CODE,
      atc4 = stringr::str_to_upper(substr(as.character(TC_ATC_NUMBER), 1, 5)),
      .keep = "none"
    ) %>%
    dplyr::mutate(atc4 = dplyr::na_if(trimws(atc4), "")) %>%
    dplyr::distinct()
  
  # DPD matches
  dpd_din_atc <- input_dins %>%
    dplyr::left_join(dpd_drug, by = "din") %>%
    dplyr::left_join(dpd_ther, by = "DRUG_CODE") %>%
    dplyr::select(din, atc4) %>%
    dplyr::distinct()
  
  matched_dins <- dpd_din_atc %>%
    dplyr::filter(!is.na(atc4)) %>%
    dplyr::pull(din) %>%
    unique()
  
  unmatched_dins_dpd <- setdiff(input_dins$din, matched_dins)
  
  matched_atc_dpd <- dpd_din_atc %>%
    dplyr::filter(!is.na(atc4)) %>%
    dplyr::pull(atc4) %>%
    unique()
  
  # CIHI fallback for DPD-unmatched DINs; exclude only ATC4 that starts with Y
  matched_atc_cihi <- character(0)
  matched_dins_cihi_non_y <- character(0)
  if (!is.null(cihi) && !is.null(cihi$data) && length(unmatched_dins_dpd) > 0) {
    cihi_din_atc <- cihi$data %>%
      dplyr::mutate(
        din = stringr::str_pad(as.character(DIN), width = 8, side = "left", pad = "0"),
        atc4 = stringr::str_to_upper(trimws(as.character(`ATC4 code`))),
        .keep = "none"
      ) %>%
      dplyr::mutate(atc4 = dplyr::na_if(atc4, "")) %>%
      dplyr::filter(!is.na(din), nzchar(trimws(din)), !is.na(atc4), !stringr::str_detect(atc4, "^Y")) %>%
      dplyr::distinct()
    
    matched_atc_cihi <- cihi_din_atc %>%
      dplyr::filter(din %in% unmatched_dins_dpd) %>%
      dplyr::pull(atc4) %>%
      unique()
    
    matched_dins_cihi_non_y <- cihi_din_atc %>%
      dplyr::filter(din %in% unmatched_dins_dpd) %>%
      dplyr::pull(din) %>%
      unique()
  }
  
  unmatched_dins <- setdiff(unmatched_dins_dpd, matched_dins_cihi_non_y)
  
  matched_atc <- c(matched_atc_dpd, matched_atc_cihi) %>%
    unique() %>%
    sort()
  
  matched_din_atc <- list(
    matched_dins = matched_dins,
    unmatched_dins = unmatched_dins,
    matched_atc = matched_atc
  )
  return(matched_din_atc)
}

# Get ATC4's with the same ATC3 root as user-provided ATC4's
get_similar_atc <- function(matched_atc, dict_who_atc) {
  
  # For drugs in DPD with ATC4, get their ATC3 roots
  atc4_atc3root <- substr(matched_atc[nchar(matched_atc) == 5], 1, 4)
  
  # Merge back in with drugs in DPD with only ATC2 or ATC3
  atc_root <- matched_atc[nchar(matched_atc) < 5] %>%
    c(atc4_atc3root) %>%
    unique() %>%
    sort()
  if (length(atc_root) == 0) {
    return(character(0))
  }
  
  atc_pattern <- .build_prefix_regex(atc_root)
  if (is.na(atc_pattern)) {
    return(character(0))
  }
  
  # Get all ATC's with the given roots
  atc_df <- dict_who_atc %>%
    dplyr::filter(stringr::str_detect(atc_code, atc_pattern)) %>%
    dplyr::pull(atc_code)
  return(atc_df)
}
