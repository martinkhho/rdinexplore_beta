# ------------------------------------------------------------------------------
# Merges DPD and CIHI
# ------------------------------------------------------------------------------

# Applies the other dataset's filters to the dataset of interest
merged_filter <- function(df1, df2_dins_excluded) {
  df1 <- df1 %>%
    dplyr::filter(!(din %in% df2_dins_excluded))
  return(df1)
}

# Appends CIHI rows to DPD
merged_dpd_cihi <- function(dpd, cihi) {
  merged <- dpd %>%
    
    # Append PDINs and unmapped DINs
    dplyr::bind_rows(cihi %>%
                       dplyr::anti_join(dpd, by = "din")) %>%
    
    # Flag PDINs and unmapped DINs
    dplyr::mutate(
      flag_unmapped = as.integer(!(is.na(flag_pdin) | flag_pdin == 1)),
      flag_pdin = dplyr::if_else(is.na(flag_pdin), 0, flag_pdin)
    )
  return(merged)
}

# Merges DPD columns into CIHI
merged_cihi_dpd <- function(cihi, dpd) {
  merged <- cihi %>%
    
    # Merge DPD columns in; where the column names overlap, use DPD columns
    dplyr::left_join(dpd, by = "din", suffix = c(".cihi", "")) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with(".cihi"),
        ~ dplyr::coalesce(
          dplyr::pick(dplyr::all_of(sub("\\.cihi$", "", dplyr::cur_column())))[[1]],
          .
        ),
        .names = "{sub('\\\\.cihi$', '', .col)}"
      )
    ) %>%
    dplyr::select(-ends_with(".cihi")) %>%
    
    # Flag unmapped DINs
    dplyr::mutate(flag_unmapped = as.integer(!(din %in% dpd$din) & flag_pdin == 0))
  return(merged)
}

# Merges in ATC, understanding that there may be "ATC_1 ! ATC_2" cases
.merged_join_atc <- function(merged, dict_who_atc) {
  
  # Clean ATC dictionary
  dict_who_atc <- dict_who_atc %>%
    dplyr::select(atc4 = atc_code, atc4_descriptor = atc_name) %>%
    dplyr::distinct()
  
  # Create dataset-specific ATC dictionary that accounts for "ATC_1 ! ATC_2"
  atc <- merged %>%
    dplyr::select(din, atc4) %>%
    tidyr::separate_rows(atc4, sep = " ! ") %>%
    dplyr::left_join(dict_who_atc, by = "atc4") %>%
    dplyr::group_by(din) %>%
    dplyr::summarise(
      atc4_descriptor = dplyr::na_if(
        paste(stats::na.omit(atc4_descriptor), collapse = " ! "),
        ""
      ),
      .groups = "drop"
    )
  
  # Merge back into dataset
  merged <- merged %>%
    dplyr::left_join(atc, by = "din")
  
  return(merged)
}

# Formats DPD+CIHI
merged_format <- function(merged, dict_who_atc) {
  has_any_atc <- "atc4" %in% names(merged) &&
    any(!is.na(merged$atc4) & nzchar(trimws(as.character(merged$atc4))))
  
  if (has_any_atc) {
    merged <- merged %>%
      .merged_join_atc(dict_who_atc)
  } else {
    merged <- merged %>%
      dplyr::mutate(atc4_descriptor = NA_character_)
  }
  
  merged <- merged %>%
    dplyr::relocate(
      atc4,
      atc4_descriptor,
      din,
      api,
      strength,
      formulation,
      route,
      brand_name,
      schedule,
      flag_biosimilar,
      flag_pdin,
      flag_unmapped
    ) %>%
    dplyr::arrange(
      atc4,
      api,
      strength,
      formulation,
      brand_name,
      din
    )
  return(merged)
}
