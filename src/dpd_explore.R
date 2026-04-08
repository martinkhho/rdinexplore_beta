# ------------------------------------------------------------------------------
# Extracts DRUG_CODEs from DPD
# ------------------------------------------------------------------------------

# Loads DPD with caching option
dpd_load_cache <- function(download_dpd = FALSE) {
  dpd_dir <- file.path(DATA_DIR, "dpd")
  dpd_cache_rds <- file.path(DATA_DIR, "dpd_cached.rds")
  if (!download_dpd && !file.exists(dpd_cache_rds)) {
    stop("No existing DPD found, perhaps because you are using this tool for the first time. Select 'Download latest DPD'.")
  }
  dpd <- if (download_dpd) {
    dpd_loaded <- dpd_load(dpd_dir, download = TRUE)
    dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(dpd_loaded, dpd_cache_rds)
    dpd_loaded
  } else {
    readRDS(dpd_cache_rds)
  }
  return(dpd)
}

# Removes French columns from DPD tables if keep_f_cols is FALSE
dpd_keep_f_cols <- function(dpd, keep_f_cols = TRUE) {
  if (isTRUE(keep_f_cols)) {
    return(dpd)
  }
  dpd_clean <- lapply(dpd, function(x) {
    if (!is.data.frame(x)) {
      return(x)
    }
    x %>% dplyr::select(-dplyr::matches("_F$"))
  })
  return(dpd_clean)
}

# Keeps only the most recent status for each DRUG_CODE
dpd_filter_status_current <- function(status, keep_f_cols = TRUE) {
  status <- status %>%
    dplyr::mutate(
      HISTORY_DATE = as.Date(HISTORY_DATE, "%d-%b-%Y"),
      PRIORITY = dplyr::case_when(
        STATUS == "CANCELLED (SAFETY ISSUE)" ~ 10,
        STATUS == "CANCELLED POST MARKET" ~ 9,
        STATUS == "CANCELLED (UNRETURNED ANNUAL)" ~ 8,
        STATUS == "AUTHORIZATION BY INTERIM ORDER REVOKED" ~ 7,
        STATUS == "AUTHORIZATION BY INTERIM ORDER EXPIRED" ~ 6,
        STATUS == "CANCELLED PRE MARKET" ~ 5,
        STATUS == "DORMANT" ~ 4,
        STATUS == "RESTRICTED ACCESS" ~ 3,
        STATUS == "AUTHORIZED BY INTERIM ORDER" ~ 2,
        STATUS == "MARKETED" ~ 1,
        TRUE ~ 0
      ),
      CURRENT_FLAG = dplyr::if_else(CURRENT_STATUS_FLAG == "Y", 1L, 0L)
    ) %>%
    dplyr::group_by(DRUG_CODE) %>%
    dplyr::arrange(dplyr::desc(HISTORY_DATE), dplyr::desc(CURRENT_FLAG), dplyr::desc(PRIORITY), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -PRIORITY,
      -CURRENT_FLAG
    ) %>%
    dplyr::rename(
      CURRENT_STATUS = STATUS,
      CURRENT_STATUS_DATE = HISTORY_DATE
    )
  if (isTRUE(keep_f_cols)) {
    status <- status %>%
      dplyr::rename(CURRENT_STATUS_F = STATUS_F)
  }
  return(status)
}

# Constructs strength columns for the ingred table
.dpd_ingred_strength <- function(ingred, keep_f_cols = TRUE) {
  if (isTRUE(keep_f_cols)) {
    ingred <- ingred %>%
      dplyr::mutate(
        STRENGTH_F = paste0(
          dplyr::if_else(is.na(STRENGTH) | STRENGTH == "NIL",
                  "",
                  STRENGTH),
          dplyr::if_else(is.na(STRENGTH_UNIT_F) | STRENGTH_UNIT_F == "NIL",
                  "",
                  dplyr::if_else(!is.na(STRENGTH) & STRENGTH != "NIL",
                          paste0(" ", STRENGTH_UNIT_F),
                          STRENGTH_UNIT_F)),
          dplyr::if_else((is.na(DOSAGE_VALUE) | DOSAGE_VALUE == "NIL") & (is.na(DOSAGE_UNIT_F) | DOSAGE_UNIT_F == "NIL"),
                  "",
                  paste0(" /",
                         dplyr::if_else(!is.na(DOSAGE_VALUE) & DOSAGE_VALUE != "NIL",
                                 paste0(" ", DOSAGE_VALUE),
                                 ""),
                         dplyr::if_else(!is.na(DOSAGE_UNIT_F) & DOSAGE_UNIT_F != "NIL",
                                 paste0(" ", DOSAGE_UNIT_F),
                                 "")))),
        STRENGTH_F = dplyr::na_if(STRENGTH_F, "")
      ) %>%
      dplyr::select(-STRENGTH_UNIT_F, -DOSAGE_UNIT_F)
  }
  ingred <- ingred %>%
    dplyr::mutate(
      STRENGTH = paste0(
        dplyr::if_else(is.na(STRENGTH) | STRENGTH == "NIL",
                "",
                STRENGTH),
        dplyr::if_else(is.na(STRENGTH_UNIT) | STRENGTH_UNIT == "NIL",
                "",
                dplyr::if_else(!is.na(STRENGTH) & STRENGTH != "NIL",
                        paste0(" ", STRENGTH_UNIT),
                        STRENGTH_UNIT)),
        dplyr::if_else((is.na(DOSAGE_VALUE) | DOSAGE_VALUE == "NIL") & (is.na(DOSAGE_UNIT) | DOSAGE_UNIT == "NIL"),
                "",
                paste0(" /",
                       dplyr::if_else(!is.na(DOSAGE_VALUE) & DOSAGE_VALUE != "NIL",
                               paste0(" ", DOSAGE_VALUE),
                               ""),
                       dplyr::if_else(!is.na(DOSAGE_UNIT) & DOSAGE_UNIT != "NIL",
                               paste0(" ", DOSAGE_UNIT),
                               "")))),
      STRENGTH = dplyr::na_if(STRENGTH, ""),
      .keep = "unused"
    )
  return(ingred)
}

# Combines DPD tables into one flat table
dpd_combine <- function(dpd, keep_f_cols = TRUE) {
  ingred <- .dpd_ingred_strength(dpd$ingred, keep_f_cols = keep_f_cols)
  
  other_tables <- dpd[!(names(dpd) %in% c("ingred", "biosimilar"))]
  other_tables <- other_tables[vapply(other_tables, function(x) {
    is.data.frame(x) && "DRUG_CODE" %in% names(x)
  }, logical(1))]
  
  merged_tables <- c(list(ingred), other_tables)
  merged <- merged_tables %>%
    purrr::reduce(dplyr::full_join, by = "DRUG_CODE") %>%
    dplyr::mutate(
      NUMBER_OF_AIS = as.numeric(NUMBER_OF_AIS),
      BIOSIMILAR_FLAG = as.integer(DRUG_CODE %in% dpd$biosimilar$DRUG_CODE)
    )

  drop_cols <- "CURRENT_STATUS_FLAG"
  if (isTRUE(keep_f_cols)) {
    drop_cols <- c(drop_cols, "STRENGTH_TYPE_F")
  }
  
  merged <- merged %>%
    dplyr::select(-dplyr::any_of(drop_cols))
  
  return(merged)
}

# Rolls up by DRUG_CODE
dpd_rollup_code <- function(dpd_flat, keep_f_cols = TRUE) {
  
  if (isTRUE(keep_f_cols)) {
    dpd_api_strength <- dpd_flat %>%
      dplyr::distinct(DRUG_CODE, INGREDIENT, INGREDIENT_F, STRENGTH, STRENGTH_F) %>%
      dplyr::arrange(DRUG_CODE, INGREDIENT, INGREDIENT_F, STRENGTH, STRENGTH_F) %>%
      dplyr::group_by(DRUG_CODE) %>%
      dplyr::summarise(INGREDIENT = paste(INGREDIENT, collapse = " ! "),
                INGREDIENT_F = paste(INGREDIENT_F, collapse = " ! "),
                STRENGTH = paste(STRENGTH, collapse = " ! "),
                STRENGTH_F = paste(STRENGTH_F, collapse = " ! "),
                .groups = "drop")
    dpd_status <- dpd_flat %>%
      dplyr::distinct(DRUG_CODE, CURRENT_STATUS_DATE, CURRENT_STATUS, CURRENT_STATUS_F, LOT_NUMBER, EXPIRATION_DATE) %>%
      dplyr::arrange(DRUG_CODE, dplyr::desc(CURRENT_STATUS_DATE), CURRENT_STATUS, CURRENT_STATUS_F, LOT_NUMBER, EXPIRATION_DATE) %>%
      dplyr::group_by(DRUG_CODE, CURRENT_STATUS, CURRENT_STATUS_F, CURRENT_STATUS_DATE) %>%
      dplyr::summarise(LOT_NUMBER = paste(LOT_NUMBER, collapse = " ! "),
                EXPIRATION_DATE = paste(EXPIRATION_DATE, collapse = " ! "),
                .groups = "drop")
  } else {
    dpd_api_strength <- dpd_flat %>%
      dplyr::distinct(DRUG_CODE, INGREDIENT, STRENGTH) %>%
      dplyr::arrange(DRUG_CODE, INGREDIENT, STRENGTH) %>%
      dplyr::group_by(DRUG_CODE) %>%
      dplyr::summarise(INGREDIENT = paste(INGREDIENT, collapse = " ! "),
                STRENGTH = paste(STRENGTH, collapse = " ! "),
                .groups = "drop")
    dpd_status <- dpd_flat %>%
      dplyr::distinct(DRUG_CODE, CURRENT_STATUS_DATE, CURRENT_STATUS, LOT_NUMBER, EXPIRATION_DATE) %>%
      dplyr::arrange(DRUG_CODE, dplyr::desc(CURRENT_STATUS_DATE), CURRENT_STATUS, LOT_NUMBER, EXPIRATION_DATE) %>%
      dplyr::group_by(DRUG_CODE, CURRENT_STATUS, CURRENT_STATUS_DATE) %>%
      dplyr::summarise(LOT_NUMBER = paste(LOT_NUMBER, collapse = " ! "),
                EXPIRATION_DATE = paste(EXPIRATION_DATE, collapse = " ! "),
                .groups = "drop")
  }
  
  cols_to_exclude <- union(
    names(dpd_api_strength),
    names(dpd_status)
  )
  cols_to_exclude <- setdiff(cols_to_exclude, "DRUG_CODE")
  dpd_other <- dpd_flat %>%
    dplyr::select(-dplyr::any_of(cols_to_exclude)) %>%
    dplyr::group_by(DRUG_CODE) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~ {
          x <- .
          if (all(is.na(x))) {
            return(NA)
          }
          if (is.character(x)) {
            paste(sort(unique(stats::na.omit(x))), collapse = " ! ")
          } else if (is.numeric(x) || inherits(x, "Date")) {
            max(x, na.rm = TRUE)
          } else {
            dplyr::first(stats::na.omit(x))
          }
        }
      )
    )
  
  base_cols <- c(
    # Main identifiers
    "DRUG_CODE",
    "DRUG_IDENTIFICATION_NUMBER",
    "BRAND_NAME",
    "DESCRIPTOR",
    "ACTIVE_INGREDIENT_CODE",
    "INGREDIENT",
    "PHARM_FORM_CODE",
    "PHARMACEUTICAL_FORM",
    "ROUTE_OF_ADMINISTRATION_CODE",
    "ROUTE_OF_ADMINISTRATION",
    "STRENGTH",
    "STRENGTH_TYPE",
    "INGREDIENT_SUPPLIED_IND",
    "BASE",
    "NOTES",
    # Other identifiers
    "TC_ATC",
    "TC_ATC_NUMBER",
    "TC_AHFS_NUMBER",
    "PRODUCT_CATEGORIZATION",
    "ACCESSION_NUMBER",
    "NUMBER_OF_AIS",
    "AI_GROUP_NO",
    "SCHEDULE",
    # Status
    "LAST_UPDATE_DATE",
    "CURRENT_STATUS",
    "CURRENT_STATUS_DATE",
    "LOT_NUMBER",
    "EXPIRATION_DATE",
    # Miscellaneous
    "PEDIATRIC_FLAG",
    "BIOSIMILAR_FLAG",
    "CLASS",
    "VET_SPECIES",
    "VET_SUB_SPECIES",
    # Packaging
    "UPC",
    "PACKAGE_SIZE_UNIT",
    "PACKAGE_TYPE",
    "PACKAGE_SIZE",
    "PRODUCT_INFORMATION",
    # Standard of manufacture
    "PHARMACEUTICAL_STD",
    # Manufacturer
    "MFR_CODE",
    "COMPANY_CODE",
    "COMPANY_NAME",
    "COMPANY_TYPE",
    "ADDRESS_MAILING_FLAG",
    "ADDRESS_BILLING_FLAG",
    "ADDRESS_NOTIFICATION_FLAG",
    "ADDRESS_OTHER",
    "SUITE_NUMBER",
    "STREET_NAME",
    "CITY_NAME",
    "PROVINCE",
    "COUNTRY",
    "POSTAL_CODE",
    "POST_OFFICE_BOX"
  )
  merged <- dpd_api_strength %>%
    dplyr::left_join(dpd_status, by = "DRUG_CODE") %>%
    dplyr::left_join(dpd_other, by = "DRUG_CODE")
  relocate_cols <- purrr::map(base_cols, function(col) {
    if (isTRUE(keep_f_cols) && paste0(col, "_F") %in% names(merged)) {
      c(col, paste0(col, "_F"))
    } else {
      col
    }
  }) %>%
    unlist(use.names = FALSE)
  merged <- merged %>%
    dplyr::relocate(dplyr::any_of(relocate_cols))
  
  return(merged)
}

# Identifies columns that are all NA or have no variability
dpd_problem_cols <- function(dpd) {
  classify_df <- function(df) {
    if (!is.data.frame(df)) {
      return(list(
        all_na_cols = character(0),
        no_variability_cols = character(0)
      ))
    }

    cols_all_na <- names(df)[vapply(df, function(col) all(is.na(col)), logical(1))]
    cols_no_var <- names(df)[vapply(df, function(col) {
      # "No variability" means a single repeated non-NA value with no missingness
      !any(is.na(col)) && length(unique(col)) == 1
    }, logical(1))]
    cols_no_var <- setdiff(cols_no_var, cols_all_na)

    list(
      all_na_cols = cols_all_na,
      no_variability_cols = cols_no_var
    )
  }

  if (is.data.frame(dpd)) {
    return(classify_df(dpd))
  }

  if (!is.list(dpd)) {
    stop("dpd_problem_cols() expects a data.frame or a list of data.frames.")
  }

  all_na_cols <- character(0)
  no_variability_cols <- character(0)
  for (x in dpd) {
    classified <- classify_df(x)
    all_na_cols <- c(all_na_cols, classified$all_na_cols)
    no_variability_cols <- c(no_variability_cols, classified$no_variability_cols)
  }

  # Return only variable names, deduplicated across input tables.
  all_na_cols <- sort(unique(all_na_cols))
  no_variability_cols <- sort(unique(setdiff(no_variability_cols, all_na_cols)))

  return(list(
    all_na_cols = all_na_cols,
    no_variability_cols = no_variability_cols
  ))
}
