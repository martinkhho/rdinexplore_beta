# ------------------------------------------------------------------------------
# Loads DPD
# ------------------------------------------------------------------------------
# Special thanks to Dr. Jon Pipitone. His package, rdrugshortages, no longer
# seems to be maintained, so the source code has been forked from
# https://github.com/pipitone/rdrugshortages and adapted.
# ------------------------------------------------------------------------------

#' Downloads all sections of the DPD extracts and unzips them
#'
#' @param data_dir  target directory to download to
#' @return none
.dpd_download <- function(data_dir) {
    dpd_base_url <- "https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/"
    dpd_extract_url <- "https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database/what-data-extract-drug-product-database.html"
    
    if (!dir.exists(data_dir)) {
        dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Download
    for (f in c("allfiles.zip", "allfiles_ia.zip", "allfiles_ap.zip", "allfiles_dr.zip")) {
        destfile <- file.path(data_dir, f)
        url <- paste0(dpd_base_url, f)
        utils::download.file(url, destfile, quiet = TRUE)
        utils::unzip(destfile, exdir = data_dir)
    }
    
    # Scrape last updated date from DPD webpage
    last_updated <- dpd_extract_url %>%
      rvest::read_html() %>%
      rvest::html_element("table") %>%
      rvest::html_elements("td") %>%
      rvest::html_text() %>%
      stringr::str_extract_all("\\d{4}-\\d{2}-\\d{2}") %>%
      unlist() %>%
      as.Date() %>%
      min(na.rm = TRUE)
    
    # Save download date and last updated date to the same directory
    writeLines(
      c("source_name: HC Drug Product Database Digital Asset Management",
        paste("source_url:", dpd_extract_url),
        paste("source_last_updated:", last_updated),
        paste("downloaded_on:", Sys.Date())),
      file.path(data_dir, "provenance.txt")
    )
}

#' Loads the DPD
#'
#' Loads the entire Drug Product Database from a cached copy stored on the
#' filesystem. All tables are returned in a named list, and all sections
#' (current, inactive, etc) are merged on a per-table bases.
#'
#' @param data_dir target directory where extract is stored.
#' @param download  TRUE if a fresh copy of the extract should be downloaded
#'  before being loaded.
#' @return A named list of each table as a data.frame
#' @export
#' @importFrom magrittr %>%
dpd_load <- function(data_dir = "data/", download = TRUE) {
    if (download) {
      .dpd_download(data_dir = data_dir)
    }
    stopifnot(dir.exists(data_dir))
    tables <- list(
      biosimilar = c("DRUG_CODE", "SI_DESC_E", "SI_DESC_F", "SI_CODE"),
      comp = c("DRUG_CODE", "MFR_CODE", "COMPANY_CODE", "COMPANY_NAME", "COMPANY_TYPE", "ADDRESS_MAILING_FLAG", "ADDRESS_BILLING_FLAG", "ADDRESS_NOTIFICATION_FLAG",  "ADDRESS_OTHER", "SUITE_NUMBER", "STREET_NAME", "CITY_NAME", "PROVINCE", "COUNTRY", "POSTAL_CODE", "POST_OFFICE_BOX", "PROVINCE_F", "COUNTRY_F"),
      drug = c("DRUG_CODE", "PRODUCT_CATEGORIZATION", "CLASS",  "DRUG_IDENTIFICATION_NUMBER", "BRAND_NAME", "DESCRIPTOR", "PEDIATRIC_FLAG", "ACCESSION_NUMBER", "NUMBER_OF_AIS", "LAST_UPDATE_DATE", "AI_GROUP_NO", "CLASS_F", "BRAND_NAME_F", "DESCRIPTOR_F"),
      form = c("DRUG_CODE", "PHARM_FORM_CODE", "PHARMACEUTICAL_FORM", "PHARMACEUTICAL_FORM_F"),
      ingred = c("DRUG_CODE", "ACTIVE_INGREDIENT_CODE", "INGREDIENT",  "INGREDIENT_SUPPLIED_IND", "STRENGTH", "STRENGTH_UNIT", "STRENGTH_TYPE", "DOSAGE_VALUE", "BASE", "DOSAGE_UNIT", "NOTES", "INGREDIENT_F", "STRENGTH_UNIT_F", "STRENGTH_TYPE_F", "DOSAGE_UNIT_F"),
      package = c("DRUG_CODE", "UPC", "PACKAGE_SIZE_UNIT", "PACKAGE_TYPE", "PACKAGE_SIZE", "PRODUCT_INFORMATION",  "PACKAGE_SIZE_UNIT_F", "PACKAGE_TYPE_F"),
      pharm = c("DRUG_CODE", "PHARMACEUTICAL_STD"),
      route = c("DRUG_CODE", "ROUTE_OF_ADMINISTRATION_CODE", "ROUTE_OF_ADMINISTRATION", "ROUTE_OF_ADMINISTRATION_F"),
      schedule = c("DRUG_CODE", "SCHEDULE", "SCHEDULE_F"),
      status = c("DRUG_CODE", "CURRENT_STATUS_FLAG", "STATUS", "HISTORY_DATE", "STATUS_F", "LOT_NUMBER",  "EXPIRATION_DATE"),
      ther = c("DRUG_CODE", "TC_ATC_NUMBER", "TC_ATC", "TC_AHFS_NUMBER", "TC_AHFS", "TC_ATC_F", "TC_AHFS_F"),
      vet = c("DRUG_CODE",  "VET_SPECIES", "VET_SUB_SPECIES", "VET_SPECIES_F"))

    dpd <- list()
    for (table_name in names(tables)) {
        headers <- tables[[table_name]]

        dpd[[table_name]] <- dir(data_dir, pattern = paste0(table_name, ".*.txt$"), full.names = TRUE) %>%
          purrr::keep(function(x) file.size(x) > 0) %>%
          purrr::map(readr::read_csv,
            col_names = headers,
            locale = readr::locale("en",  date_format = "%d-%b-%Y"),
            col_types = purrr::map(headers, function(x) dplyr::case_when(endsWith(x, "_DATE") ~ "D", x == "DRUG_CODE" ~ "i", T ~ "c")) %>%
              stats::setNames(headers), progress = FALSE) %>%
          dplyr::bind_rows()
    }

    return(dpd)
}
