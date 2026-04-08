# ------------------------------------------------------------------------------
# Loads CIHI formulary coverage data
# ------------------------------------------------------------------------------

# Downloads CIHI formulary coverage data
.cihi_download <- function(data_dir, destfile) {
  cihi_base_url <- "https://www.cihi.ca/sites/default/files/document/formulary-coverage-data-tool-data-table-en.xlsx"
  
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Download
  httr::GET(cihi_base_url, httr::write_disk(destfile, overwrite = TRUE))
  
  # Extract last updated date from cover page
  cover_page <- readxl::read_excel(destfile, sheet = 1, col_names = FALSE)
  last_updated <- cover_page %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    na.omit() %>%
    stringr::str_extract("(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4}") %>%
    na.omit() %>%
    unique() %>%
    lubridate::my() %>%
    min() %>%
    format("%Y-%m")
  
  # Save download date and last updated date to the same directory
  writeLines(
    c("source_name: CIHI Formulary Coverage in the Pharmaceutical Data Tool",
      paste("source_url:", cihi_base_url),
      paste("source_last_updated:", last_updated),
      paste("downloaded_on:", Sys.Date())),
    file.path(data_dir, "provenance.txt")
  )
}

# Loads CIHI formulary coverage data and finds last updated date
cihi_load <- function(data_dir = "data/", download = TRUE) {
  destfile <- file.path(data_dir, paste0("formulary_coverage_data.xlsx"))
  
  if (download) {
    .cihi_download(data_dir = data_dir, destfile = destfile)
  }
  stopifnot(dir.exists(data_dir))
  
  data <- readxl::read_excel(destfile, sheet = 2, skip = 1) %>%
    dplyr::slice(-dplyr::n())
  
  provenance_path <- file.path(data_dir, "provenance.txt")
  last_updated <- tryCatch(
    read_date_field(provenance_path, "source_last_updated"),
    error = function(e) NA_character_
  )
  
  cihi <- list(data = data, last_updated = last_updated)
  return(cihi)
}
