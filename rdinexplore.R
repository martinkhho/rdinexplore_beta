# ------------------------------------------------------------------------------
# Shiny app for exploring DINs in Canada
# ------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(here)
library(readr)
library(readxl)
library(dplyr)

source(here("./src/read_files.R"))
source(here("./src/dpd_load.R"))
source(here("./src/cihi_load.R"))
source(here("./src/dpd_clean.R"))
source(here("./src/cihi_clean.R"))
source(here("./src/merged_clean.R"))

DATA_DIR <- here("./data/")
dict_dpd_atc <- read_csv(file.path(DATA_DIR, "dictionary_dpd_atc.csv"), show_col_types = FALSE)
dict_dpd_route <- read_csv(file.path(DATA_DIR, "dictionary_dpd_route.csv"), show_col_types = FALSE)
dict_dpd_schedule <- read_csv(file.path(DATA_DIR, "dictionary_dpd_schedule.csv"), show_col_types = FALSE)



# User Interface ---------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .top-banner {
        background-color: #0072B2;
        color: white;
        padding: 15px;
        font-size: 24px;
        font-weight: bold;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        z-index: 1000;
      }
      
      .content-wrapper {
        margin-top: 80px;
        margin-bottom: 72px;
      }
      
      .checkbox label {
        white-space: nowrap;
      }
      
      .btn {
        background-color: #56B4E9 !important;
        border-color: #56B4E9 !important;
        color: white !important;
      }
      
      .btn:hover {
        background-color: #3CA0D6 !important;
        border-color: #3CA0D6 !important;
        color: white !important;
      }
      
      .btn:disabled {
        background-color: #A9D6F5 !important;
        border-color: #A9D6F5 !important;
        color: white !important;
      }
      
      .btn-clear {
        background-color: #eeeeee !important;
        color: #555555 !important;
        border: none !important;
      }

      .btn-clear:hover {
        background-color: #f5f5f5 !important;
        color: #cccccc !important;
      }
      
      .shiny-input-container {
        margin-bottom: 0px !important;
      }

      .checkbox {
        margin-top: 0px !important;
        margin-bottom: 0px !important;
      }
      
      .dpd-choice-wrap {
        display: flex;
        gap: 14px;
        margin: 16px 0px 20px 0px;
      }
      
      .dpd-choice-btn {
        width: 100%;
        padding: 14px 10px !important;
        font-weight: 600;
      }
      
      .btn-dpd-selected {
        background-color: #3CA0D6 !important;
        border-color: #3CA0D6 !important;
        opacity: 1 !important;
      }
      
      .btn-dpd-selected:hover {
        background-color: #3CA0D6 !important;
        border-color: #3CA0D6 !important;
      }
      
      .btn-dpd-faded {
        opacity: 0.45;
      }
      
      .progress-footer {
        position: fixed;
        left: 0;
        right: 0;
        bottom: 0;
        background-color: #ffffff;
        border-top: 1px solid #e9eef2;
        padding: 8px 16px 10px 16px;
        z-index: 900;
      }

    "))
  ),
  
  div(class = "top-banner", "rdinexplore"),
  
  div(class = "content-wrapper",
      uiOutput("page_content"),
      uiOutput("page_progress_ui")
  )
)



# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  app_data_dir <- here("./data/")
  dpd_cache_rds <- file.path(app_data_dir, "dpd_cached.rds")
  cihi_cache_rds <- file.path(app_data_dir, "cihi_cached.rds")
  atc_checklist_cache_rds <- file.path(app_data_dir, "atc_checklist_cached.rds")
  log_root_dir <- here("./log")
  run_id <- paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    substr(gsub("-", "", session$token), 1, 8)
  )
  output_root_dir <- here("./output/runs_rdinexplore")
  dir.create(output_root_dir, recursive = TRUE, showWarnings = FALSE)
  run_output_dir <- file.path(
    output_root_dir,
    format(Sys.Date(), "%Y"),
    format(Sys.Date(), "%m"),
    format(Sys.Date(), "%d"),
    run_id
  )
  
  log_event <- function(level = "INFO", event, details = list()) {
    tryCatch({
      day_dir <- file.path(
        log_root_dir,
        format(Sys.Date(), "%Y"),
        format(Sys.Date(), "%m"),
        format(Sys.Date(), "%d")
      )
      dir.create(day_dir, recursive = TRUE, showWarnings = FALSE)
      log_file <- file.path(day_dir, paste0("rdinexplore_", format(Sys.Date(), "%Y%m%d"), ".log"))
      
      detail_str <- ""
      if (length(details) > 0) {
        detail_vals <- vapply(details, function(x) {
          x_chr <- paste(as.character(x), collapse = ",")
          x_chr <- gsub("[\\r\\n\\t]", " ", x_chr, perl = TRUE)
          x_chr
        }, character(1))
        detail_str <- paste(
          paste0(names(detail_vals), "=", detail_vals),
          collapse = "; "
        )
      }
      
      line <- paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " [", level, "] ",
        "run_id=", run_id,
        " event=", event,
        if (nzchar(detail_str)) paste0(" ", detail_str) else ""
      )
      write(line, file = log_file, append = TRUE)
    }, error = function(e) {
      invisible(NULL)
    })
  }
  
  log_event("INFO", "session_start")
  session$onSessionEnded(function() {
    log_event("INFO", "session_end", list(page = tryCatch(current_page(), error = function(e) "unknown")))
  })
  
  ##############################################################################
  # User selections
  uploaded_file <- reactiveVal(FALSE)
  atc_dict_uploaded <- reactiveVal(FALSE)
  dpd_choice <- reactiveVal(NULL)
  cihi_choice <- reactiveVal(NULL)
  search_all <- reactiveVal(NULL)
  atc_saved_selected_ids <- reactiveVal(character(0))
  cihi_page4_saved_selected_ids <- reactiveVal(character(0))
  page1_atc_status_message <- reactiveVal(NULL)
  page1_status_message <- reactiveVal(NULL)
  page1_has_atc_choice <- reactiveVal(NULL)
  page1_has_din_choice <- reactiveVal(NULL)
  input_din_list <- reactiveVal(FALSE)
  skip_cascade_deselect_ids <- reactiveVal(character(0))
  skip_cihi_parent_deselect_ids <- reactiveVal(character(0))
  matched_dins <- reactiveVal(character(0))
  unmatched_dins <- reactiveVal(character(0))
  matched_atc <- reactiveVal(character(0))
  similar_atc <- reactiveVal(character(0))
  user_atc <- reactiveVal(character(0))
  cihi_atc_y_candidates <- reactiveVal(tibble::tibble())
  page6_cihi_y_selected_ids <- reactiveVal(character(0))
  dpd_obj <- reactiveVal(NULL)
  cihi_obj <- reactiveVal(NULL)
  cihi_ui_revision <- reactiveVal(0L)
  
  format_source_date <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return("Unknown")
    }
    x_first <- x[[1]]
    if (is.na(x_first)) {
      return("Unknown")
    }
    x_chr <- as.character(x_first)
    if (!nzchar(x_chr)) {
      return("Unknown")
    }
    x_chr
  }
  
  dpd_last_downloaded <- reactiveVal(NULL)
  dpd_last_updated <- reactiveVal(NULL)
  cihi_last_downloaded <- reactiveVal(NULL)
  cihi_last_updated <- reactiveVal(NULL)
  page5_dpd_last_updated <- reactiveVal(NULL)
  page5_cihi_last_updated <- reactiveVal(NULL)
  
  parse_source_date <- function(x) {
    if (is.null(x) || length(x) == 0) return(as.Date(NA))
    x1 <- x[[1]]
    
    if (inherits(x1, "Date")) {
      return(x1)
    }
    if (inherits(x1, "POSIXt")) {
      return(as.Date(x1))
    }
    
    tryCatch(
      suppressWarnings(as.Date(as.character(x1))),
      error = function(e) as.Date(NA)
    )
  }
  
  normalize_yyyymmdd <- function(x) {
    x <- trimws(as.character(x))
    if (grepl("^\\d{8}$", x)) {
      return(paste0(substr(x, 1, 4), "-", substr(x, 5, 6), "-", substr(x, 7, 8)))
    }
    x
  }
  
  parse_user_date <- function(x) {
    x <- normalize_yyyymmdd(x)
    if (!nzchar(x)) {
      return(as.Date(NA))
    }
    tryCatch(
      suppressWarnings(as.Date(x)),
      error = function(e) as.Date(NA)
    )
  }
  
  safe_read_date_field <- function(path, field) {
    tryCatch(
      read_date_field(path, field),
      error = function(e) NA
    )
  }
  
  refresh_provenance_dates <- function() {
    dpd_last_downloaded(
      safe_read_date_field(
        file.path(app_data_dir, "./dpd/provenance.txt"),
        "downloaded_on"
      )
    )
    dpd_last_updated(
      safe_read_date_field(
        file.path(app_data_dir, "./dpd/provenance.txt"),
        "source_last_updated"
      )
    )
    cihi_last_downloaded(
      safe_read_date_field(
        file.path(app_data_dir, "./cihi/provenance.txt"),
        "downloaded_on"
      )
    )
    cihi_last_updated(
      safe_read_date_field(
        file.path(app_data_dir, "./cihi/provenance.txt"),
        "source_last_updated"
      )
    )
    
    assign("dpd_last_downloaded", dpd_last_downloaded(), envir = .GlobalEnv)
    assign("dpd_last_updated", dpd_last_updated(), envir = .GlobalEnv)
    assign("cihi_last_downloaded", cihi_last_downloaded(), envir = .GlobalEnv)
    assign("cihi_last_updated", cihi_last_updated(), envir = .GlobalEnv)
  }
  
  refresh_page5_last_updated <- function() {
    dpd_val <- safe_read_date_field(
      file.path(app_data_dir, "./dpd/provenance.txt"),
      "source_last_updated"
    )
    cihi_val <- safe_read_date_field(
      file.path(app_data_dir, "./cihi/provenance.txt"),
      "source_last_updated"
    )
    
    page5_dpd_last_updated(dpd_val)
    page5_cihi_last_updated(cihi_val)
    
    dpd_last_updated_d <<- parse_source_date(dpd_val)
    cihi_date <- suppressWarnings(
      as.Date(lubridate::ym(as.character(format_source_date(cihi_val))))
    )
    if (is.na(cihi_date)) {
      cihi_date <- parse_source_date(cihi_val)
    }
    cihi_last_updated_d <<- cihi_date
    
    assign("dpd_last_updated_d", dpd_last_updated_d, envir = .GlobalEnv)
    assign("cihi_last_updated_d", cihi_last_updated_d, envir = .GlobalEnv)
  }
  
  cihi_nodes <- list()
  cihi_parent_ids <- character(0)
  cihi_child_ids <- character(0)
  cihi_all_checkbox_ids <- character(0)
  cihi_observers_bound <- reactiveVal(FALSE)
  
  rebuild_cihi_formulary_structures <- function(df_formularies) {
    if (is.null(df_formularies) || nrow(df_formularies) == 0) {
      dict_cihi_formularies <<- tibble::tibble(
        Jurisdiction = character(0),
        `Drug program` = character(0)
      )
      cihi_nodes <<- list()
      cihi_parent_ids <<- character(0)
      cihi_child_ids <<- character(0)
      cihi_all_checkbox_ids <<- character(0)
      cihi_observers_bound(FALSE)
      cihi_ui_revision(cihi_ui_revision() + 1L)
      assign("dict_cihi_formularies", dict_cihi_formularies, envir = .GlobalEnv)
      return(NULL)
    }
    
    df_formularies <- df_formularies[, c("Jurisdiction", "Drug program"), drop = FALSE]
    df_formularies <- unique(df_formularies)
    df_formularies <- df_formularies[
      order(df_formularies$Jurisdiction, df_formularies$`Drug program`),
      ,
      drop = FALSE
    ]
    dict_cihi_formularies <<- df_formularies
    
    cihi_jurisdictions <- unique(as.character(df_formularies$Jurisdiction))
    cihi_nodes_local <- lapply(seq_along(cihi_jurisdictions), function(i) {
      jurisdiction <- cihi_jurisdictions[[i]]
      parent_id <- paste0("cihi_jur_", sprintf("%03d", i))
      toggle_id <- paste0("toggle_", parent_id)
      child_wrap_id <- paste0("child_", parent_id)
      
      programs <- as.character(df_formularies[
        df_formularies$Jurisdiction == jurisdiction,
        "Drug program"
      ][[1]])
      programs <- unique(programs)
      
      child_ids <- vapply(
        seq_along(programs),
        function(j) paste0(parent_id, "_prog_", sprintf("%03d", j)),
        character(1)
      )
      
      list(
        jurisdiction = jurisdiction,
        parent_id = parent_id,
        toggle_id = toggle_id,
        child_wrap_id = child_wrap_id,
        programs = data.frame(
          program = programs,
          child_id = child_ids,
          stringsAsFactors = FALSE
        )
      )
    })
    
    cihi_nodes <<- cihi_nodes_local
    cihi_parent_ids <<- vapply(cihi_nodes, function(x) x$parent_id, character(1))
    cihi_child_ids <<- unlist(
      lapply(cihi_nodes, function(x) x$programs$child_id),
      use.names = FALSE
    )
    cihi_all_checkbox_ids <<- c(cihi_parent_ids, cihi_child_ids)
    cihi_observers_bound(FALSE)
    cihi_ui_revision(cihi_ui_revision() + 1L)
    assign("dict_cihi_formularies", dict_cihi_formularies, envir = .GlobalEnv)
    NULL
  }
  
  cihi_benefit_values <- if (exists("dict_cihi_benefits", inherits = TRUE)) {
    vals <- unique(as.character(get("dict_cihi_benefits", inherits = TRUE)))
    vals <- trimws(vals)
    vals[nzchar(vals)]
  } else {
    character(0)
  }
  
  selected_benefit <- character(0)
  selected_formulary <- character(0)
  selected_public_programs <- character(0)
  dpd_last_updated_d <- as.Date(NA)
  cihi_last_updated_d <- as.Date(NA)
  
  filter_time <- FALSE
  study_start <- as.Date(NA)
  study_end <- as.Date(NA)
  filter_therapeutic <- FALSE
  filter_rx <- FALSE
  add_pdin <- FALSE
  add_unmapped <- FALSE
  add_route <- FALSE
  add_schedule <- FALSE
  add_biosimilar <- FALSE
  
  dict_who_atc <- tibble::tibble(atc_code = character(0), atc_name = character(0))
  dict_who_atc1 <- dict_who_atc
  dict_who_atc2 <- dict_who_atc
  dict_who_atc3 <- dict_who_atc
  dict_who_atc4 <- dict_who_atc
  
  atc1_codes <- character(0)
  atc2_codes <- character(0)
  atc3_codes <- character(0)
  atc4_codes <- character(0)
  non_leaf_codes <- character(0)
  all_atc_codes <- character(0)
  all_atc_ids <- character(0)
  direct_children_map <- list()
  descendants_map <- list()
  atc_observers_bound <- reactiveVal(FALSE)
  nested_atc_ui_cache <- reactiveVal(NULL)
  
  # Full 4-level nested ATC UI
  build_nested_atc_ui <- function() {
    if (nrow(dict_who_atc1) == 0) {
      return(NULL)
    }
    
    tagList(
      lapply(seq_len(nrow(dict_who_atc1)), function(i1) {
        
        a1 <- dict_who_atc1$atc_code[i1]
        name1 <- dict_who_atc1$atc_name[i1]
        atc2_rows <- which(startsWith(dict_who_atc2$atc_code, a1))
        
        child_id_1 <- paste0("child_", a1)
        
        tagList(
          
          # ATC1
          div(
            style = "display:flex; justify-content:space-between; align-items:center;
                     background-color:#cfe6ff;
                     padding:8px 10px;
                     border-radius:4px;
                     border:3px solid white;
                     margin-bottom:4px;",
            
            checkboxInput(
              paste0("atc_", a1),
              paste(a1, "-", name1),
              FALSE
            ),
            
            actionButton(
              paste0("toggle_", a1),
              label = NULL,
              icon = icon("chevron-down"),
              style = "padding:4px 8px;"
            )
          ),
          
          # ATC2, ATC3, and ATC4
          hidden(
            div(
              id = child_id_1,
              style = "margin-left:20px;",
              
              lapply(atc2_rows, function(i2) {
                
                a2 <- dict_who_atc2$atc_code[i2]
                name2 <- dict_who_atc2$atc_name[i2]
                atc3_rows <- which(startsWith(dict_who_atc3$atc_code, a2))
                
                child_id_2 <- paste0("child_", a2)
                
                tagList(
                  
                  div(
                    style = "display:flex; justify-content:space-between; align-items:center;
                             background-color:#e4f2ff;
                             padding:6px 10px;
                             border-radius:4px;
                             border:2px solid white;
                             margin-bottom:4px;",
                    
                    checkboxInput(
                      paste0("atc_", a2),
                      paste(a2, "-", name2),
                      FALSE
                    ),
                    
                    actionButton(
                      paste0("toggle_", a2),
                      label = NULL,
                      icon = icon("chevron-down"),
                      style = "padding:3px 6px;"
                    )
                  ),
                  
                  hidden(
                    div(
                      id = child_id_2,
                      style = "margin-left:20px;",
                      
                      lapply(atc3_rows, function(i3) {
                        
                        a3 <- dict_who_atc3$atc_code[i3]
                        name3 <- dict_who_atc3$atc_name[i3]
                        atc4_rows <- which(startsWith(dict_who_atc4$atc_code, a3))
                        
                        child_id_3 <- paste0("child_", a3)
                        
                        tagList(
                          
                          div(
                            style = "display:flex; justify-content:space-between; align-items:center;
                                     background-color:#f2f8ff;
                                     padding:6px 10px;
                                     border-radius:4px;
                                     border:2px solid white;
                                     margin-bottom:4px;",
                            
                            checkboxInput(
                              paste0("atc_", a3),
                              paste(a3, "-", name3),
                              FALSE
                            ),
                            
                            actionButton(
                              paste0("toggle_", a3),
                              label = NULL,
                              icon = icon("chevron-down"),
                              style = "padding:2px 5px;"
                            )
                          ),
                          
                          hidden(
                            div(
                              id = child_id_3,
                              style = "margin-left:20px;",
                              
                              lapply(atc4_rows, function(i4) {
                                
                                a4 <- dict_who_atc4$atc_code[i4]
                                name4 <- dict_who_atc4$atc_name[i4]
                                
                                checkboxInput(
                                  paste0("atc_", a4),
                                  paste(a4, "-", name4),
                                  FALSE
                                )
                              })
                            )
                          )
                        )
                      })
                    )
                  )
                )
              })
            )
          )
        )
      })
    )
  }
  
  save_atc_checklist_cache <- function() {
    ui_obj <- nested_atc_ui_cache()
    if (is.null(ui_obj)) {
      ui_obj <- build_nested_atc_ui()
      nested_atc_ui_cache(ui_obj)
    }
    
    cache_obj <- list(
      dict_who_atc = dict_who_atc,
      dict_who_atc1 = dict_who_atc1,
      dict_who_atc2 = dict_who_atc2,
      dict_who_atc3 = dict_who_atc3,
      dict_who_atc4 = dict_who_atc4,
      atc1_codes = atc1_codes,
      atc2_codes = atc2_codes,
      atc3_codes = atc3_codes,
      atc4_codes = atc4_codes,
      non_leaf_codes = non_leaf_codes,
      all_atc_codes = all_atc_codes,
      all_atc_ids = all_atc_ids,
      direct_children_map = direct_children_map,
      descendants_map = descendants_map,
      nested_atc_ui = ui_obj
    )
    
    tryCatch({
      dir.create(dirname(atc_checklist_cache_rds), recursive = TRUE, showWarnings = FALSE)
      saveRDS(cache_obj, atc_checklist_cache_rds)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }
  
  rebuild_atc_structures <- function(df_atc) {
    df_atc <- df_atc[, c("atc_code", "atc_name"), drop = FALSE]
    df_atc$atc_code <- as.character(df_atc$atc_code)
    df_atc$atc_name <- as.character(df_atc$atc_name)
    df_atc <- df_atc %>%
      dplyr::mutate(
        atc_code = stringr::str_to_upper(trimws(atc_code)),
        atc_name = trimws(atc_name)
      ) %>%
      dplyr::filter(!is.na(atc_code), nzchar(atc_code)) %>%
      dplyr::group_by(atc_code) %>%
      dplyr::summarise(
        atc_name = {
          name_vals <- atc_name[!is.na(atc_name) & nzchar(atc_name)]
          if (length(name_vals) == 0) NA_character_ else name_vals[[1]]
        },
        .groups = "drop"
      )
    
    dict_who_atc <<- df_atc
    dict_who_atc1 <<- filter(dict_who_atc, nchar(atc_code) == 1)
    dict_who_atc2 <<- filter(dict_who_atc, nchar(atc_code) == 3)
    dict_who_atc3 <<- filter(dict_who_atc, nchar(atc_code) == 4)
    dict_who_atc4 <<- filter(dict_who_atc, nchar(atc_code) == 5)
    
    atc1_codes <<- dict_who_atc1$atc_code
    atc2_codes <<- dict_who_atc2$atc_code
    atc3_codes <<- dict_who_atc3$atc_code
    atc4_codes <<- dict_who_atc4$atc_code
    non_leaf_codes <<- c(atc1_codes, atc2_codes, atc3_codes)
    all_atc_codes <<- c(non_leaf_codes, atc4_codes)
    all_atc_ids <<- paste0("atc_", all_atc_codes)
    
    direct_children_map_local <- setNames(
      vector("list", length(non_leaf_codes)),
      non_leaf_codes
    )
    for (code in atc1_codes) {
      direct_children_map_local[[code]] <- atc2_codes[startsWith(atc2_codes, code)]
    }
    for (code in atc2_codes) {
      direct_children_map_local[[code]] <- atc3_codes[startsWith(atc3_codes, code)]
    }
    for (code in atc3_codes) {
      direct_children_map_local[[code]] <- atc4_codes[startsWith(atc4_codes, code)]
    }
    direct_children_map <<- direct_children_map_local
    
    descendants_map_local <- setNames(
      vector("list", length(all_atc_codes)),
      all_atc_codes
    )
    for (code in all_atc_codes) {
      descendants_map_local[[code]] <- c(
        atc2_codes[startsWith(atc2_codes, code)],
        atc3_codes[startsWith(atc3_codes, code)],
        atc4_codes[startsWith(atc4_codes, code)]
      )
    }
    descendants_map <<- descendants_map_local
    
    assign("dict_who_atc", dict_who_atc, envir = .GlobalEnv)
    assign("dict_who_atc1", dict_who_atc1, envir = .GlobalEnv)
    assign("dict_who_atc2", dict_who_atc2, envir = .GlobalEnv)
    assign("dict_who_atc3", dict_who_atc3, envir = .GlobalEnv)
    assign("dict_who_atc4", dict_who_atc4, envir = .GlobalEnv)
    
    nested_atc_ui_cache(build_nested_atc_ui())
    if (!isTRUE(save_atc_checklist_cache())) {
      log_event("WARN", "atc_checklist_cache_save_failed")
    }
  }
  
  load_cached_atc_checklist <- function() {
    cache_obj <- tryCatch(readRDS(atc_checklist_cache_rds), error = function(e) NULL)
    req_fields <- c(
      "dict_who_atc", "dict_who_atc1", "dict_who_atc2", "dict_who_atc3", "dict_who_atc4",
      "atc1_codes", "atc2_codes", "atc3_codes", "atc4_codes",
      "non_leaf_codes", "all_atc_codes", "all_atc_ids",
      "direct_children_map", "descendants_map", "nested_atc_ui"
    )
    
    if (is.null(cache_obj) || !is.list(cache_obj) || !all(req_fields %in% names(cache_obj))) {
      return(FALSE)
    }
    
    rebuild_atc_structures(cache_obj$dict_who_atc)
    atc_observers_bound(FALSE)
    TRUE
  }
  
  # Deselection helper
  programmatic_uncheck <- function(id) {
    if (id %in% names(input) && isTRUE(input[[id]])) {
      skip_cascade_deselect_ids(
        unique(c(skip_cascade_deselect_ids(), id))
      )
      updateCheckboxInput(session, id, value = FALSE)
    }
  }
  
  # Return direct children for one ATC code.
  get_direct_children <- function(code) {
    children <- direct_children_map[[code]]
    if (is.null(children)) character(0) else children
  }
  
  # Return all descendants below one ATC code.
  get_all_descendants <- function(code) {
    descendants <- descendants_map[[code]]
    if (is.null(descendants)) character(0) else descendants
  }
  
  # If all direct children of a parent are selected, auto-select parent.
  maybe_autocheck_parent <- function(code) {
    parent <- substr(code, 1, nchar(code) - 1)
    if (nchar(parent) < 1) return(NULL)
    
    parent_id <- paste0("atc_", parent)
    if (!(parent_id %in% names(input))) return(NULL)
    
    siblings <- get_direct_children(parent)
    if (length(siblings) == 0) return(NULL)
    
    sibling_ids <- paste0("atc_", siblings)
    all_selected <- all(vapply(
      sibling_ids,
      function(id) isTRUE(input[[id]]),
      logical(1)
    ))
    
    if (all_selected && !isTRUE(input[[parent_id]])) {
      updateCheckboxInput(session, parent_id, value = TRUE)
    }
  }
  
  # Deselection unchecks ancestors
  uncheck_ancestors <- function(code) {
    parent <- substr(code, 1, nchar(code) - 1)
    while (nchar(parent) >= 1) {
      parent_id <- paste0("atc_", parent)
      if (parent_id %in% names(input)) {
        programmatic_uncheck(parent_id)
      }
      parent <- substr(parent, 1, nchar(parent) - 1)
    }
  }
  
  bind_atc_observers <- function() {
    if (isTRUE(atc_observers_bound()) || length(all_atc_codes) == 0) {
      return(NULL)
    }
    
    lapply(non_leaf_codes, function(code) {
      observeEvent(input[[paste0("atc_", code)]], {
        input_id <- paste0("atc_", code)
        value <- input[[input_id]]
        
        if (isTRUE(value)) {
          children <- get_direct_children(code)
          lapply(children, function(child) {
            updateCheckboxInput(
              session,
              paste0("atc_", child),
              value = value
            )
          })
          maybe_autocheck_parent(code)
        }
        
        if (!isTRUE(value)) {
          if (input_id %in% skip_cascade_deselect_ids()) {
            skip_cascade_deselect_ids(
              setdiff(skip_cascade_deselect_ids(), input_id)
            )
            return(NULL)
          }
          
          descendants <- get_all_descendants(code)
          lapply(descendants, function(child) {
            programmatic_uncheck(paste0("atc_", child))
          })
          uncheck_ancestors(code)
        }
      }, ignoreInit = TRUE)
    })
    
    lapply(atc4_codes, function(code) {
      observeEvent(input[[paste0("atc_", code)]], {
        input_id <- paste0("atc_", code)
        value <- input[[input_id]]
        
        if (isTRUE(value)) {
          maybe_autocheck_parent(code)
        }
        
        if (!isTRUE(value)) {
          if (input_id %in% skip_cascade_deselect_ids()) {
            skip_cascade_deselect_ids(
              setdiff(skip_cascade_deselect_ids(), input_id)
            )
            return(NULL)
          }
          uncheck_ancestors(code)
        }
      }, ignoreInit = TRUE)
    })
    
    lapply(non_leaf_codes, function(code) {
      observeEvent(input[[paste0("toggle_", code)]], {
        shinyjs::toggle(id = paste0("child_", code))
      }, ignoreInit = TRUE)
    })
    
    atc_observers_bound(TRUE)
    NULL
  }
  
  clear_all_atc <- function() {
    skip_cascade_deselect_ids(unique(c(skip_cascade_deselect_ids(), all_atc_ids)))
    lapply(all_atc_ids, function(id) {
      updateCheckboxInput(session, id, value = FALSE)
    })
  }
  
  select_codes <- function(codes) {
    ids <- paste0("atc_", codes)
    valid_ids <- unique(ids[ids %in% all_atc_ids])
    lapply(valid_ids, function(id) {
      updateCheckboxInput(session, id, value = TRUE)
    })
  }
  
  sync_button_enabled <- function(id, is_enabled) {
    session$onFlushed(function() {
      if (isTRUE(is_enabled)) {
        shinyjs::enable(id)
      } else {
        shinyjs::disable(id)
      }
    }, once = TRUE)
  }
  
  get_selected_ids <- function(ids) {
    ids[vapply(ids, function(id) isTRUE(input[[id]]), logical(1))]
  }
  
  save_atc_dictionary_upload <- function(require_upload = FALSE) {
    if (is.null(input$atc_dict_upload)) {
      if (isTRUE(require_upload)) {
        log_event("ERROR", "atc_upload_missing_file")
        showModal(modalDialog(
          title = "Missing ATC Dictionary File",
          "Please upload an ATC dictionary CSV file.",
          easyClose = TRUE
        ))
        return(FALSE)
      }
      return(isTRUE(atc_dict_uploaded()))
    }
    
    atc_file_path <- input$atc_dict_upload$datapath
    atc_file_name <- input$atc_dict_upload$name
    atc_ext <- tolower(tools::file_ext(atc_file_name))
    
    if (!identical(atc_ext, "csv")) {
      log_event(
        "ERROR",
        "atc_upload_invalid_extension",
        list(file = atc_file_name, extension = atc_ext)
      )
      showModal(modalDialog(
        title = "Invalid ATC Dictionary File",
        paste0(
          "ATC dictionary upload error: '", atc_file_name,
          "' is not a CSV file. Please upload a CSV file."
        ),
        easyClose = TRUE
      ))
      return(FALSE)
    }
    
    dir.create(here("data"), showWarnings = FALSE, recursive = TRUE)
    
    df_atc <- tryCatch(
      read_csv(atc_file_path, show_col_types = FALSE),
      error = function(e) NULL
    )
    if (is.null(df_atc)) {
      log_event(
        "ERROR",
        "atc_upload_unreadable",
        list(file = atc_file_name)
      )
      showModal(modalDialog(
        title = "Unreadable ATC Dictionary File",
        paste0(
          "ATC dictionary upload error: could not read '", atc_file_name,
          "' as CSV."
        ),
        easyClose = TRUE
      ))
      return(FALSE)
    }
    
    colnames_lower <- tolower(names(df_atc))
    atc_name_idx <- which(colnames_lower == "atc_name")
    atc_code_idx <- which(colnames_lower == "atc_code")
    if (length(atc_code_idx) == 0 || length(atc_name_idx) == 0) {
      log_event(
        "ERROR",
        "atc_upload_missing_columns",
        list(file = atc_file_name, columns = paste(names(df_atc), collapse = ","))
      )
      showModal(modalDialog(
        title = "Missing ATC Dictionary Columns",
        paste0(
          "ATC dictionary upload error in '", atc_file_name,
          "': required columns are 'atc_code' and 'atc_name' (case-insensitive)."
        ),
        easyClose = TRUE
      ))
      return(FALSE)
    }
    
    df_atc <- df_atc[, c(atc_code_idx[[1]], atc_name_idx[[1]]), drop = FALSE]
    names(df_atc) <- c("atc_code", "atc_name")
    df_atc$atc_code <- as.character(df_atc$atc_code)
    df_atc$atc_name <- as.character(df_atc$atc_name)
    
    rebuild_atc_structures(df_atc)
    write_csv(dict_who_atc, here("data", "dictionary_who_atc.csv"))
    
    atc_dict_uploaded(TRUE)
    page1_atc_status_message(paste0(
      "Saved and using uploaded ATC dictionary file: ", atc_file_name,
      " (saved to ./data/dictionary_who_atc.csv). ",
      "Processed checklist cached to ./data/atc_checklist_cached.rds."
    ))
    TRUE
  }
  
  programmatic_uncheck_cihi_parent <- function(id) {
    if (id %in% names(input) && isTRUE(input[[id]])) {
      skip_cihi_parent_deselect_ids(
        unique(c(skip_cihi_parent_deselect_ids(), id))
      )
      updateCheckboxInput(session, id, value = FALSE)
    }
  }
  
  bind_cihi_observers <- function() {
    if (isTRUE(cihi_observers_bound()) || length(cihi_nodes) == 0) {
      return(NULL)
    }
    
    lapply(cihi_nodes, function(node) {
      observeEvent(input[[node$parent_id]], {
        value <- input[[node$parent_id]]
        
        if (!isTRUE(value) && node$parent_id %in% skip_cihi_parent_deselect_ids()) {
          skip_cihi_parent_deselect_ids(
            setdiff(skip_cihi_parent_deselect_ids(), node$parent_id)
          )
          return(NULL)
        }
        
        lapply(node$programs$child_id, function(child_id) {
          updateCheckboxInput(session, child_id, value = value)
        })
      }, ignoreInit = TRUE)
    })
    
    lapply(cihi_nodes, function(node) {
      lapply(node$programs$child_id, function(child_id) {
        observeEvent(input[[child_id]], {
          value <- input[[child_id]]
          
          if (!isTRUE(value)) {
            programmatic_uncheck_cihi_parent(node$parent_id)
          } else {
            all_selected <- all(vapply(
              node$programs$child_id,
              function(id) isTRUE(input[[id]]),
              logical(1)
            ))
            if (all_selected && !isTRUE(input[[node$parent_id]])) {
              updateCheckboxInput(session, node$parent_id, value = TRUE)
            }
          }
        }, ignoreInit = TRUE)
      })
    })
    
    lapply(cihi_nodes, function(node) {
      observeEvent(input[[node$toggle_id]], {
        shinyjs::toggle(id = node$child_wrap_id)
      }, ignoreInit = TRUE)
    })
    
    cihi_observers_bound(TRUE)
    NULL
  }
  
  ##############################################################################
  # Page flow
  current_page <- reactiveVal("1_dpd")
  page_order <- c("1_dpd", "2_upload", "3_atc", "4_search", "5_filters", "6_missing_atc", "7_end")
  
  observeEvent(current_page(), {
    log_event("INFO", "page_transition", list(page = current_page()))
  }, ignoreInit = FALSE)
  
  page1_can_continue <- reactive({
    atc_choice <- page1_has_atc_choice()
    input_atc_choice <- input$page1_use_existing_atc
    if (!is.null(input_atc_choice) && nzchar(input_atc_choice)) {
      atc_choice <- input_atc_choice
      if (!identical(page1_has_atc_choice(), atc_choice)) {
        page1_has_atc_choice(atc_choice)
      }
    }
    
    atc_has_valid_selection <- !is.null(input$atc_dict_upload) &&
      identical(tolower(tools::file_ext(input$atc_dict_upload$name)), "csv")
    has_atc_choice <- !is.null(atc_choice)
    atc_ready <- if (identical(atc_choice, "yes")) {
      file.exists(atc_checklist_cache_rds)
    } else if (identical(atc_choice, "no")) {
      atc_has_valid_selection
    } else {
      FALSE
    }
    
    din_choice <- page1_has_din_choice()
    input_din_choice <- input$page1_has_din_list
    if (!is.null(input_din_choice) && nzchar(input_din_choice)) {
      din_choice <- input_din_choice
      if (!identical(page1_has_din_choice(), din_choice)) {
        page1_has_din_choice(din_choice)
      }
    }
    
    has_din_choice <- !is.null(din_choice)
    din_needed <- identical(din_choice, "yes")
    din_ready <- !din_needed || isTRUE(uploaded_file()) || !is.null(input$file_upload)
    
    isTRUE(has_atc_choice && atc_ready && has_din_choice && din_ready)
  })
  
  output$page1_continue_btn <- renderUI({
    if (isTRUE(page1_can_continue())) {
      actionButton("continue_btn1", "Continue")
    } else {
      disabled(actionButton("continue_btn1", "Continue"))
    }
  })
  
  # Continue
  observeEvent(input$continue_btn1, {
    shinyjs::disable("continue_btn1")
    can_go_page3 <- TRUE
    abort_page2_continue <- function() {
      can_go_page3 <<- FALSE
      shinyjs::enable("continue_btn1")
    }
    
    atc_choice <- input$page1_use_existing_atc
    if (is.null(atc_choice) || !nzchar(atc_choice)) {
      showModal(modalDialog(
        title = "Missing ATC Dictionary Choice",
        "Please answer whether you want to use an existing cached ATC dictionary.",
        easyClose = TRUE
      ))
      abort_page2_continue()
      return(NULL)
    }
    page1_has_atc_choice(atc_choice)
    
    if (identical(atc_choice, "yes")) {
      if (!isTRUE(load_cached_atc_checklist())) {
        showModal(modalDialog(
          title = "Missing Cached ATC Checklist",
          paste(
            "No cached ATC checklist was found.",
            "Select 'No' to upload an ATC dictionary CSV and build/cache it."
          ),
          easyClose = TRUE
        ))
        abort_page2_continue()
        return(NULL)
      }
      atc_dict_uploaded(TRUE)
      page1_atc_status_message("Loaded cached ATC dictionary/checklist from ./data/atc_checklist_cached.rds.")
    } else {
      if (!isTRUE(save_atc_dictionary_upload(require_upload = TRUE))) {
        abort_page2_continue()
        return(NULL)
      }
    }
    
    log_event(
      "INFO",
      "page2_to_page3_start",
      list(
        atc_choice = atc_choice,
        dpd_choice = dpd_choice(),
        cihi_choice = cihi_choice()
      )
    )
    
    tryCatch({
      withProgress(message = "Preparing DIN/ATC...", value = 0, {
        din_choice <- input$page1_has_din_list
        if (is.null(din_choice) || !nzchar(din_choice)) {
          abort_page2_continue()
          return(NULL)
        }
        page1_has_din_choice(din_choice)
        use_din_upload <- identical(din_choice, "yes")
        
        if (use_din_upload) {
          if (is.null(input$file_upload)) {
            log_event("ERROR", "din_upload_missing_file")
            showModal(modalDialog(
              title = "Missing DIN List File",
              "You selected 'Yes' for existing DIN list. Please upload a DIN list file.",
              easyClose = TRUE
            ))
            abort_page2_continue()
            return(NULL)
          }
          
          file_path <- input$file_upload$datapath
          file_name <- input$file_upload$name
          ext <- tolower(tools::file_ext(file_name))
          
          dir.create(here("data"), showWarnings = FALSE, recursive = TRUE)
          
          if (!ext %in% c("csv", "xls", "xlsx")) {
            log_event(
              "ERROR",
              "din_upload_invalid_extension",
              list(file = file_name, extension = ext)
            )
            showModal(modalDialog(
              title = "Invalid DIN List File",
              paste0(
                "DIN list upload error: '", file_name,
                "' is not a CSV or Excel file."
              ),
              easyClose = TRUE
            ))
            abort_page2_continue()
            return(NULL)
          }
          
          # Read file
          df <- tryCatch(
            if (ext == "csv") {
              read_csv(file_path, show_col_types = FALSE)
            } else {
              read_excel(file_path)
            },
            error = function(e) NULL
          )
          if (is.null(df)) {
            log_event(
              "ERROR",
              "din_upload_unreadable",
              list(file = file_name)
            )
            showModal(modalDialog(
              title = "Unreadable DIN List File",
              paste0(
                "DIN list upload error: could not read '", file_name,
                "' as CSV/Excel."
              ),
              easyClose = TRUE
            ))
            abort_page2_continue()
            return(NULL)
          }
          
          # Validate DIN column (case-insensitive)
          colnames_lower <- tolower(names(df))
          din_idx <- which(colnames_lower == "din")
          if (length(din_idx) == 0) {
            log_event(
              "ERROR",
              "din_upload_missing_din_column",
              list(file = file_name, columns = paste(names(df), collapse = ","))
            )
            showModal(modalDialog(
              title = "Missing DIN Column",
              paste0(
                "DIN list upload error in '", file_name,
                "': required column is 'din' (case-insensitive)."
              ),
              easyClose = TRUE
            ))
            abort_page2_continue()
            return(NULL)
          }
          
          # Keep only required DIN column and standardize the name.
          din_col <- names(df)[din_idx[[1]]]
          df <- tibble::tibble(
            din = stringr::str_pad(
              as.character(df[[din_col]]),
              width = 8,
              side = "left",
              pad = "0"
            )
          )
          
          # Save validated file
          write_csv(df, here("data", "input_din_list.csv"))
          assign("initial_dins", df, envir = .GlobalEnv)
          
          uploaded_file(TRUE)
          page1_status_message(paste0(
            "Saved and using uploaded DIN file: ", file_name,
            " (saved to ./data/input_din_list.csv)."
          ))
        } else {
          uploaded_file(FALSE)
          page1_status_message(NULL)
        }
        
        input_din_list(use_din_upload)
        assign("input_din_list", isTRUE(use_din_upload), envir = .GlobalEnv)
        
        dpd <- dpd_obj()
        cihi <- cihi_obj()
        if (is.null(dpd)) {
          abort_page2_continue()
          showModal(modalDialog(
            title = "Missing DPD Data",
            "DPD data is not loaded. Please go back to Page 1 and continue again.",
            easyClose = TRUE
          ))
          return(NULL)
        }
        
        incProgress(0.4, detail = "Preparing matches")
        
        if (isTRUE(input_din_list())) {
          input_din_atc <- read_din_atc(
            file.path(app_data_dir, "./input_din_list.csv"),
            dpd,
            cihi,
            dict_who_atc
          )
          
          matched_dins_vals <- input_din_atc$matched_dins
          unmatched_dins_vals <- input_din_atc$unmatched_dins
          matched_atc_vals <- input_din_atc$matched_atc
          similar_atc_vals <- get_similar_atc(matched_atc_vals, dict_who_atc)
          user_atc_vals <- similar_atc_vals
          
          matched_dins(matched_dins_vals)
          unmatched_dins(unmatched_dins_vals)
          matched_atc(matched_atc_vals)
          similar_atc(similar_atc_vals)
          user_atc(user_atc_vals)
          
          assign("matched_dins", matched_dins_vals, envir = .GlobalEnv)
          assign("unmatched_dins", unmatched_dins_vals, envir = .GlobalEnv)
          assign("matched_atc", matched_atc_vals, envir = .GlobalEnv)
          assign("similar_atc", similar_atc_vals, envir = .GlobalEnv)
          assign("user_atc", user_atc_vals, envir = .GlobalEnv)
        } else {
          user_atc_vals <- c(
            pull(dict_who_atc1, atc_code),
            pull(dict_who_atc2, atc_code),
            pull(dict_who_atc3, atc_code),
            pull(dict_who_atc4, atc_code)
          )
          matched_dins(character(0))
          unmatched_dins(character(0))
          matched_atc(character(0))
          similar_atc(character(0))
          user_atc(user_atc_vals)
          assign("user_atc", user_atc_vals, envir = .GlobalEnv)
        }
        
        incProgress(1, detail = "Done")
      })
      if (!isTRUE(can_go_page3)) {
        return(NULL)
      }
      log_event("INFO", "page2_to_page3_success")
      
      current_page("3_atc")
    }, error = function(e) {
      shinyjs::enable("continue_btn1")
      msg <- conditionMessage(e)
      msg <- gsub("\\x1b\\[[0-9;]*m", "", msg, perl = TRUE)
      log_event("ERROR", "page2_to_page3_error", list(message = msg))
      showModal(modalDialog(
        title = "Error loading data",
        paste0("Could not continue from Page 2 to Page 3. ", msg),
        easyClose = TRUE
      ))
    })
  })
  
  output$page1_atc_upload_status <- renderUI({
    atc_choice <- input$page1_use_existing_atc
    selected_name <- if (is.null(input$atc_dict_upload)) {
      NULL
    } else {
      input$atc_dict_upload$name
    }
    cache_exists <- file.exists(atc_checklist_cache_rds)
    
    if (
      is.null(page1_atc_status_message()) &&
      is.null(selected_name) &&
      !identical(atc_choice, "yes")
    ) {
      return(NULL)
    }
    
    tagList(
      if (!is.null(page1_atc_status_message())) {
        div(
          style = "margin:8px 0 4px 0; color:#2d6a4f;",
          strong("Status: "),
          page1_atc_status_message()
        )
      },
      if (identical(atc_choice, "yes") && !cache_exists) {
        div(
          style = "margin:8px 0 4px 0; color:#9b2226;",
          strong("Status: "),
          "No cached ATC checklist found at ./data/atc_checklist_cached.rds. Choose 'No' to upload and cache."
        )
      },
      if (!is.null(selected_name) && !identical(atc_choice, "yes")) {
        div(
          style = "margin:2px 0 8px 0; color:#1d3557;",
          strong("Current file selection: "),
          selected_name
        )
      }
    )
  })
  
  output$page1_upload_status <- renderUI({
    if (!identical(page1_has_din_choice(), "yes")) {
      return(NULL)
    }
    
    selected_name <- if (is.null(input$file_upload)) {
      NULL
    } else {
      input$file_upload$name
    }
    
    if (is.null(page1_status_message()) && is.null(selected_name)) {
      return(NULL)
    }
    
    tagList(
      if (!is.null(page1_status_message())) {
        div(
          style = "margin:8px 0 4px 0; color:#2d6a4f;",
          strong("Status: "),
          page1_status_message()
        )
      },
      if (!is.null(selected_name)) {
        div(
          style = "margin:2px 0 8px 0; color:#1d3557;",
          strong("Current file selection: "),
          selected_name
        )
      }
    )
  })
  
  observeEvent(input$page1_toggle_atc_info, {
    shinyjs::toggle("page1_atc_info")
  })
  
  observeEvent(input$page1_has_din_list, {
    choice <- input$page1_has_din_list
    if (is.null(choice) || !nzchar(choice)) {
      return(NULL)
    }
    
    page1_has_din_choice(choice)
    
    if (identical(choice, "no")) {
      uploaded_file(FALSE)
      page1_status_message(NULL)
      reset("file_upload")
      input_din_list(FALSE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$dpd_download_btn, {
    dpd_choice("download")
  })
  
  observeEvent(input$dpd_no_download_btn, {
    dpd_choice("no_download")
  })
  
  observeEvent(input$cihi_download_btn, {
    cihi_choice("download")
  })
  
  observeEvent(input$cihi_no_download_btn, {
    cihi_choice("no_download")
  })
  
  observeEvent(current_page(), {
    req(current_page() == "1_dpd")
    refresh_provenance_dates()
    session$onFlushed(function() {
      shinyjs::runjs("$('#page1_loading_inline').hide();")
    }, once = TRUE)
  }, ignoreInit = FALSE)
  
  observeEvent(input$back_btn2, {
    current_page("1_dpd")
  })
  
  observeEvent(input$continue_btn_dpd, {
    req(!is.null(dpd_choice()))
    req(!is.null(cihi_choice()))
    shinyjs::disable("continue_btn_dpd")
    log_event(
      "INFO",
      "page1_to_page2_start",
      list(
        dpd_choice = dpd_choice(),
        cihi_choice = cihi_choice()
      )
    )
    
    tryCatch({
      withProgress(message = "Loading DPD/CIHI...", value = 0, {
        download_dpd <- identical(dpd_choice(), "download")
        download_cihi <- identical(cihi_choice(), "download")
        
        assign("download_dpd", download_dpd, envir = .GlobalEnv)
        assign("download_cihi", download_cihi, envir = .GlobalEnv)
        
        dpd_dir <- file.path(app_data_dir, "dpd")
        cihi_dir <- file.path(app_data_dir, "cihi")
        
        if (!download_dpd && !file.exists(dpd_cache_rds)) {
          stop("No existing DPD found, perhaps because you are using this tool for the first time. Select 'Download latest DPD'.")
        }
        if (!download_cihi && !file.exists(cihi_cache_rds)) {
          stop("No existing CIHI found, perhaps because you are using this tool for the first time. Select 'Download latest CIHI'.")
        }
        
        incProgress(
          0.1,
          detail = if (download_dpd) "Loading DPD" else "Loading cached DPD"
        )
        dpd <- tryCatch(
          {
            if (download_dpd) {
              dpd_loaded <- dpd_load(dpd_dir, download = TRUE)
              dir.create(app_data_dir, recursive = TRUE, showWarnings = FALSE)
              saveRDS(dpd_loaded, dpd_cache_rds)
              dpd_loaded
            } else {
              readRDS(dpd_cache_rds)
            }
          },
          error = function(e) {
            msg <- conditionMessage(e)
            msg <- gsub("\\x1b\\[[0-9;]*m", "", msg, perl = TRUE)
            log_event(
              "ERROR",
              "dpd_load_failed",
              list(download = download_dpd, message = msg)
            )
            stop(paste0("DPD load failed. ", msg))
          }
        )
        if (is.null(dpd$drug) || is.null(dpd$ther)) {
          log_event(
            "ERROR",
            "dpd_load_failed",
            list(download = download_dpd, message = "DPD loaded but required tables ('drug' and 'ther') are missing.")
          )
          stop("DPD loaded but required tables ('drug' and 'ther') are missing.")
        }
        
        incProgress(
          0.3,
          detail = if (download_cihi) "Loading CIHI" else "Loading cached CIHI"
        )
        cihi <- tryCatch(
          {
            if (download_cihi) {
              cihi_loaded <- cihi_load(cihi_dir, download = TRUE)
              dir.create(app_data_dir, recursive = TRUE, showWarnings = FALSE)
              saveRDS(cihi_loaded, cihi_cache_rds)
              cihi_loaded
            } else {
              readRDS(cihi_cache_rds)
            }
          },
          error = function(e) {
            msg <- conditionMessage(e)
            msg <- gsub("\\x1b\\[[0-9;]*m", "", msg, perl = TRUE)
            log_event(
              "ERROR",
              "cihi_load_failed",
              list(download = download_cihi, message = msg)
            )
            stop(paste0("CIHI load failed. ", msg))
          }
        )
        if (is.null(cihi$data)) {
          log_event(
            "ERROR",
            "cihi_load_failed",
            list(download = download_cihi, message = "CIHI loaded but expected `cihi$data` is missing.")
          )
          stop("CIHI loaded but expected `cihi$data` is missing.")
        }
        
        dpd_obj(dpd)
        cihi_obj(cihi)
        assign("dpd", dpd, envir = .GlobalEnv)
        assign("cihi", cihi, envir = .GlobalEnv)
        
        if (isTRUE(download_dpd) || isTRUE(download_cihi)) {
          refresh_provenance_dates()
        }
        
        incProgress(0.5, detail = "Preparing CIHI checklist")
        dict_cihi_formularies_runtime <- cihi$data %>%
          distinct(Jurisdiction, `Drug program`) %>%
          arrange(Jurisdiction, `Drug program`)
        rebuild_cihi_formulary_structures(dict_cihi_formularies_runtime)
        
        dict_cihi_benefits <- sort(unique(cihi$data$`Benefit status`))
        cihi_benefit_values <<- dict_cihi_benefits
        assign("dict_cihi_benefits", dict_cihi_benefits, envir = .GlobalEnv)
        
        cihi_page4_saved_selected_ids(character(0))
        
        incProgress(1, detail = "Done")
      })
      log_event("INFO", "page1_to_page2_success")
      current_page("2_upload")
    }, error = function(e) {
      shinyjs::runjs("$('#page1_loading_inline').hide();")
      shinyjs::enable("continue_btn_dpd")
      msg <- conditionMessage(e)
      msg <- gsub("\\x1b\\[[0-9;]*m", "", msg, perl = TRUE)
      log_event("ERROR", "page1_to_page2_error", list(message = msg))
      showModal(modalDialog(
        title = "Error loading data",
        paste0("Could not continue from Page 1 to Page 2. ", msg),
        easyClose = TRUE
      ))
    })
  })
  
  output$nested_atc_ui <- renderUI({
    ui_obj <- nested_atc_ui_cache()
    if (is.null(ui_obj)) {
      ui_obj <- build_nested_atc_ui()
      nested_atc_ui_cache(ui_obj)
    }
    ui_obj
  })
  
  # Enable Continue only if >=1 ATC selected
  atc_selected_flags <- reactive({
    vapply(all_atc_ids, function(id) isTRUE(input[[id]]), logical(1))
  })
  
  observe({
    req(current_page() == "3_atc")
    atc_checked <- any(atc_selected_flags())
    sync_button_enabled("continue_btn2", atc_checked)
  })
  
  # Counter
  selected_count <- reactive({
    sum(atc_selected_flags(), na.rm = TRUE)
  })
  
  # Exact matches overwrite all selections
  observeEvent(input$exact_btn, {
    target_codes <- matched_atc()
    clear_all_atc()
    session$onFlushed(function() {
      select_codes(target_codes)
    }, once = TRUE)
  })
  
  # Similar matches overwrite all selections
  observeEvent(input$similar_btn, {
    target_codes <- similar_atc()
    clear_all_atc()
    session$onFlushed(function() {
      select_codes(target_codes)
    }, once = TRUE)
  })
  
  # Render counter
  output$selected_counter <- renderText({
    paste("Number of codes selected:", selected_count())
  })
  
  # Clear all
  observeEvent(input$clear_all_btn, {
    clear_all_atc()
  })
  
  # Expand all
  observeEvent(input$expand_all, {
    session$onFlushed(function() {
      lapply(non_leaf_codes, function(code) {
        shinyjs::show(paste0("child_", code))
      })
    }, once = TRUE)
  })
  
  # Collapse all
  observeEvent(input$collapse_all, {
    session$onFlushed(function() {
      lapply(non_leaf_codes, function(code) {
        shinyjs::hide(paste0("child_", code))
      })
    }, once = TRUE)
  })
  
  # Back
  observeEvent(input$back_btn1, {
    atc_saved_selected_ids(get_selected_ids(all_atc_ids))
    current_page("2_upload")
  })
  
  # Continue
  observeEvent(input$continue_btn2, {
    selected_ids <- get_selected_ids(all_atc_ids)
    atc_saved_selected_ids(selected_ids)
    selected_codes <- unique(gsub("^atc_", "", selected_ids))
    user_atc(selected_codes)
    assign("user_atc", selected_codes, envir = .GlobalEnv)
    current_page("4_search")
  })
  
  # Restore ATC selections when returning to page 3
  observeEvent(current_page(), {
    req(current_page() == "3_atc")
    bind_atc_observers()
    selected_ids <- isolate(atc_saved_selected_ids())
    
    session$onFlushed(function() {
      lapply(all_atc_ids, function(id) {
        updateCheckboxInput(session, id, value = id %in% selected_ids)
      })
    }, once = TRUE)
  }, ignoreInit = TRUE)
  
  # Search selectors
  sync_page4_scope_ui <- function(search_all_value) {
    shinyjs::removeClass("search_all_btn", "btn-dpd-selected")
    shinyjs::removeClass("search_all_btn", "btn-dpd-faded")
    shinyjs::removeClass("search_public_btn", "btn-dpd-selected")
    shinyjs::removeClass("search_public_btn", "btn-dpd-faded")
    
    if (isTRUE(search_all_value)) {
      shinyjs::addClass("search_all_btn", "btn-dpd-selected")
      shinyjs::addClass("search_public_btn", "btn-dpd-faded")
      shinyjs::hide("page4_public_checklist_wrap")
    } else if (identical(search_all_value, FALSE)) {
      shinyjs::addClass("search_public_btn", "btn-dpd-selected")
      shinyjs::addClass("search_all_btn", "btn-dpd-faded")
      shinyjs::show("page4_public_checklist_wrap")
    } else {
      shinyjs::hide("page4_public_checklist_wrap")
    }
  }
  
  page4_benefit_selected_count <- reactive({
    if (is.null(input$page4_benefit_statuses)) {
      return(0L)
    }
    length(input$page4_benefit_statuses)
  })
  
  cihi_public_selected_count <- reactive({
    cihi_ui_revision()
    sum(vapply(cihi_child_ids, function(id) isTRUE(input[[id]]), logical(1)))
  })
  
  sync_page4_continue <- function() {
    search_all_value <- search_all()
    public_count <- cihi_public_selected_count()
    benefit_count <- page4_benefit_selected_count()
    
    can_continue <- isTRUE(search_all_value) ||
      (identical(search_all_value, FALSE) && public_count > 0 && benefit_count > 0)
    
    if (can_continue) {
      shinyjs::enable("continue_btn4")
    } else {
      shinyjs::disable("continue_btn4")
    }
  }
  
  observeEvent(input$search_all_btn, {
    req(current_page() == "4_search")
    search_all(TRUE)
    sync_page4_scope_ui(TRUE)
    sync_page4_continue()
  }, ignoreInit = TRUE)
  
  observeEvent(input$search_public_btn, {
    req(current_page() == "4_search")
    search_all(FALSE)
    sync_page4_scope_ui(FALSE)
    sync_page4_continue()
  }, ignoreInit = TRUE)
  
  # Keep page 4 continue button state in sync while on page 4
  observe({
    req(current_page() == "4_search")
    sync_page4_scope_ui(search_all())
    sync_page4_continue()
  })
  
  # Page 4 checklist UI (Jurisdiction -> Drug program)
  output$cihi_public_formulary_ui <- renderUI({
    cihi_ui_revision()
    tagList(
      lapply(cihi_nodes, function(node) {
        tagList(
          div(
            style = "display:flex; justify-content:space-between; align-items:center;
                     background-color:#f2f8ff;
                     padding:6px 10px;
                     border-radius:4px;
                     border:2px solid white;
                     margin-bottom:4px;",
            
            checkboxInput(
              node$parent_id,
              node$jurisdiction,
              FALSE
            ),
            
            actionButton(
              node$toggle_id,
              label = NULL,
              icon = icon("chevron-down"),
              style = "padding:2px 5px;"
            )
          ),
          
          hidden(
            div(
              id = node$child_wrap_id,
              style = "margin-left:20px;",
              lapply(seq_len(nrow(node$programs)), function(i) {
                checkboxInput(
                  node$programs$child_id[i],
                  node$programs$program[i],
                  FALSE
                )
              })
            )
          )
        )
      })
    )
  })
  
  # Page 4 checklist expand/collapse
  observeEvent(input$select_all_cihi, {
    lapply(cihi_all_checkbox_ids, function(id) {
      updateCheckboxInput(session, id, value = TRUE)
    })
  })
  
  observeEvent(input$clear_all_cihi, {
    lapply(cihi_all_checkbox_ids, function(id) {
      updateCheckboxInput(session, id, value = FALSE)
    })
  })
  
  observeEvent(input$expand_all_cihi, {
    session$onFlushed(function() {
      lapply(cihi_nodes, function(node) {
        shinyjs::show(node$child_wrap_id)
      })
    }, once = TRUE)
  })
  
  observeEvent(input$collapse_all_cihi, {
    session$onFlushed(function() {
      lapply(cihi_nodes, function(node) {
        shinyjs::hide(node$child_wrap_id)
      })
    }, once = TRUE)
  })
  
  # Page 4 navigation
  observeEvent(input$back_btn3, {
    cihi_page4_saved_selected_ids(get_selected_ids(cihi_child_ids))
    current_page("3_atc")
  })
  
  # Restore page 4 checklist selections when returning to page 4
  observeEvent(current_page(), {
    req(current_page() == "4_search")
    bind_cihi_observers()
    selected_child_ids <- isolate(cihi_page4_saved_selected_ids())
    skip_cihi_parent_deselect_ids(character(0))
    
    session$onFlushed(function() {
      # Reset all parents first, then restore child selections.
      lapply(cihi_parent_ids, function(id) {
        updateCheckboxInput(session, id, value = FALSE)
      })
      lapply(cihi_child_ids, function(id) {
        updateCheckboxInput(session, id, value = id %in% selected_child_ids)
      })
    }, once = TRUE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$continue_btn4, {
    req(
      isTRUE(search_all()) ||
        (
          identical(search_all(), FALSE) &&
            cihi_public_selected_count() > 0 &&
            page4_benefit_selected_count() > 0
        )
    )
    
    assign("search_all", isTRUE(search_all()), envir = .GlobalEnv)
    
    if (identical(search_all(), FALSE)) {
      selected_child_ids <- get_selected_ids(cihi_child_ids)
      
      selected_benefit <<- if (is.null(input$page4_benefit_statuses)) {
        character(0)
      } else {
        as.character(input$page4_benefit_statuses)
      }
      selected_formulary <<- unlist(lapply(cihi_nodes, function(node) {
        idx <- which(node$programs$child_id %in% selected_child_ids)
        if (length(idx) == 0) {
          return(character(0))
        }
        paste0(node$jurisdiction, "::: ", node$programs$program[idx])
      }), use.names = FALSE)
      selected_public_programs <<- unlist(lapply(cihi_nodes, function(node) {
        idx <- which(node$programs$child_id %in% selected_child_ids)
        if (length(idx) == 0) {
          return(character(0))
        }
        paste0(node$jurisdiction, " - ", node$programs$program[idx])
      }), use.names = FALSE)
    } else {
      selected_benefit <<- character(0)
      selected_formulary <<- character(0)
      selected_public_programs <<- character(0)
    }
    
    assign("selected_benefit", selected_benefit, envir = .GlobalEnv)
    assign("selected_formulary", selected_formulary, envir = .GlobalEnv)
    assign("selected_public_programs", selected_public_programs, envir = .GlobalEnv)
    
    cihi_page4_saved_selected_ids(get_selected_ids(cihi_child_ids))
    current_page("5_filters")
  })
  
  # Continue only when all three questions are answered
  observe({
    req(current_page() == "5_filters")
    
    has_q1 <- !is.null(input$page5_q1_study_period) &&
      nzchar(input$page5_q1_study_period)
    has_q2 <- !is.null(input$page5_q2_remove_nondrugs) &&
      nzchar(input$page5_q2_remove_nondrugs)
    has_q3 <- !is.null(input$page5_q3_remove_nonrx) &&
      nzchar(input$page5_q3_remove_nonrx)
    
    study_period_ok <- TRUE
    if (identical(input$page5_q1_study_period, "yes")) {
      start_raw <- trimws(if (is.null(input$page5_start_date)) "" else input$page5_start_date)
      end_raw <- trimws(if (is.null(input$page5_end_date)) "" else input$page5_end_date)
      has_both <- nzchar(start_raw) && nzchar(end_raw)
      start_date <- parse_user_date(start_raw)
      end_date <- parse_user_date(end_raw)
      start_valid <- !is.na(start_date)
      end_valid <- !is.na(end_date)
      order_ok <- start_valid && end_valid && (start_date <= end_date)
      today_ok <- end_valid && (end_date <= Sys.Date())
      study_period_ok <- has_both && start_valid && end_valid && order_ok && today_ok
    }
    
    sync_button_enabled("continue_btn5", has_q1 && has_q2 && has_q3 && study_period_ok)
  })
  
  observeEvent(input$page5_start_date, {
    req(current_page() == "5_filters")
    x <- if (is.null(input$page5_start_date)) "" else input$page5_start_date
    nx <- normalize_yyyymmdd(x)
    if (!identical(nx, x)) {
      updateTextInput(session, "page5_start_date", value = nx)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$page5_end_date, {
    req(current_page() == "5_filters")
    x <- if (is.null(input$page5_end_date)) "" else input$page5_end_date
    nx <- normalize_yyyymmdd(x)
    if (!identical(nx, x)) {
      updateTextInput(session, "page5_end_date", value = nx)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$page5_toggle_q2_info, {
    shinyjs::toggle("page5_q2_info")
  })
  
  observeEvent(input$page5_toggle_q3_info, {
    shinyjs::toggle("page5_q3_info")
  })
  
  output$page5_date_warning <- renderUI({
    if (!identical(input$page5_q1_study_period, "yes")) {
      return(NULL)
    }
    
    dpd_ref_raw <- page5_dpd_last_updated()
    cihi_ref_raw <- page5_cihi_last_updated()
    dpd_ref_date <- parse_source_date(dpd_ref_raw)
    cihi_ref_date <- suppressWarnings(
      as.Date(lubridate::ym(as.character(format_source_date(cihi_ref_raw))))
    )
    if (is.na(cihi_ref_date)) {
      cihi_ref_date <- parse_source_date(cihi_ref_raw)
    }
    
    start_raw <- trimws(if (is.null(input$page5_start_date)) "" else input$page5_start_date)
    end_raw <- trimws(if (is.null(input$page5_end_date)) "" else input$page5_end_date)
    if (!nzchar(start_raw) && !nzchar(end_raw)) {
      return(NULL)
    }
    
    start_date <- parse_user_date(start_raw)
    end_date <- parse_user_date(end_raw)
    warnings <- list()
    
    if (nzchar(start_raw) && is.na(start_date)) {
      warnings <- c(warnings, list(div(
        style = "color:#8a6d3b; margin-top:6px;",
        "Start date could not be parsed. Please use YYYY-MM-DD."
      )))
    }
    if (nzchar(end_raw) && is.na(end_date)) {
      warnings <- c(warnings, list(div(
        style = "color:#8a6d3b; margin-top:6px;",
        "End date could not be parsed. Please use YYYY-MM-DD."
      )))
    }
    
    if (!is.na(start_date) && !is.na(end_date) && start_date > end_date) {
      warnings <- c(warnings, list(div(
        style = "color:#8a6d3b; margin-top:6px;",
        "Start date cannot be later than end date."
      )))
    }
    
    end_after_today <- !is.na(end_date) && end_date > Sys.Date()
    if (end_after_today) {
      warnings <- c(warnings, list(div(
        style = "color:#8a6d3b; margin-top:6px;",
        "End date cannot be later than today's date."
      )))
    }
    
    if (is.na(end_date)) {
      if (length(warnings) == 0) {
        return(NULL)
      }
      return(tagList(warnings))
    }
    
    warn_dpd <- !is.na(dpd_ref_date) && end_date > dpd_ref_date
    warn_cihi <- !is.na(cihi_ref_date) && end_date > cihi_ref_date
    
    if (!end_after_today && (warn_dpd || warn_cihi)) {
      parts <- c()
      if (warn_dpd) {
        parts <- c(parts, paste0("DPD (last updated ", format_source_date(dpd_ref_raw), ")"))
      }
      if (warn_cihi) {
        parts <- c(parts, paste0("CIHI (last updated ", format_source_date(cihi_ref_raw), ")"))
      }
      warnings <- c(warnings, list(
        div(
          style = "color:#8a6d3b; margin-top:6px;",
          paste0(
            "Warning: End date is later than ",
            paste(parts, collapse = " and "),
            "."
          )
        )
      ))
    }
    
    if (length(warnings) == 0) {
      return(NULL)
    }
    tagList(warnings)
  })
  
  observeEvent(input$back_btn4, {
    current_page("4_search")
  })
  
  observeEvent(current_page(), {
    req(current_page() == "5_filters")
    refresh_page5_last_updated()
    session$onFlushed(function() {
      shinyjs::runjs("$('#page5_loading_inline').hide();")
    }, once = TRUE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$continue_btn5, {
    req(!is.null(input$page5_q1_study_period))
    req(!is.null(input$page5_q2_remove_nondrugs))
    req(!is.null(input$page5_q3_remove_nonrx))
    shinyjs::disable("continue_btn5")
    page6_cihi_y_selected_ids(character(0))
    cihi_atc_y_candidates(tibble::tibble())
    log_event(
      "INFO",
      "page5_to_page6_start",
      list(
        filter_time = input$page5_q1_study_period,
        filter_therapeutic = input$page5_q2_remove_nondrugs,
        filter_rx = input$page5_q3_remove_nonrx
      )
    )
    
    tryCatch({
      withProgress(message = "Preparing DIN list...", value = 0, {
        filter_time <<- identical(input$page5_q1_study_period, "yes")
        filter_therapeutic <<- identical(input$page5_q2_remove_nondrugs, "yes")
        filter_rx <<- identical(input$page5_q3_remove_nonrx, "yes")
        add_pdin <<- isTRUE(input$page5_opt_pseudodins)
        add_unmapped <<- isTRUE(input$page5_opt_unmapped_dins)
        add_route <<- isTRUE(input$page5_opt_route)
        add_schedule <<- isTRUE(input$page5_opt_schedule)
        add_biosimilar <<- isTRUE(input$page5_opt_biosimilar_flag)
        
        study_start <<- as.Date(NA)
        study_end <<- as.Date(NA)
        
        if (identical(input$page5_q1_study_period, "yes")) {
          start_raw <- trimws(if (is.null(input$page5_start_date)) "" else input$page5_start_date)
          end_raw <- trimws(if (is.null(input$page5_end_date)) "" else input$page5_end_date)
          req(nzchar(start_raw), nzchar(end_raw))
          start_date <- parse_user_date(start_raw)
          end_date <- parse_user_date(end_raw)
          req(!is.na(start_date), !is.na(end_date))
          req(start_date <= end_date)
          req(end_date <= Sys.Date())
          study_start <<- start_date
          study_end <<- end_date
        }
        
        assign("filter_time", filter_time, envir = .GlobalEnv)
        assign("study_start", study_start, envir = .GlobalEnv)
        assign("study_end", study_end, envir = .GlobalEnv)
        assign("filter_therapeutic", filter_therapeutic, envir = .GlobalEnv)
        assign("filter_rx", filter_rx, envir = .GlobalEnv)
        assign("add_pdin", add_pdin, envir = .GlobalEnv)
        assign("add_unmapped", add_unmapped, envir = .GlobalEnv)
        assign("add_route", add_route, envir = .GlobalEnv)
        assign("add_schedule", add_schedule, envir = .GlobalEnv)
        assign("add_biosimilar", add_biosimilar, envir = .GlobalEnv)
        
        dpd <- dpd_obj()
        cihi <- cihi_obj()
        if (is.null(dpd) || is.null(cihi)) {
          stop("DPD/CIHI data objects are not available. Please go back to Page 2 and continue again.")
        }
        
        selected_ids_vals <- atc_saved_selected_ids()
        selected_codes_vals <- unique(gsub("^atc_", "", selected_ids_vals))
        user_atc_vals <- if (length(selected_codes_vals) > 0) {
          selected_codes_vals
        } else {
          user_atc()
        }
        uploaded_dins_vals <- character(0)
        if (exists("initial_dins", inherits = TRUE)) {
          uploaded_dins_vals <- as.character(get("initial_dins", inherits = TRUE)$din)
          uploaded_dins_vals <- trimws(uploaded_dins_vals)
          uploaded_dins_vals <- unique(uploaded_dins_vals[nzchar(uploaded_dins_vals)])
        }
        input_din_list_val <- isTRUE(input_din_list())
        search_all_val <- isTRUE(search_all())
        
        selected_benefit_vals <- selected_benefit
        selected_formulary_vals <- selected_formulary
        
        incProgress(0.2, detail = "Preparing datasets")
        
        apply_optional_features <- function(df) {
          if (input_din_list_val) {
            df <- df %>%
              mutate(matched = as.integer(as.character(din) %in% uploaded_dins_vals))
          }
          if (!add_pdin && ("flag_pdin" %in% names(df))) {
            df <- df %>%
              filter(is.na(flag_pdin) | flag_pdin == 0) %>%
              select(-flag_pdin)
          }
          if (!add_unmapped && ("flag_unmapped" %in% names(df))) {
            df <- df %>%
              filter(flag_unmapped == 0) %>%
              select(-flag_unmapped)
          }
          if (!add_route && ("route" %in% names(df))) {
            df <- select(df, -route)
          }
          if (!add_schedule && ("schedule" %in% names(df))) {
            df <- select(df, -schedule)
          }
          if (!add_biosimilar && ("flag_biosimilar" %in% names(df))) {
            df <- select(df, -flag_biosimilar)
          }
          df
        }
        
        # Prepare datasets
        dpd_flat_for_y <- dpd_extract_all(dpd)
        dpd_dins_for_y <- unique(dpd_flat_for_y$din)
        dpd_flat <- dpd_filter_atc(dpd_flat_for_y, user_atc_vals)
        dpd_dins <- unique(dpd_flat$din)
        cihi_flat <- cihi_extract(
          cihi,
          selected_benefit_vals,
          selected_formulary_vals,
          user_atc_vals
        )
        cihi_dins <- unique(cihi_flat$din)
        
        # Apply filters
        if (filter_time) {
          marketed_codes <- dpd_filter_marketed(dpd$status, study_start, study_end)
          dpd_flat <- marketed_codes %>%
            inner_join(dpd_flat, by = "code")
          dpd_flat_for_y <- marketed_codes %>%
            inner_join(dpd_flat_for_y, by = "code")
          cihi_flat <- cihi_filter_covered(cihi_flat, study_start, study_end) %>%
            inner_join(cihi_flat, by = "din")
        }
        if (filter_therapeutic) {
          dpd_flat <- dpd_filter_therapeutic(dpd_flat, dict_dpd_route, dict_dpd_schedule, dict_dpd_atc)
          dpd_flat_for_y <- dpd_filter_therapeutic(dpd_flat_for_y, dict_dpd_route, dict_dpd_schedule, dict_dpd_atc)
          cihi_flat <- cihi_filter_therapeutic(cihi_flat, dict_dpd_atc)
        }
        if (filter_rx) {
          dpd_flat <- dpd_filter_rx(dpd_flat, dict_dpd_schedule)
          dpd_flat_for_y <- dpd_filter_rx(dpd_flat_for_y, dict_dpd_schedule)
          cihi_flat <- cihi_filter_rx(cihi_flat)
        }
        
        incProgress(0.5, detail = "Merging datasets")
        
        # Split CIHI ATC-Y candidates from non-Y rows after Page 5 filters
        cihi_flat_y <- cihi_flat %>%
          filter(stringr::str_detect(atc4, "^Y"))
        cihi_flat_non_y <- cihi_flat %>%
          filter(!stringr::str_detect(atc4, "^Y"))
        cihi_dins_non_y <- unique(cihi_flat_non_y$din)
        
        # Roll up datasets at DIN-level
        dpd_flat <- dpd_rollup_din(dpd_flat)
        dpd_dins_excluded <- setdiff(dpd_dins, unique(dpd_flat$din))
        dpd_flat_for_y <- dpd_rollup_din(dpd_flat_for_y)
        dpd_dins_excluded_for_y <- setdiff(dpd_dins_for_y, unique(dpd_flat_for_y$din))
        cihi_flat_non_y <- cihi_rollup_din(cihi_flat_non_y)
        cihi_dins_excluded <- setdiff(cihi_dins_non_y, unique(cihi_flat_non_y$din))
        cihi_flat_y <- cihi_rollup_din(cihi_flat_y)
        
        # Merge datasets
        dpd_cihi <- merged_filter(dpd_flat, cihi_dins_excluded)
        cihi_dpd <- merged_filter(cihi_flat_non_y, dpd_dins_excluded)
        if (search_all_val) {
          merged <- merged_dpd_cihi(dpd_cihi, cihi_dpd)
        } else {
          merged <- merged_cihi_dpd(cihi_dpd, dpd_cihi)
        }
        merged <- merged_format(merged, dict_who_atc)
        
        cihi_atc_y_merged <- if (nrow(cihi_flat_y) > 0) {
          cihi_y_dpd <- merged_filter(cihi_flat_y, dpd_dins_excluded_for_y)
          merged_cihi_dpd(cihi_y_dpd, dpd_flat_for_y) %>%
            merged_format(dict_who_atc) %>%
            mutate(atc4 = na_if(atc4, "Y99YY"))
        } else {
          merged[0, , drop = FALSE]
        }
        
        # Apply optional features
        merged <- apply_optional_features(merged)
        cihi_atc_y_merged <- apply_optional_features(cihi_atc_y_merged)
        cihi_atc_y_candidates(cihi_atc_y_merged)
        
        assign("dpd_flat", dpd_flat, envir = .GlobalEnv)
        assign("dpd_flat_for_y", dpd_flat_for_y, envir = .GlobalEnv)
        assign("dpd_dins", dpd_dins, envir = .GlobalEnv)
        assign("dpd_dins_for_y", dpd_dins_for_y, envir = .GlobalEnv)
        assign("dpd_dins_excluded", dpd_dins_excluded, envir = .GlobalEnv)
        assign("dpd_dins_excluded_for_y", dpd_dins_excluded_for_y, envir = .GlobalEnv)
        assign("cihi_flat", cihi_flat_non_y, envir = .GlobalEnv)
        assign("cihi_flat_y", cihi_flat_y, envir = .GlobalEnv)
        assign("cihi_dins", cihi_dins, envir = .GlobalEnv)
        assign("cihi_dins_non_y", cihi_dins_non_y, envir = .GlobalEnv)
        assign("cihi_dins_excluded", cihi_dins_excluded, envir = .GlobalEnv)
        assign("dpd_cihi", dpd_cihi, envir = .GlobalEnv)
        assign("cihi_dpd", cihi_dpd, envir = .GlobalEnv)
        assign("cihi_atc_y_candidates", cihi_atc_y_merged, envir = .GlobalEnv)
        assign("merged", merged, envir = .GlobalEnv)
        
        write_run_outputs(merged)
        
        incProgress(1, detail = "Done")
      })
      log_event("INFO", "page5_to_page6_success")
      
      current_page("6_missing_atc")
    }, error = function(e) {
      shinyjs::runjs("$('#page5_loading_inline').hide();")
      shinyjs::enable("continue_btn5")
      msg <- conditionMessage(e)
      msg <- gsub("\\x1b\\[[0-9;]*m", "", msg, perl = TRUE)
      log_event("ERROR", "page5_to_page6_error", list(message = msg))
      showModal(modalDialog(
        title = "Error preparing DIN list",
        paste0("Could not continue from Page 5 to Page 6. ", msg),
        easyClose = TRUE
      ))
    })
  })
  
  observeEvent(input$back_btn5, {
    current_page("5_filters")
  })
  
  observeEvent(input$continue_btn6, {
    current_page("7_end")
  })
  
  observeEvent(input$back_btn6, {
    current_page("6_missing_atc")
  })
  
  reset_for_new_datasets <- function() {
    dpd_choice(NULL)
    cihi_choice(NULL)
    search_all(NULL)
    uploaded_file(FALSE)
    atc_dict_uploaded(FALSE)
    page1_atc_status_message(NULL)
    page1_status_message(NULL)
    page1_has_atc_choice(NULL)
    page1_has_din_choice(NULL)
    input_din_list(FALSE)
    matched_dins(character(0))
    unmatched_dins(character(0))
    matched_atc(character(0))
    similar_atc(character(0))
    user_atc(character(0))
    cihi_atc_y_candidates(tibble::tibble())
    page6_cihi_y_selected_ids(character(0))
    atc_saved_selected_ids(character(0))
    cihi_page4_saved_selected_ids(character(0))
    
    selected_benefit <<- character(0)
    selected_formulary <<- character(0)
    selected_public_programs <<- character(0)
    
    assign("input_din_list", FALSE, envir = .GlobalEnv)
    assign("selected_benefit", selected_benefit, envir = .GlobalEnv)
    assign("selected_formulary", selected_formulary, envir = .GlobalEnv)
    assign("selected_public_programs", selected_public_programs, envir = .GlobalEnv)
  }
  
  observeEvent(input$page6_reuse_datasets_btn, {
    log_event("INFO", "page6_reuse_datasets", list(target_page = "3_atc"))
    current_page("3_atc")
  })
  
  observeEvent(input$page6_new_datasets_btn, {
    log_event("INFO", "page6_use_new_datasets", list(target_page = "1_dpd"))
    reset_for_new_datasets()
    current_page("1_dpd")
  })
  
  observeEvent(input$page6_toggle_atc, {
    shinyjs::toggle("page6_atc_wrap")
  }, ignoreInit = TRUE)
  
  atc_with_desc_from_selected <- function() {
    atc_ids <- atc_saved_selected_ids()
    atc_codes <- sort(unique(gsub("^atc_", "", atc_ids)))
    vapply(atc_codes, function(code) {
      idx <- which(dict_who_atc$atc_code == code)
      if (length(idx) > 0) {
        paste0(code, " - ", as.character(dict_who_atc$atc_name[idx[[1]]]))
      } else {
        code
      }
    }, character(1))
  }
  
  page6_atc_with_desc <- reactive({
    req(current_page() == "7_end")
    atc_with_desc_from_selected()
  })
  
  write_csv_with_notice <- function(df, file, label) {
    tryCatch({
      readr::write_csv(df, file)
      log_event("INFO", "download_success", list(label = label, file = file))
    }, error = function(e) {
      msg <- conditionMessage(e)
      msg <- gsub("\\x1b\\[[0-9;]*m", "", msg, perl = TRUE)
      log_event("ERROR", "download_error", list(label = label, file = file, message = msg))
      showNotification(
        paste0(label, " failed. If the file is open, close it and try again. ", msg),
        type = "error",
        duration = NULL
      )
      stop(msg)
    })
  }
  
  build_page6_summary_df <- function(expand_atc = FALSE, appended_dins = character(0)) {
    yes_no <- function(x) {
      if (isTRUE(x)) "Yes" else if (identical(x, FALSE)) "No" else "Not answered"
    }
    collapse_or_na <- function(x) {
      x <- as.character(x)
      x <- x[nzchar(trimws(x))]
      if (length(x) == 0) "None" else paste(x, collapse = ", ")
    }
    add_row <- function(items, values, item, value) {
      list(items = c(items, item), values = c(values, value))
    }
    
    atc_with_desc <- atc_with_desc_from_selected()
    atc_codes <- sub(" - .*", "", atc_with_desc)
    
    atc_file_name <- if (!is.null(input$atc_dict_upload)) {
      input$atc_dict_upload$name
    } else if (identical(page1_has_atc_choice(), "yes")) {
      "Cached ATC dictionary/checklist"
    } else if (!is.null(page1_atc_status_message()) && grepl("Saved and using uploaded ATC dictionary file:", page1_atc_status_message(), fixed = TRUE)) {
      sub("^Saved and using uploaded ATC dictionary file: (.*) \\(saved to .*\\)\\.$", "\\1", page1_atc_status_message())
    } else {
      "Unknown"
    }
    din_file_name <- if (isTRUE(input_din_list()) && !is.null(input$file_upload)) {
      input$file_upload$name
    } else if (isTRUE(input_din_list()) && !is.null(page1_status_message()) && grepl("Saved and using uploaded DIN file:", page1_status_message(), fixed = TRUE)) {
      sub("^Saved and using uploaded DIN file: (.*) \\(saved to .*\\)\\.$", "\\1", page1_status_message())
    } else {
      "Not uploaded"
    }
    
    n_uploaded_dins <- if (exists("initial_dins", inherits = TRUE)) {
      nrow(get("initial_dins", inherits = TRUE))
    } else {
      0L
    }
    n_matched_dins <- length(unique(as.character(matched_dins())))
    
    search_scope_label <- if (isTRUE(search_all())) {
      "Search all drugs"
    } else if (identical(search_all(), FALSE)) {
      "Search publicly covered drugs"
    } else {
      "Not selected"
    }
    
    dpd_last_updated_for_summary <- page5_dpd_last_updated()
    cihi_last_updated_for_summary <- page5_cihi_last_updated()
    
    items <- character(0)
    values <- character(0)
    
    out <- add_row(items, values, "DPD data", if (identical(dpd_choice(), "download")) "Download latest DPD" else "Use existing DPD")
    items <- out$items; values <- out$values
    
    out <- add_row(items, values, "CIHI data", if (identical(cihi_choice(), "download")) "Download latest formularies" else "Use existing formularies")
    items <- out$items; values <- out$values
    
    out <- add_row(items, values, "DPD last updated", format_source_date(dpd_last_updated_for_summary))
    items <- out$items; values <- out$values
    
    out <- add_row(items, values, "CIHI last updated", format_source_date(cihi_last_updated_for_summary))
    items <- out$items; values <- out$values
    
    out <- add_row(items, values, "ATC dictionary file", atc_file_name)
    items <- out$items; values <- out$values
    
    if (isTRUE(input_din_list())) {
      out <- add_row(items, values, "Existing DIN list file", din_file_name)
      items <- out$items; values <- out$values
      
      out <- add_row(items, values, "Number of uploaded DINs", as.character(n_uploaded_dins))
      items <- out$items; values <- out$values
      
      out <- add_row(items, values, "Number of uploaded DINs with unknown ATC", as.character(n_uploaded_dins - n_matched_dins))
      items <- out$items; values <- out$values
    }
    
    out <- add_row(items, values, "Search scope", search_scope_label)
    items <- out$items; values <- out$values
    
    if (identical(search_all(), FALSE)) {
      benefits <- as.character(selected_benefit)
      benefits <- benefits[nzchar(trimws(benefits))]
      if (length(benefits) == 0) {
        out <- add_row(items, values, "Selected benefit statuses", "None")
        items <- out$items; values <- out$values
      } else {
        for (b in benefits) {
          out <- add_row(items, values, "Selected benefit statuses", b)
          items <- out$items; values <- out$values
        }
      }
      
      programs <- as.character(selected_public_programs)
      programs <- programs[nzchar(trimws(programs))]
      if (length(programs) == 0) {
        out <- add_row(items, values, "Selected public drug programs", "None")
        items <- out$items; values <- out$values
      } else {
        for (p in programs) {
          out <- add_row(items, values, "Selected public drug programs", p)
          items <- out$items; values <- out$values
        }
      }
    }
    
    out <- add_row(items, values, "Restrict to study period", yes_no(filter_time))
    items <- out$items; values <- out$values
    
    if (isTRUE(filter_time) && !is.na(study_start) && !is.na(study_end)) {
      out <- add_row(items, values, "Study start date", as.character(study_start))
      items <- out$items; values <- out$values
      out <- add_row(items, values, "Study end date", as.character(study_end))
      items <- out$items; values <- out$values
    }
    
    out <- add_row(items, values, "Remove non-drugs and non-human drugs", yes_no(filter_therapeutic))
    items <- out$items; values <- out$values
    out <- add_row(items, values, "Remove non-prescription drugs", yes_no(filter_rx))
    items <- out$items; values <- out$values
    out <- add_row(items, values, "Include pseudoDINs", yes_no(add_pdin))
    items <- out$items; values <- out$values
    out <- add_row(items, values, "Include unmapped DINs", yes_no(add_unmapped))
    items <- out$items; values <- out$values
    out <- add_row(items, values, "Include route field", yes_no(add_route))
    items <- out$items; values <- out$values
    out <- add_row(items, values, "Include schedule field", yes_no(add_schedule))
    items <- out$items; values <- out$values
    out <- add_row(items, values, "Include biosimilar flag field", yes_no(add_biosimilar))
    items <- out$items; values <- out$values
    
    if (expand_atc) {
      if (length(atc_with_desc) == 0) {
        out <- add_row(items, values, "ATC selection", "None")
        items <- out$items; values <- out$values
      } else {
        for (x in atc_with_desc) {
          out <- add_row(items, values, "ATC selection", x)
          items <- out$items; values <- out$values
        }
      }
    } else {
      out <- add_row(
        items,
        values,
        "ATC selections",
        paste0(length(atc_codes), " selected. Click the button below to show.")
      )
      items <- out$items; values <- out$values
    }
    
    appended_dins <- unique(trimws(as.character(appended_dins)))
    appended_dins <- appended_dins[nzchar(appended_dins)]
    if (length(appended_dins) > 0) {
      for (din_value in appended_dins) {
        out <- add_row(items, values, "Appended DIN with missing ATC", din_value)
        items <- out$items; values <- out$values
      }
    }
    
    data.frame(Item = items, Selection = values, stringsAsFactors = FALSE)
  }
  
  write_run_outputs <- function(merged_df) {
    tryCatch({
      dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)
      
      summary_df <- build_page6_summary_df(expand_atc = TRUE)
      readr::write_csv(summary_df, file.path(run_output_dir, "summary.csv"))
      readr::write_csv(merged_df, file.path(run_output_dir, "merged.csv"))
      
      params <- list(
        run_id = run_id,
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        dpd_choice = dpd_choice(),
        cihi_choice = cihi_choice(),
        search_all = isTRUE(search_all()),
        input_din_list = isTRUE(input_din_list()),
        filter_time = filter_time,
        study_start = if (is.na(study_start)) NA_character_ else as.character(study_start),
        study_end = if (is.na(study_end)) NA_character_ else as.character(study_end),
        filter_therapeutic = filter_therapeutic,
        filter_rx = filter_rx,
        add_pdin = add_pdin,
        add_unmapped = add_unmapped,
        add_route = add_route,
        add_schedule = add_schedule,
        add_biosimilar = add_biosimilar,
        selected_benefit = as.character(selected_benefit),
        selected_public_programs = as.character(selected_public_programs),
        selected_atc = as.character(atc_with_desc_from_selected())
      )
      
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::write_json(
          params,
          file.path(run_output_dir, "params.json"),
          auto_unbox = TRUE,
          pretty = TRUE,
          null = "null"
        )
      } else {
        readr::write_lines(
          capture.output(str(params)),
          file.path(run_output_dir, "params.txt")
        )
      }
      
      assign("run_output_dir", run_output_dir, envir = .GlobalEnv)
      log_event(
        "INFO",
        "run_outputs_written",
        list(path = run_output_dir, merged_rows = nrow(merged_df), summary_rows = nrow(summary_df))
      )
    }, error = function(e) {
      msg <- conditionMessage(e)
      msg <- gsub("\\x1b\\[[0-9;]*m", "", msg, perl = TRUE)
      log_event("ERROR", "run_outputs_write_error", list(path = run_output_dir, message = msg))
      showNotification(
        paste0("Automatic output copy failed for this run at ", run_output_dir, ". ", msg),
        type = "warning",
        duration = 10
      )
    })
  }
  
  page6_appended_missing_atc_dins <- reactive({
    y_df <- cihi_atc_y_candidates()
    if (is.null(y_df) || nrow(y_df) == 0 || !"din" %in% names(y_df)) {
      return(character(0))
    }
    checkbox_ids <- paste0("page6_cihi_y_row_", seq_len(nrow(y_df)))
    keep <- vapply(checkbox_ids, function(id) isTRUE(input[[id]]), logical(1))
    dins <- as.character(y_df$din[keep])
    dins <- trimws(dins)
    unique(dins[nzchar(dins)])
  })
  
  page6_summary_table_df <- reactive({
    build_page6_summary_df(
      expand_atc = FALSE,
      appended_dins = page6_appended_missing_atc_dins()
    )
  })
  
  page6_summary_download_df <- reactive({
    build_page6_summary_df(
      expand_atc = TRUE,
      appended_dins = page6_appended_missing_atc_dins()
    )
  })
  
  output$page6_summary_table <- renderTable({
    page6_summary_table_df()
  }, bordered = TRUE, striped = TRUE, spacing = "s")
  
  output$page6_download_summary <- downloadHandler(
    filename = function() {
      paste0("rdinexplore_summary_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv_with_notice(page6_summary_download_df(), file, "Summary CSV download")
    }
  )
  
  output$page6_atc_list <- renderUI({
    req(current_page() == "7_end")
    atc_with_desc <- page6_atc_with_desc()
    
    if (length(atc_with_desc) == 0) {
      return(div("No ATC codes selected."))
    }
    
    tags$ul(
      lapply(atc_with_desc, tags$li)
    )
  })
  
  arrange_like_merged_format <- function(df) {
    if (is.null(df) || ncol(df) == 0) {
      return(df)
    }
    leading_cols <- c(
      "atc4",
      "atc4_descriptor",
      "din",
      "api",
      "strength",
      "formulation",
      "route",
      "brand_name",
      "schedule",
      "flag_biosimilar",
      "flag_pdin",
      "flag_unmapped"
    )
    sort_cols <- c("atc4", "api", "strength", "formulation", "brand_name", "din")
    
    df %>%
      dplyr::relocate(dplyr::any_of(leading_cols)) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(sort_cols)))
  }
  
  output$page6_cihi_atc_y_candidates_ui <- renderUI({
    req(current_page() == "6_missing_atc")
    y_df <- cihi_atc_y_candidates()
    if (is.null(y_df) || nrow(y_df) == 0) {
      return(
        div(
          style = "background-color:#f7f7f7; border:1px solid #ececec; border-radius:4px; padding:10px; margin-bottom:12px;",
          strong("No drugs in CIHI with missing ATC to review.")
        )
      )
    }
    
    checkbox_ids <- paste0("page6_cihi_y_row_", seq_len(nrow(y_df)))
    selected_ids <- page6_cihi_y_selected_ids()
    column_class <- function(col_name) {
      if (identical(col_name, "api")) {
        return("api-col")
      }
      if (identical(col_name, "strength")) {
        return("strength-col")
      }
      NULL
    }
    row_cells <- function(i) {
      tags$tr(
        tags$td(
          class = "append-col",
          checkboxInput(
            inputId = checkbox_ids[[i]],
            label = NULL,
            value = checkbox_ids[[i]] %in% selected_ids,
            width = "18px"
          )
        ),
        lapply(names(y_df), function(col) {
          value_chr <- as.character(y_df[[col]][[i]])
          tags$td(
            class = column_class(col),
            title = if (identical(col, "api") || identical(col, "strength")) value_chr else NULL,
            value_chr
          )
        })
      )
    }
    
    div(
      style = "background-color:#f7f7f7; border:1px solid #ececec; border-radius:4px; padding:10px; margin-bottom:12px;",
      tags$style(HTML("
        .cihi-y-scroll-shell {
          position: relative;
        }
        .cihi-y-scroll-wrap {
          overflow-x: auto;
        }
        .cihi-y-table {
          table-layout: auto;
          width: max-content;
          min-width: 100%;
          margin-bottom: 0;
        }
        .cihi-y-table th,
        .cihi-y-table td {
          white-space: nowrap;
          vertical-align: top;
        }
        .cihi-y-table th.append-col,
        .cihi-y-table td.append-col {
          min-width: 88px;
          width: 88px;
          max-width: 88px;
          position: sticky;
          left: 0;
          z-index: 3;
          text-align: center;
          background-color: #f8f8f8;
          padding-left: 6px;
          padding-right: 6px;
        }
        .cihi-y-table thead th.append-col {
          z-index: 4;
          background-color: #f0f0f0;
        }
        .cihi-y-table th.api-col,
        .cihi-y-table td.api-col {
          width: 260px;
          max-width: 260px;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .cihi-y-table th.strength-col,
        .cihi-y-table td.strength-col {
          width: 220px;
          max-width: 220px;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .cihi-y-table td.append-col .form-group,
        .cihi-y-table td.append-col .checkbox {
          margin: 0;
        }
        .cihi-y-table td.append-col label {
          margin: 0;
          padding-left: 0;
        }
        .cihi-y-table td.append-col input[type='checkbox'] {
          position: static;
          margin: 0;
        }
        .cihi-y-scroll-indicator {
          position: absolute;
          top: 0;
          bottom: 0;
          width: 44px;
          display: none;
          align-items: center;
          color: #666;
          font-size: 16px;
          pointer-events: none;
          z-index: 6;
        }
        .cihi-y-scroll-indicator.left {
          left: 88px;
          padding-left: 6px;
          background: linear-gradient(to right, rgba(247,247,247,0.98), rgba(247,247,247,0));
        }
        .cihi-y-scroll-indicator.right {
          right: 0;
          justify-content: flex-end;
          padding-right: 6px;
          background: linear-gradient(to left, rgba(247,247,247,0.98), rgba(247,247,247,0));
        }
        .cihi-y-scroll-shell.has-overflow.show-left .cihi-y-scroll-indicator.left {
          display: flex;
        }
        .cihi-y-scroll-shell.has-overflow.show-right .cihi-y-scroll-indicator.right {
          display: flex;
        }
      ")),
      tags$script(HTML("
        (function () {
          var shell = document.getElementById('page6_cihi_y_scroll_shell');
          var wrap = document.getElementById('page6_cihi_y_scroll_wrap');
          if (!shell || !wrap) return;
          var update = function () {
            var maxScroll = wrap.scrollWidth - wrap.clientWidth;
            var hasOverflow = maxScroll > 2;
            shell.classList.toggle('has-overflow', hasOverflow);
            shell.classList.toggle('show-left', hasOverflow && wrap.scrollLeft > 2);
            shell.classList.toggle('show-right', hasOverflow && wrap.scrollLeft < maxScroll - 2);
          };
          if (!wrap.dataset.scrollBound) {
            wrap.addEventListener('scroll', update);
            wrap.dataset.scrollBound = '1';
          }
          setTimeout(update, 0);
        })();
      ")),
      strong("Select rows to append to the downloadable DIN list (optional append):"),
      p("Hover over `api` to see full ingredient lists. Shift + scroll to see more columns if they do not all fit on one screen."),
      div(
        style = "margin-bottom:8px;",
        actionButton("page6_cihi_y_select_all", "Select all", class = "btn-clear"),
        actionButton("page6_cihi_y_clear_all", "Clear all", class = "btn-clear")
      ),
      div(
        id = "page6_cihi_y_scroll_shell",
        class = "cihi-y-scroll-shell",
        div(
          id = "page6_cihi_y_scroll_wrap",
          class = "cihi-y-scroll-wrap",
          tags$table(
            class = "table table-striped table-bordered cihi-y-table",
            tags$thead(
              tags$tr(
                tags$th(class = "append-col", "append"),
                lapply(names(y_df), function(col) {
                  tags$th(class = column_class(col), col)
                })
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(y_df)), row_cells)
            )
          )
        ),
        div(class = "cihi-y-scroll-indicator left", icon("chevron-left")),
        div(class = "cihi-y-scroll-indicator right", icon("chevron-right"))
      )
    )
  })
  
  observeEvent(input$page6_cihi_y_select_all, {
    req(current_page() == "6_missing_atc")
    y_df <- cihi_atc_y_candidates()
    if (is.null(y_df) || nrow(y_df) == 0) {
      return(NULL)
    }
    checkbox_ids <- paste0("page6_cihi_y_row_", seq_len(nrow(y_df)))
    page6_cihi_y_selected_ids(checkbox_ids)
    lapply(checkbox_ids, function(id) {
      if (id %in% names(input)) {
        updateCheckboxInput(session, id, value = TRUE)
      }
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$page6_cihi_y_clear_all, {
    req(current_page() == "6_missing_atc")
    y_df <- cihi_atc_y_candidates()
    if (is.null(y_df) || nrow(y_df) == 0) {
      return(NULL)
    }
    checkbox_ids <- paste0("page6_cihi_y_row_", seq_len(nrow(y_df)))
    page6_cihi_y_selected_ids(character(0))
    lapply(checkbox_ids, function(id) {
      if (id %in% names(input)) {
        updateCheckboxInput(session, id, value = FALSE)
      }
    })
  }, ignoreInit = TRUE)
  
  observeEvent(cihi_atc_y_candidates(), {
    y_df <- cihi_atc_y_candidates()
    if (is.null(y_df) || nrow(y_df) == 0) {
      page6_cihi_y_selected_ids(character(0))
      return(NULL)
    }
    valid_ids <- paste0("page6_cihi_y_row_", seq_len(nrow(y_df)))
    page6_cihi_y_selected_ids(intersect(page6_cihi_y_selected_ids(), valid_ids))
  }, ignoreInit = TRUE)
  
  observe({
    y_df <- cihi_atc_y_candidates()
    if (is.null(y_df) || nrow(y_df) == 0) {
      return(NULL)
    }
    checkbox_ids <- paste0("page6_cihi_y_row_", seq_len(nrow(y_df)))
    if (!all(checkbox_ids %in% names(input))) {
      return(NULL)
    }
    selected_ids <- checkbox_ids[vapply(checkbox_ids, function(id) isTRUE(input[[id]]), logical(1))]
    page6_cihi_y_selected_ids(selected_ids)
  })
  
  page6_selected_cihi_atc_y_rows <- reactive({
    y_df <- cihi_atc_y_candidates()
    if (is.null(y_df) || nrow(y_df) == 0) {
      if (is.null(y_df)) {
        return(tibble::tibble())
      }
      return(y_df[0, , drop = FALSE])
    }
    
    checkbox_ids <- paste0("page6_cihi_y_row_", seq_len(nrow(y_df)))
    keep <- checkbox_ids %in% page6_cihi_y_selected_ids()
    y_df[keep, , drop = FALSE]
  })
  
  page6_final_merged_df <- reactive({
    base_df <- if (exists("merged", inherits = TRUE)) {
      get("merged", inherits = TRUE)
    } else {
      tibble::tibble()
    }
    y_selected_df <- page6_selected_cihi_atc_y_rows()
    if (nrow(y_selected_df) == 0) {
      return(base_df)
    }
    
    dplyr::bind_rows(base_df, y_selected_df) %>%
      dplyr::distinct() %>%
      arrange_like_merged_format()
  })
  
  output$page6_merged_rows <- renderText({
    req(current_page() == "7_end")
    merged_df <- page6_final_merged_df()
    paste0("Rows: ", nrow(merged_df))
  })
  
  output$page6_merged_cols_count <- renderText({
    req(current_page() == "7_end")
    merged_df <- page6_final_merged_df()
    paste0("Columns: ", ncol(merged_df))
  })
  
  output$page6_merged_stats <- renderUI({
    req(current_page() == "7_end")
    merged_df <- page6_final_merged_df()
    n_pdin <- if ("flag_pdin" %in% names(merged_df)) sum(merged_df$flag_pdin == 1, na.rm = TRUE) else 0
    n_unmapped <- if ("flag_unmapped" %in% names(merged_df)) sum(merged_df$flag_unmapped == 1, na.rm = TRUE) else 0
    n_din <- nrow(merged_df) - n_pdin - n_unmapped
    
    tags$ul(
      tags$li(paste0("Number of DINs: ", n_din)),
      tags$li(paste0("Number of pseudoDINs: ", n_pdin)),
      tags$li(paste0("Number of unmapped DINs: ", n_unmapped))
    )
  })
  
  output$page6_merged_columns <- renderUI({
    req(current_page() == "7_end")
    merged_df <- page6_final_merged_df()
    if (ncol(merged_df) == 0) {
      return(tags$ul(tags$li("None")))
    }
    cols <- names(merged_df)
    if (length(cols) == 0) {
      return(tags$ul(tags$li("None")))
    }
    tags$ul(lapply(cols, tags$li))
  })
  
  output$page6_download_merged <- downloadHandler(
    filename = function() {
      paste0("rdinexplore_dinlist_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv_with_notice(page6_final_merged_df(), file, "Merged CSV download")
    }
  )
  
  output$page_progress_ui <- renderUI({
    idx <- match(current_page(), page_order)
    if (is.na(idx)) {
      return(NULL)
    }
    pct <- round(100 * idx / length(page_order))
    
    div(
      class = "progress-footer",
      div(
        style = "margin:0;",
        paste0("Page ", idx, " of ", length(page_order))
      ),
      div(
        class = "progress",
        style = "height:10px; margin:6px 0 0 0;",
        div(
          class = "progress-bar",
          role = "progressbar",
          style = paste0("width:", pct, "%; background-color:#A9D6F5;")
        )
      )
    )
  })
  
  ##############################################################################
  # Render pages dynamically
  output$page_content <- renderUI({
    
    # Page 2: Upload files
    if (current_page() == "2_upload") {
      page2_download_messages <- character(0)
      if (identical(dpd_choice(), "download")) {
        page2_download_messages <- c(
          page2_download_messages,
          paste0(
            "Downloaded latest DPD (last updated: ",
            format_source_date(dpd_last_updated()),
            ")."
          )
        )
      }
      if (identical(cihi_choice(), "download")) {
        page2_download_messages <- c(
          page2_download_messages,
          paste0(
            "Downloaded latest formularies (last updated: ",
            format_source_date(cihi_last_updated()),
            ")."
          )
        )
      }
      
      tagList(
        if (length(page2_download_messages) > 0) {
          div(
            style = "background-color:#f7f7f7; border:1px solid #ececec; border-radius:4px; padding:10px; margin-bottom:12px;",
            lapply(page2_download_messages, function(msg) {
              div(msg)
            })
          )
        },
        div(
          style = "display:flex; align-items:center; gap:6px;",
          strong("Use existing cached ATC dictionary/checklist:"),
          tags$span(
            icon("circle-question"),
            title = paste(
              "Yes: loads cached ATC checklist from ./data/atc_checklist_cached.rds.",
              "No: upload ATC dictionary CSV and cache the processed checklist."
            ),
            style = "cursor:pointer;"
          )
        ),
        radioButtons(
          "page1_use_existing_atc",
          label = NULL,
          choices = c("Yes" = "yes", "No" = "no"),
          selected = isolate(
            if (is.null(page1_has_atc_choice())) character(0) else page1_has_atc_choice()
          ),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.page1_use_existing_atc == 'no'",
        div(style = "height:6px;"),
        div(
          style = "display:flex; align-items:center; gap:8px;",
          p("Upload ATC dictionary (CSV):", style = "margin:0;"),
          actionButton(
            "page1_toggle_atc_info",
            label = tagList("Details ", icon("chevron-down")),
            class = "btn-clear",
            style = "padding:2px 8px;"
          )
        ),
        hidden(
          div(
            id = "page1_atc_info",
            style = "margin-top:8px; margin-bottom:10px; background-color:#f7f7f7; padding:8px 10px; border-radius:4px;",
            paste(
              "The WHO ATC classification system classifies active substances into a hierarchy with five levels.",
              "For example, metformin is represented by A10BA02, where A is the first level, A10 is the second level, and so on.",
              "For active substances with multiple indications, only the main indication is usually assigned.",
              "However, those with different indications at different strengths or routes can receive multiple ATC codes.",
              "Given these nuances, there is a risk of misclassification when only considering ATC, as this tool does.",
              "Use with discretion."
            ),
            tags$br(),
            tags$br(),
            paste(
              "There are two ways to obtain the full ATC dictionary in tabular format.",
              "For commercial uses, you should purchase a copy from WHO for \u20AC200:"
            ),
            tags$a(
              href = "https://www.whocc.no/atc_ddd_index_and_guidelines/order/",
              target = "_blank",
              "https://www.whocc.no/atc_ddd_index_and_guidelines/order/"
            ),
            "For non-commercial uses, you may use the web-scraping tool developed by Fabrício Kury, which makes use of the fact that the WHO publishes the same data for free on their website but not in tabular format:",
            tags$a(
              href = "https://github.com/fabkury/atcd",
              target = "_blank",
              "https://github.com/fabkury/atcd"
            ),
            "Note that although the DPD has an alternative classification system, ASHP AHFS, it is strictly proprietary."
          )
        ),
        div(style = "height:6px;"),
        fileInput(
          "atc_dict_upload",
          label = NULL,
          multiple = FALSE,
          accept = c(".csv")
        ),
        ),
        uiOutput("page1_atc_upload_status"),
        br(),
        div(
          style = "display:flex; align-items:center; gap:6px;",
          strong("Do you want to upload a DIN list (CSV or Excel):"),
          tags$span(
            icon("circle-question"),
            title = paste(
              "DINs will be linked to DPD to get their ATC4 codes, if available.",
              "Later, when selecting ATC codes, you will be given the opportunity to select exact ATC4 matches to the DINs you provided",
              "You will also be given the opportunity to select similar ATC4 matches (same ATC3 roots) to the DINs you provided.",
              "A 'matched' column will be appended to the DIN list {0: not an uploaded DIN, 1: an uploaded DIN}.",
              "Invalid DINs (e.g., pseudoDINs) will be ignored."
            ),
            style = "cursor:pointer;"
          )
        ),
        radioButtons(
          "page1_has_din_list",
          label = NULL,
          choices = c("Yes" = "yes", "No" = "no"),
          selected = isolate(
            if (is.null(page1_has_din_choice())) character(0) else page1_has_din_choice()
          ),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.page1_has_din_list == 'yes'",
          fileInput(
            "file_upload",
            label = NULL,
            multiple = FALSE,
            accept = c(".csv", ".xls", ".xlsx")
          ),
          uiOutput("page1_upload_status")
        ),
        br(),
        div(
          style = "display:flex; align-items:center; gap:10px;",
          actionButton("back_btn2", "Back"),
          uiOutput("page1_continue_btn")
        )
      )
      
    } else if (current_page() == "1_dpd") {
      selected_dpd <- dpd_choice()
      selected_cihi <- cihi_choice()
      can_continue_page2 <- !is.null(selected_dpd) && !is.null(selected_cihi)
      
      dpd_download_class <- if (identical(selected_dpd, "download")) {
        "btn-dpd-selected"
      } else if (identical(selected_dpd, "no_download")) {
        "btn-dpd-faded"
      } else {
        ""
      }
      dpd_no_download_class <- if (identical(selected_dpd, "no_download")) {
        "btn-dpd-selected"
      } else if (identical(selected_dpd, "download")) {
        "btn-dpd-faded"
      } else {
        ""
      }
      
      cihi_download_class <- if (identical(selected_cihi, "download")) {
        "btn-dpd-selected"
      } else if (identical(selected_cihi, "no_download")) {
        "btn-dpd-faded"
      } else {
        ""
      }
      cihi_no_download_class <- if (identical(selected_cihi, "no_download")) {
        "btn-dpd-selected"
      } else if (identical(selected_cihi, "download")) {
        "btn-dpd-faded"
      } else {
        ""
      }
      
      tagList(
        div(
          style = "display:flex; align-items:center; gap:6px;",
          h3("HC Drug Product Database", style = "margin:0 0 8px 0;"),
          tags$span(
            icon("circle-question"),
            title = "Even if you only wish to search publicly covered drugs using CIHI data, most of the data pulled comes from DPD.",
            style = "cursor:pointer;"
          )
        ),
        p(strong("Last downloaded: "), format_source_date(dpd_last_downloaded())),
        p(strong("Last updated: "), format_source_date(dpd_last_updated())),
        p("Select one option to continue:"),
        
        div(
          class = "dpd-choice-wrap",
          
          div(
            style = "flex:1;",
            actionButton(
              "dpd_download_btn",
              "Download latest DPD",
              class = paste("dpd-choice-btn", dpd_download_class)
            )
          ),
          
          div(
            style = "flex:1;",
            actionButton(
              "dpd_no_download_btn",
              "Use existing DPD",
              class = paste("dpd-choice-btn", dpd_no_download_class)
            )
          )
        ),
        
        div(
          style = "display:flex; align-items:center; gap:6px;",
          h3("CIHI Public Formulary Coverage Data", style = "margin:0 0 8px 0;"),
          tags$span(
            icon("circle-question"),
            title = "Even if you wish to search all drugs on the Canadian market using DPD, some filters may use formulary data.",
            style = "cursor:pointer;"
          )
        ),
        p(strong("Last downloaded: "), format_source_date(cihi_last_downloaded())),
        p(strong("Last updated: "), format_source_date(cihi_last_updated())),
        p("Select one option to continue:"),
        
        div(
          class = "dpd-choice-wrap",
          
          div(
            style = "flex:1;",
            actionButton(
              "cihi_download_btn",
              "Download latest formularies",
              class = paste("dpd-choice-btn", cihi_download_class)
            )
          ),
          
          div(
            style = "flex:1;",
            actionButton(
              "cihi_no_download_btn",
              "Use existing formularies",
              class = paste("dpd-choice-btn", cihi_no_download_class)
            )
          )
        ),
        
        br(),
        div(
          style = "display:flex; align-items:center; gap:10px;",
          if (can_continue_page2) {
            actionButton(
              "continue_btn_dpd",
              "Continue",
              onclick = "$('#page1_loading_inline').show();"
            )
          } else {
            disabled(actionButton("continue_btn_dpd", "Continue"))
          },
          div(
            id = "page1_loading_inline",
            style = "display:none; color:#3CA0D6; font-weight:600;",
            icon("spinner", class = "fa-spin"),
            " Loading..."
          )
        )
      )
      
      # Page 3: Select ATC's similar to uploaded DIN list
    } else if (current_page() == "3_atc") {
      page3_unmatched_dins <- character(0)
      
      if (isTRUE(input_din_list())) {
        unmatched_chr <- as.character(unmatched_dins())
        unmatched_chr <- trimws(unmatched_chr)
        unmatched_chr <- unmatched_chr[nzchar(unmatched_chr)]
        if (length(unmatched_chr) > 0) {
          page3_unmatched_dins <- unmatched_chr
        }
      }
      
      tagList(
        if (length(page3_unmatched_dins) > 0) {
          div(
            style = "background-color:#f7f7f7; border:1px solid #ececec; border-radius:4px; padding:10px; margin-bottom:12px;",
            paste0(
              "Uploaded DINs with missing ATCs: ",
              paste(page3_unmatched_dins, collapse = ", ")
            )
          )
        },
        h3("Select ATC Codes"),
        
        if (uploaded_file()) {
          div(
            style = "display:flex; gap:40px; align-items:center;",
            
            div(
              actionButton("exact_btn", "Select exact matches"),
              tags$span(
                icon("circle-question"),
                title = paste(
                  "Clears all. Using your uploaded DIN list, finds the relevant ATC4 from DPD (in rare cases, only ATC2 or ATC3 are available) and selects them exactly as found.",
                  "Example: if you provided DIN 02415089, this selects the matching ATC4 on DPD (A10AB)."
                ),
                style = "margin-left:5px; cursor:pointer;"
              )
            ),
            
            div(
              actionButton("similar_btn", "Select similar matches"),
              tags$span(
                icon("circle-question"),
                title = paste(
                  "Clears all. Using your uploaded DIN list, finds the relevant ATC4 from DPD (in rare cases, only ATC2 or ATC3 are available) and selects all ATC4's with the same ATC3 roots.",
                  "Example: if you provided DIN 02415089, this selects the matching ATC4 on DPD (A10AB) and all other A10A categories."
                ),
                style = "margin-left:5px; cursor:pointer;"
              )
            )
          )
        },
        
        br(),
        
        # Counter
        strong(textOutput("selected_counter", inline = TRUE)),
        
        br(),
        
        # Clear all
        actionButton(
          "clear_all_btn",
          "Clear all",
          class = "btn-clear"
        ),
        
        br(), br(),
        
        div(
          style = "margin-bottom:10px;",
          actionButton("expand_all", "Expand all"),
          actionButton("collapse_all", "Collapse all")
        ),
        
        br(),
        p("Expand all and use Ctrl + F to find a drug class of interest"),
        
        uiOutput("nested_atc_ui"),
        
        br(),
        
        actionButton("back_btn1", "Back"),
        actionButton("continue_btn2", "Continue")
      )
      
    } else if (current_page() == "4_search") {
      tagList(
        h3("Drugs to Search"),
        p("Select one option to continue:"),
        
        div(
          class = "dpd-choice-wrap",
          div(
            style = "flex:1;",
            actionButton(
              "search_all_btn",
              "Search all drugs",
              class = "dpd-choice-btn"
            )
          ),
          div(
            style = "flex:1;",
            actionButton(
              "search_public_btn",
              "Search publicly covered drugs",
              class = "dpd-choice-btn"
            )
          )
        ),
        
        div(
          id = "page4_public_checklist_wrap",
          style = "display:none;",
          div(
            style = "display:flex; align-items:center; gap:6px; margin-bottom:8px;",
            strong("Select formulary benefits:"),
            tags$span(
              icon("circle-question"),
              title = paste(
                "Selecting more than one benefit status will pool them.",
                "There is currently no functionality to distinguish the benefit statuses because this tool focuses on the DIN level, not DIN-benefit level.",
                "If you that is your goal, you can either run one benefit status at a time, or run all and then compare to the raw CIHI public formulary coverage dataset found in ./data/cihi."
              ),
              style = "cursor:pointer;"
            )
          ),
          checkboxGroupInput(
            "page4_benefit_statuses",
            label = NULL,
            choices = cihi_benefit_values,
            selected = character(0)
          ),
          br(),
          div(
            style = "display:flex; align-items:center; gap:6px; margin-bottom:8px;",
            strong("Select jurisdictions and public drug programs:"),
            tags$span(
              icon("circle-question"),
              title = paste(
                "Selecting more than one jurisdiction and/or public drug program will pool them.",
                "There is currently no functionality to distinguish the jurisdictions/programs because this tool focuses on the DIN level, not DIN-jurisdiction/program level.",
                "If you that is your goal, you can either run one jurisdiction/program at a time, or run all and then compare to the raw CIHI public formulary coverage dataset found in ./data/cihi."
              ),
              style = "cursor:pointer;"
            )
          ),
          div(
            style = "margin-bottom:6px;",
            actionButton("select_all_cihi", "Select all", class = "btn-clear"),
            actionButton("clear_all_cihi", "Clear all", class = "btn-clear")
          ),
          div(
            style = "margin-bottom:10px;",
            actionButton("expand_all_cihi", "Expand all"),
            actionButton("collapse_all_cihi", "Collapse all")
          ),
          uiOutput("cihi_public_formulary_ui")
        ),
        
        br(),
        actionButton("back_btn3", "Back"),
        disabled(actionButton("continue_btn4", "Continue"))
      )
      
    } else if (current_page() == "5_filters") {
      q1_wording <- if (identical(search_all(), FALSE)) {
        "1. Restrict to drugs covered during a study period:"
      } else {
        "1. Restrict to drugs marketed during a study period:"
      }
      
      tagList(
        h3("Filters"),
        
        div(
          style = "margin-bottom:14px;",
          strong(q1_wording),
          radioButtons(
            "page5_q1_study_period",
            label = NULL,
            choices = c("Yes" = "yes", "No" = "no"),
            selected = character(0),
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.page5_q1_study_period == 'yes'",
            textInput("page5_start_date", "Start date (inclusive)", placeholder = "YYYY-MM-DD"),
            textInput("page5_end_date", "End date (inclusive)", placeholder = "YYYY-MM-DD"),
            uiOutput("page5_date_warning")
          )
        ),
        
        div(
          style = "margin-bottom:14px;",
          strong("2. Remove non-drugs and non-human drugs:"),
          actionButton(
            "page5_toggle_q2_info",
            label = tagList("Details ", icon("chevron-down")),
            class = "btn-clear",
            style = "padding:2px 8px;"
          ),
          hidden(
            div(
              id = "page5_q2_info",
              style = "margin-top:8px; background-color:#f7f7f7; padding:8px 10px; border-radius:4px;",
              "Warning: This is a custom set of filters set by the author on 2026-02-10. Use with discretion.",
              tags$div(style = "margin-top:8px;", "Non-drugs and non-human drugs were defined by:"),
              tags$ul(
                tags$li("DPD/CIHI ATC: e.g., allergens, radiopharmaceuticals (see ./data/dictionary_dpd_atc.csv)"),
                tags$li("DPD route: e.g., disinfectants (see ./data/dictionary_dpd_route.csv)"),
                tags$li("DPD schedule: e.g., homeopathic (see ./data/dictionary_dpd_schedule.csv)"),
                tags$li("DPD veterinary flag"),
                tags$li("CIHI drug type: 'Not applicable'")
              )
            )
          ),
          br(),
          radioButtons(
            "page5_q2_remove_nondrugs",
            label = NULL,
            choices = c("Yes" = "yes", "No" = "no"),
            selected = character(0),
            inline = TRUE
          )
        ),
        
        div(
          style = "margin-bottom:14px;",
          strong("3. Remove non-prescription drugs:"),
          actionButton(
            "page5_toggle_q3_info",
            label = tagList("Details ", icon("chevron-down")),
            class = "btn-clear",
            style = "padding:2px 8px;"
          ),
          hidden(
            div(
              id = "page5_q3_info",
              style = "margin-top:8px; background-color:#f7f7f7; padding:8px 10px; border-radius:4px;",
              "Warning: This is a custom set of filters set by the author on 2026-02-10. Use with discretion.",
              tags$div(style = "margin-top:8px;", "Placeholder bullet notes:"),
              tags$ul(
                tags$li("DPD schedule: 'NON-PRESCRIPTION DRUGS' (see ./data/dictionary_dpd_schedule.csv)"),
                tags$li("CIHI drug type: 'Over the counter'")
              )
            )
          ),
          br(),
          radioButtons(
            "page5_q3_remove_nonrx",
            label = NULL,
            choices = c("Yes" = "yes", "No" = "no"),
            selected = character(0),
            inline = TRUE
          )
        ),
        
        div(
          style = "margin-bottom:14px;",
          strong("4. Select Optional Drugs"),
          checkboxInput(
            "page5_opt_pseudodins",
            tagList(
              "PseudoDINs (PDINs)",
              tags$span(
                icon("circle-question"),
                title = "PDINs reflect drug benefits (e.g., to differentiate between package size/indication such as for methadone in pain vs. addiction management, to provide temporary coverage of a non-benefit drug to address shortages in a benefit drug) and non-drug benefits (e.g., diabetic supplies). PDINs are flagged in the CIHI public formulary coverage dataset.\n\nSelecting this will add PDIN rows and a 'flag_pdin' column {0: no, 1: yes}. However, the DPD fields will be missing as PDINs do not link to DPD.",
                style = "margin-left:5px; cursor:pointer;"
              )
            ),
            FALSE
          ),
          checkboxInput(
            "page5_opt_unmapped_dins",
            tagList(
              "Unmapped DINs",
              tags$span(
                icon("circle-question"),
                title = "Unmapped DINs are DINs that are listed in the CIHI public formulary coverage dataset but do not exist in DPD. E.g., 02060876 was a betamethasone product that used to be covered by the New Brunswick PDP until it was discontinued in 2000; it is not in the DPD.\n\nSelecting this will add unmapped DIN rows and a 'flag_unmapped' column {0: no, 1: yes}. However, the DPD fields will be missing as unmapped DINs do not link to DPD.",
                style = "margin-left:5px; cursor:pointer;"
              )
            ),
            FALSE
          )
        ),
        
        div(
          style = "margin-bottom:14px;",
          strong("5. Select Optional Fields"),
          checkboxInput(
            "page5_opt_route",
            tagList(
              "Route",
              tags$span(
                icon("circle-question"),
                title = "Selecting this will add a column for DPD routes (there may be multiple routes per DIN).",
                style = "margin-left:5px; cursor:pointer;"
              )
            ),
            FALSE
          ),
          checkboxInput(
            "page5_opt_schedule",
            tagList(
              "Schedule",
              tags$span(
                icon("circle-question"),
                title = "Selecting this will add a column for DPD schedules (there may be multiple schedules per DIN).",
                style = "margin-left:5px; cursor:pointer;"
              )
            ),
            FALSE
          ),
          checkboxInput(
            "page5_opt_biosimilar_flag",
            tagList(
              "Biosimilar Flag",
              tags$span(
                icon("circle-question"),
                title = "Selecting this will add a column for DPD biosimilar flags {0: no, 1: yes}.",
                style = "margin-left:5px; cursor:pointer;"
              )
            ),
            FALSE
          )
        ),
        
        br(),
        div(
          style = "display:flex; align-items:center; gap:10px;",
          actionButton("back_btn4", "Back"),
          disabled(actionButton(
            "continue_btn5",
            "Prepare DIN list",
            onclick = "$('#page5_loading_inline').show();"
          )),
          div(
            id = "page5_loading_inline",
            style = "display:none; color:#3CA0D6; font-weight:600;",
            icon("spinner", class = "fa-spin"),
            " Loading..."
          )
        )
      )
      
    } else if (current_page() == "6_missing_atc") {
      tagList(
        h3("Drugs in CIHI with Missing ATC"),
        uiOutput("page6_cihi_atc_y_candidates_ui"),
        br(),
        div(
          style = "display:flex; align-items:center; gap:10px;",
          actionButton("back_btn5", "Back"),
          actionButton("continue_btn6", "Continue to Results")
        )
      )
      
    } else if (current_page() == "7_end") {
      cached_count <- if (isTRUE(input_din_list())) 4 else 3
      tagList(
        h3("Results"),
        tableOutput("page6_summary_table"),
        actionButton("page6_toggle_atc", "Show/Hide selected ATC codes"),
        hidden(
          div(
            id = "page6_atc_wrap",
            uiOutput("page6_atc_list")
          )
        ),
        downloadButton("page6_download_summary", "Download Summary Table (CSV)"),
        br(), br(),
        strong(textOutput("page6_merged_rows", inline = TRUE)),
        uiOutput("page6_merged_stats"),
        strong(textOutput("page6_merged_cols_count", inline = TRUE)),
        uiOutput("page6_merged_columns"),
        downloadButton("page6_download_merged", "Download DIN List (CSV)"),
        br(),
        br(),
        actionButton("back_btn6", "Back"),
        br(),
        br(),
        div(
          style = "background-color:#f7f7f7; border:1px solid #ececec; border-radius:4px; padding:10px;",
          strong(paste0("Cached datasets in this session (", cached_count, "):")),
          tags$ul(
            tags$li("DPD dataset"),
            tags$li("CIHI public formulary coverage dataset"),
            tags$li("ATC dictionary"),
            if (isTRUE(input_din_list())) tags$li("DIN list")
          ),
          div(
            style = "display:flex; align-items:center; gap:10px;",
            actionButton("page6_reuse_datasets_btn", "Reuse datasets (resume at page 3)"),
            actionButton("page6_new_datasets_btn", "Use new datasets (return to page 1)")
          )
        )
      )
      
    }
  })
  
}



# Run Shiny app ----------------------------------------------------------------

options(shiny.launch.browser = TRUE)
shinyApp(ui, server)
