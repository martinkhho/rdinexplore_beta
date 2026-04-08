# ------------------------------------------------------------------------------
# Shiny app for exploring WHO ATC codes
# ------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(here)
library(readr)
library(tools)
library(dplyr)
library(tibble)
library(stringr)



# User Interface ---------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      .top-banner {
        background-color: #4fa86a;
        color: white;
        border-bottom: 1px solid #3d8d56;
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
        background-color: #58b676 !important;
        border-color: #58b676 !important;
        color: white !important;
      }

      .btn:hover {
        background-color: #439f62 !important;
        border-color: #439f62 !important;
        color: white !important;
      }

      .btn:disabled {
        background-color: #9fd5b0 !important;
        border-color: #9fd5b0 !important;
        color: white !important;
      }

      .btn-clear {
        background-color: #eeeeee !important;
        color: #555555 !important;
        border: none !important;
      }

      .btn-clear:hover {
        background-color: #f5f5f5 !important;
        color: #555555 !important;
      }

      .progress-bar {
        background-color: #4fa86a !important;
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
        margin: 12px 0px 14px 0px;
      }

      .dpd-choice-btn {
        width: 100%;
        padding: 14px 10px !important;
        font-weight: 600;
      }

      .btn-dpd-selected {
        background-color: #439f62 !important;
        border-color: #439f62 !important;
        opacity: 1 !important;
      }

      .btn-dpd-selected:hover {
        background-color: #439f62 !important;
        border-color: #439f62 !important;
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
        border-top: 1px solid #bedfc8;
        padding: 8px 16px 10px 16px;
        z-index: 900;
      }
    "))
  ),

  div(class = "top-banner", "ratcexplore"),

  div(
    class = "content-wrapper",
    uiOutput("page_content"),
    uiOutput("page_progress_ui")
  )
)


# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  app_data_dir <- here("./data/")
  atc_checklist_cache_rds <- file.path(app_data_dir, "atc_checklist_cached.rds")

  log_root_dir <- here("./log")
  run_id <- paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    substr(gsub("-", "", session$token), 1, 8)
  )
  output_root_dir <- here("./output/runs_ratcexplore")
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
      log_file <- file.path(day_dir, paste0("ratcexplore_", format(Sys.Date(), "%Y%m%d"), ".log"))

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

  page1_atc_status_message <- reactiveVal(NULL)
  page1_has_atc_choice <- reactiveVal(NULL)
  page1_viewer_choice <- reactiveVal(NULL)
  page3_viewer_mode <- reactiveVal(NULL)
  atc_dict_uploaded <- reactiveVal(FALSE)
  atc_saved_selected_ids <- reactiveVal(character(0))
  skip_cascade_deselect_ids <- reactiveVal(character(0))
  finder_explode_clicked <- reactiveVal(FALSE)
  page3_selected_codes <- reactiveVal(character(0))
  page3_atc_table <- reactiveVal(tibble(Level = character(0), Code = character(0), Description = character(0)))

  dict_who_atc <- tibble(atc_code = character(0), atc_name = character(0))
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
  atc_data_revision <- reactiveVal(0L)

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
                     background-color:#a9d9b7;
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
                             background-color:#bfe4ca;
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
                                     background-color:#d8efe0;
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
      mutate(
        atc_code = str_to_upper(trimws(atc_code)),
        atc_name = trimws(atc_name)
      ) %>%
      filter(!is.na(atc_code), nzchar(atc_code)) %>%
      group_by(atc_code) %>%
      summarise(
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

    atc_observers_bound(FALSE)
    nested_atc_ui_cache(build_nested_atc_ui())
    atc_data_revision(isolate(atc_data_revision()) + 1L)
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
        toggle(id = paste0("child_", code))
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

  get_selected_ids <- function(ids) {
    ids[vapply(ids, function(id) isTRUE(input[[id]]), logical(1))]
  }

  code_to_level <- function(code) {
    code_len <- nchar(code)
    if (code_len == 1) {
      return("ATC1")
    }
    if (code_len == 3) {
      return("ATC2")
    }
    if (code_len == 4) {
      return("ATC3")
    }
    if (code_len == 5) {
      return("ATC4")
    }
    paste0("ATC", code_len)
  }

  build_atc_table_from_codes <- function(codes) {
    code_vals <- unique(trimws(as.character(codes)))
    code_vals <- code_vals[nzchar(code_vals)]
    if (length(code_vals) == 0) {
      return(tibble(Level = character(0), Code = character(0), Description = character(0)))
    }

    tibble(
      Level = vapply(code_vals, code_to_level, character(1)),
      Code = code_vals,
      Description = vapply(code_vals, resolve_atc_name, character(1))
    ) %>%
      arrange(Code)
  }

  atc_with_desc_from_codes <- function(codes) {
    code_vals <- unique(trimws(as.character(codes)))
    code_vals <- code_vals[nzchar(code_vals)]
    if (length(code_vals) == 0) {
      return(character(0))
    }
    vapply(code_vals, function(code) {
      desc <- resolve_atc_name(code)
      if (nzchar(desc)) {
        paste0(code, " - ", desc)
      } else {
        code
      }
    }, character(1))
  }

  write_csv_with_notice <- function(df, file, label) {
    tryCatch({
      write_csv(df, file)
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

  build_page3_summary_df <- function(expand_atc = FALSE) {
    add_row <- function(items, values, item, value) {
      list(items = c(items, item), values = c(values, value))
    }

    selected_codes <- page3_selected_codes()
    atc_with_desc <- atc_with_desc_from_codes(selected_codes)

    atc_file_name <- if (!is.null(input$atc_dict_upload)) {
      input$atc_dict_upload$name
    } else if (identical(page1_has_atc_choice(), "yes")) {
      "Cached ATC dictionary/checklist"
    } else {
      "Unknown"
    }

    viewer_mode <- page3_viewer_mode()
    viewer_label <- if (identical(viewer_mode, "searchbar")) {
      "Searchbar"
    } else if (identical(viewer_mode, "checklist")) {
      "Checklist"
    } else {
      "Unknown"
    }

    items <- character(0)
    values <- character(0)

    out <- add_row(items, values, "Viewer mode", viewer_label)
    items <- out$items; values <- out$values

    out <- add_row(items, values, "ATC dictionary source", if (identical(page1_has_atc_choice(), "yes")) "Use existing cached ATC dictionary/checklist" else "Upload ATC dictionary")
    items <- out$items; values <- out$values

    out <- add_row(items, values, "ATC dictionary file", atc_file_name)
    items <- out$items; values <- out$values

    if (identical(viewer_mode, "searchbar")) {
      out <- add_row(items, values, "Explode ATCs clicked", if (isTRUE(finder_explode_clicked())) "Yes" else "No")
      items <- out$items; values <- out$values
    }

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
      out <- add_row(items, values, "ATC selections", paste0(length(selected_codes), " selected. Click the button below to show."))
      items <- out$items; values <- out$values
    }

    data.frame(Item = items, Selection = values, stringsAsFactors = FALSE)
  }

  write_run_outputs <- function() {
    tryCatch({
      dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)

      summary_df <- build_page3_summary_df(expand_atc = TRUE)
      atc_df <- page3_atc_table()

      write_csv(summary_df, file.path(run_output_dir, "summary.csv"))
      write_csv(atc_df, file.path(run_output_dir, "atcs.csv"))

      params <- list(
        run_id = run_id,
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        viewer_mode = page3_viewer_mode(),
        atc_source = page1_has_atc_choice(),
        explode_clicked = isTRUE(finder_explode_clicked()),
        selected_atc = as.character(atc_with_desc_from_codes(page3_selected_codes()))
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
        write_lines(
          capture.output(str(params)),
          file.path(run_output_dir, "params.txt")
        )
      }

      log_event(
        "INFO",
        "run_outputs_written",
        list(path = run_output_dir, summary_rows = nrow(summary_df), atc_rows = nrow(atc_df))
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

  current_page <- reactiveVal("1_upload")
  page_order <- c("1_upload", "2_viewer", "3_end")

  observeEvent(current_page(), {
    log_event("INFO", "page_transition", list(page = current_page()))
  }, ignoreInit = FALSE)

  page1_can_continue <- reactive({
    viewer_choice <- page1_viewer_choice()

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

    has_viewer_choice <- !is.null(viewer_choice)

    isTRUE(has_viewer_choice && has_atc_choice && atc_ready)
  })

  output$page1_continue_btn <- renderUI({
    if (isTRUE(page1_can_continue())) {
      actionButton("continue_btn1", "Continue")
    } else {
      disabled(actionButton("continue_btn1", "Continue"))
    }
  })

  observeEvent(input$continue_btn1, {
    disable("continue_btn1")
    can_go_page2 <- TRUE
    abort_page1_continue <- function() {
      can_go_page2 <<- FALSE
      enable("continue_btn1")
    }

    viewer_choice <- page1_viewer_choice()
    if (is.null(viewer_choice) || !nzchar(viewer_choice)) {
      showModal(modalDialog(
        title = "Missing Viewer Choice",
        "Please choose whether to view ATCs with Checklist or Searchbar mode.",
        easyClose = TRUE
      ))
      abort_page1_continue()
      return(NULL)
    }

    atc_choice <- input$page1_use_existing_atc
    if (is.null(atc_choice) || !nzchar(atc_choice)) {
      showModal(modalDialog(
        title = "Missing ATC Dictionary Choice",
        "Please answer whether you want to use an existing cached ATC dictionary.",
        easyClose = TRUE
      ))
      abort_page1_continue()
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
        abort_page1_continue()
        return(NULL)
      }
      atc_dict_uploaded(TRUE)
      page1_atc_status_message("Loaded cached ATC dictionary/checklist from ./data/atc_checklist_cached.rds.")
    } else {
      if (!isTRUE(save_atc_dictionary_upload(require_upload = TRUE))) {
        abort_page1_continue()
        return(NULL)
      }
    }

    log_event(
      "INFO",
      "page1_to_page2_start",
      list(atc_choice = atc_choice, viewer_choice = viewer_choice)
    )

    tryCatch({
      withProgress(message = "Preparing ATC viewer...", value = 0, {
        incProgress(0.5, detail = "Loading ATC dictionary")
        incProgress(1, detail = "Done")
      })
      if (!isTRUE(can_go_page2)) {
        return(NULL)
      }
      log_event("INFO", "page1_to_page2_success")
      if (identical(viewer_choice, "searchbar")) {
        finder_explode_clicked(FALSE)
        current_page("2_atc_searchbar")
      } else {
        current_page("2_atc_checklist")
      }
    }, error = function(e) {
      enable("continue_btn1")
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
          style = "margin:2px 0 8px 0; color:#496233;",
          strong("Current file selection: "),
          selected_name
        )
      }
    )
  })

  observeEvent(input$page1_toggle_atc_info, {
    toggle("page1_atc_info")
  })

  observeEvent(input$viewer_checklist_btn, {
    page1_viewer_choice("checklist")
  })

  observeEvent(input$viewer_searchbar_btn, {
    page1_viewer_choice("searchbar")
  })

  output$nested_atc_ui <- renderUI({
    ui_obj <- nested_atc_ui_cache()
    if (is.null(ui_obj)) {
      ui_obj <- build_nested_atc_ui()
      nested_atc_ui_cache(ui_obj)
    }
    ui_obj
  })

  atc_selected_flags <- reactive({
    vapply(all_atc_ids, function(id) isTRUE(input[[id]]), logical(1))
  })

  selected_count <- reactive({
    sum(atc_selected_flags(), na.rm = TRUE)
  })

  output$selected_counter <- renderText({
    paste("Number of codes selected:", selected_count())
  })

  build_atc_choices <- function(df) {
    if (nrow(df) == 0) {
      return(setNames(character(0), character(0)))
    }

    labels <- ifelse(
      is.na(df$atc_name) | !nzchar(df$atc_name),
      df$atc_code,
      paste(df$atc_code, "-", df$atc_name)
    )
    setNames(as.character(df$atc_code), labels)
  }

  normalize_selection <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }
    x_chr <- as.character(x)
    x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
    unique(x_chr)
  }

  keep_by_prefix <- function(codes, prefixes) {
    prefixes <- normalize_selection(prefixes)
    if (length(prefixes) == 0) {
      return(rep(TRUE, length(codes)))
    }
    Reduce(`|`, lapply(prefixes, function(prefix) startsWith(codes, prefix)))
  }

  resolve_atc_name <- function(code) {
    if (is.null(code) || !nzchar(code)) {
      return("")
    }
    idx <- which(dict_who_atc$atc_code == code)
    if (length(idx) == 0) {
      return("")
    }
    as.character(dict_who_atc$atc_name[idx[[1]]])
  }

  valid_selected <- function(current, valid_values) {
    intersect(normalize_selection(current), normalize_selection(valid_values))
  }

  filtered_atc2_df <- reactive({
    atc_data_revision()
    df <- dict_who_atc2
    atc1_vals <- normalize_selection(input$finder_atc1)
    if (length(atc1_vals) > 0) {
      df <- df[keep_by_prefix(df$atc_code, atc1_vals), , drop = FALSE]
    }
    df
  })

  filtered_atc3_df <- reactive({
    atc_data_revision()
    df <- dict_who_atc3
    atc1_vals <- normalize_selection(input$finder_atc1)
    atc2_vals <- normalize_selection(input$finder_atc2)
    if (length(atc2_vals) > 0) {
      df <- df[keep_by_prefix(df$atc_code, atc2_vals), , drop = FALSE]
    } else if (length(atc1_vals) > 0) {
      df <- df[keep_by_prefix(df$atc_code, atc1_vals), , drop = FALSE]
    }
    df
  })

  filtered_atc4_df <- reactive({
    atc_data_revision()
    df <- dict_who_atc4
    atc1_vals <- normalize_selection(input$finder_atc1)
    atc2_vals <- normalize_selection(input$finder_atc2)
    atc3_vals <- normalize_selection(input$finder_atc3)
    if (length(atc3_vals) > 0) {
      df <- df[keep_by_prefix(df$atc_code, atc3_vals), , drop = FALSE]
    } else if (length(atc2_vals) > 0) {
      df <- df[keep_by_prefix(df$atc_code, atc2_vals), , drop = FALSE]
    } else if (length(atc1_vals) > 0) {
      df <- df[keep_by_prefix(df$atc_code, atc1_vals), , drop = FALSE]
    }
    df
  })

  observeEvent(list(current_page(), atc_data_revision()), {
    req(identical(current_page(), "2_atc_searchbar"))
    choices <- build_atc_choices(dict_who_atc1)
    vals <- unname(choices)
    selected_current <- isolate(input$finder_atc1)
    updateSelectizeInput(
      session,
      "finder_atc1",
      choices = choices,
      selected = valid_selected(selected_current, vals),
      options = list(placeholder = "Nothing selected")
    )
  }, ignoreInit = FALSE)

  observeEvent(list(current_page(), atc_data_revision(), input$finder_atc1), {
    req(identical(current_page(), "2_atc_searchbar"))
    df <- filtered_atc2_df()
    choices <- build_atc_choices(df)
    vals <- unname(choices)
    selected_current <- isolate(input$finder_atc2)
    updateSelectizeInput(
      session,
      "finder_atc2",
      choices = choices,
      selected = valid_selected(selected_current, vals),
      options = list(placeholder = "Nothing selected")
    )
  }, ignoreInit = FALSE)

  observeEvent(
    list(current_page(), atc_data_revision(), input$finder_atc1, input$finder_atc2),
    {
      req(identical(current_page(), "2_atc_searchbar"))
    df <- filtered_atc3_df()
    choices <- build_atc_choices(df)
    vals <- unname(choices)
    selected_current <- isolate(input$finder_atc3)
    updateSelectizeInput(
      session,
      "finder_atc3",
      choices = choices,
      selected = valid_selected(selected_current, vals),
      options = list(placeholder = "Nothing selected")
    )
    },
    ignoreInit = FALSE
  )

  observeEvent(
    list(current_page(), atc_data_revision(), input$finder_atc1, input$finder_atc2, input$finder_atc3),
    {
      req(identical(current_page(), "2_atc_searchbar"))
    df <- filtered_atc4_df()
    choices <- build_atc_choices(df)
    vals <- unname(choices)
    selected_current <- isolate(input$finder_atc4)
    updateSelectizeInput(
      session,
      "finder_atc4",
      choices = choices,
      selected = valid_selected(selected_current, vals),
      options = list(placeholder = "Nothing selected")
    )
    },
    ignoreInit = FALSE
  )

  selection_rows <- function(level_name, codes) {
    code_vals <- normalize_selection(codes)
    if (length(code_vals) == 0) {
      return(tibble(Level = character(0), Code = character(0), Description = character(0)))
    }
    tibble(
      Level = level_name,
      Code = code_vals,
      Description = vapply(code_vals, resolve_atc_name, character(1))
    )
  }

  finder_selection_df <- reactive({
    atc_data_revision()
    bind_rows(
      selection_rows("ATC1", input$finder_atc1),
      selection_rows("ATC2", input$finder_atc2),
      selection_rows("ATC3", input$finder_atc3),
      selection_rows("ATC4", input$finder_atc4)
    ) %>%
      arrange(Code)
  })

  output$finder_selected_table <- renderTable({
    df <- finder_selection_df()
    if (nrow(df) == 0) {
      return(data.frame(Level = "None", Code = "", Description = "", check.names = FALSE))
    }
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "")

  output$finder_selected_counter <- renderText({
    paste("Number of codes selected:", nrow(finder_selection_df()))
  })

  observeEvent(input$finder_explode_atcs, {
    req(identical(current_page(), "2_atc_searchbar"))

    selected_codes <- unique(c(
      normalize_selection(input$finder_atc1),
      normalize_selection(input$finder_atc2),
      normalize_selection(input$finder_atc3),
      normalize_selection(input$finder_atc4)
    ))

    if (length(selected_codes) == 0) {
      return(NULL)
    }

    expanded_codes <- unique(c(
      selected_codes,
      unlist(lapply(selected_codes, get_all_descendants), use.names = FALSE)
    ))

    updateSelectizeInput(
      session,
      "finder_atc1",
      selected = intersect(dict_who_atc1$atc_code, expanded_codes)
    )
    updateSelectizeInput(
      session,
      "finder_atc2",
      selected = intersect(dict_who_atc2$atc_code, expanded_codes)
    )
    updateSelectizeInput(
      session,
      "finder_atc3",
      selected = intersect(dict_who_atc3$atc_code, expanded_codes)
    )
    updateSelectizeInput(
      session,
      "finder_atc4",
      selected = intersect(dict_who_atc4$atc_code, expanded_codes)
    )
    finder_explode_clicked(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$finder_atc1_select_all, {
    req(identical(current_page(), "2_atc_searchbar"))
    updateSelectizeInput(session, "finder_atc1", selected = dict_who_atc1$atc_code)
  }, ignoreInit = TRUE)

  observeEvent(input$finder_atc1_deselect_all, {
    req(identical(current_page(), "2_atc_searchbar"))
    updateSelectizeInput(session, "finder_atc1", selected = character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$finder_atc2_select_all, {
    req(identical(current_page(), "2_atc_searchbar"))
    updateSelectizeInput(session, "finder_atc2", selected = filtered_atc2_df()$atc_code)
  }, ignoreInit = TRUE)

  observeEvent(input$finder_atc2_deselect_all, {
    req(identical(current_page(), "2_atc_searchbar"))
    updateSelectizeInput(session, "finder_atc2", selected = character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$finder_atc3_select_all, {
    req(identical(current_page(), "2_atc_searchbar"))
    updateSelectizeInput(session, "finder_atc3", selected = filtered_atc3_df()$atc_code)
  }, ignoreInit = TRUE)

  observeEvent(input$finder_atc3_deselect_all, {
    req(identical(current_page(), "2_atc_searchbar"))
    updateSelectizeInput(session, "finder_atc3", selected = character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$finder_atc4_select_all, {
    req(identical(current_page(), "2_atc_searchbar"))
    updateSelectizeInput(session, "finder_atc4", selected = filtered_atc4_df()$atc_code)
  }, ignoreInit = TRUE)

  observeEvent(input$finder_atc4_deselect_all, {
    req(identical(current_page(), "2_atc_searchbar"))
    updateSelectizeInput(session, "finder_atc4", selected = character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$clear_all_btn, {
    clear_all_atc()
  })

  observeEvent(input$expand_all, {
    session$onFlushed(function() {
      lapply(non_leaf_codes, function(code) {
        show(paste0("child_", code))
      })
    }, once = TRUE)
  })

  observeEvent(input$collapse_all, {
    session$onFlushed(function() {
      lapply(non_leaf_codes, function(code) {
        hide(paste0("child_", code))
      })
    }, once = TRUE)
  })

  observeEvent(input$back_btn1, {
    atc_saved_selected_ids(get_selected_ids(all_atc_ids))
    current_page("1_upload")
  })

  get_checklist_selected_codes <- function() {
    ids <- get_selected_ids(all_atc_ids)
    sort(unique(sub("^atc_", "", ids)))
  }

  get_searchbar_selected_codes <- function() {
    sort(unique(c(
      normalize_selection(input$finder_atc1),
      normalize_selection(input$finder_atc2),
      normalize_selection(input$finder_atc3),
      normalize_selection(input$finder_atc4)
    )))
  }

  searchbar_has_unexploded_descendants <- function(selected_codes = get_searchbar_selected_codes()) {
    selected_codes <- unique(as.character(selected_codes))
    selected_codes <- selected_codes[nzchar(selected_codes)]
    if (length(selected_codes) == 0) {
      return(FALSE)
    }

    descendant_codes <- unique(unlist(
      lapply(selected_codes, get_all_descendants),
      use.names = FALSE
    ))
    descendant_codes <- descendant_codes[nzchar(descendant_codes)]

    length(setdiff(descendant_codes, selected_codes)) > 0
  }

  go_to_page3 <- function(viewer_mode, selected_codes, atc_table) {
    page3_viewer_mode(viewer_mode)
    page3_selected_codes(selected_codes)
    page3_atc_table(atc_table)
    write_run_outputs()
    current_page("3_end")
  }

  observeEvent(input$continue_btn2_checklist, {
    selected_codes <- get_checklist_selected_codes()
    atc_table <- build_atc_table_from_codes(selected_codes)
    go_to_page3("checklist", selected_codes, atc_table)
  }, ignoreInit = TRUE)

  observeEvent(input$continue_btn2_searchbar, {
    selected_codes <- get_searchbar_selected_codes()
    atc_table <- finder_selection_df()

    if (isTRUE(searchbar_has_unexploded_descendants(selected_codes))) {
      showModal(modalDialog(
        title = "Continue without exploding?",
        "Some selected ATC codes still have downstream levels not selected. The code will not auto-explode your ATC selections.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("continue_btn2_searchbar_noexplode", "Continue without exploding")
        ),
        easyClose = TRUE
      ))
      return(NULL)
    }

    go_to_page3("searchbar", selected_codes, atc_table)
  }, ignoreInit = TRUE)

  observeEvent(input$continue_btn2_searchbar_noexplode, {
    removeModal()
    selected_codes <- get_searchbar_selected_codes()
    atc_table <- finder_selection_df()
    go_to_page3("searchbar", selected_codes, atc_table)
  }, ignoreInit = TRUE)

  observeEvent(input$back_btn1_search, {
    current_page("1_upload")
  })

  observeEvent(input$back_btn3, {
    viewer_mode <- page3_viewer_mode()
    if (identical(viewer_mode, "searchbar")) {
      current_page("2_atc_searchbar")
    } else {
      current_page("2_atc_checklist")
    }
  }, ignoreInit = TRUE)

  observeEvent(current_page(), {
    req(current_page() == "2_atc_checklist")
    bind_atc_observers()
    selected_ids <- isolate(atc_saved_selected_ids())

    session$onFlushed(function() {
      lapply(all_atc_ids, function(id) {
        updateCheckboxInput(session, id, value = id %in% selected_ids)
      })
    }, once = TRUE)
  }, ignoreInit = TRUE)

  page3_summary_table_df <- reactive({
    build_page3_summary_df(expand_atc = FALSE)
  })

  page3_summary_download_df <- reactive({
    build_page3_summary_df(expand_atc = TRUE)
  })

  output$page3_summary_table <- renderTable({
    page3_summary_table_df()
  }, bordered = TRUE, striped = TRUE, spacing = "s")

  output$page3_download_summary <- downloadHandler(
    filename = function() {
      paste0("ratcexplore_summary_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv_with_notice(page3_summary_download_df(), file, "Summary CSV download")
    }
  )

  output$page3_download_atcs <- downloadHandler(
    filename = function() {
      paste0("ratcexplore_atcs_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv_with_notice(page3_atc_table(), file, "ATC CSV download")
    }
  )

  output$page3_selected_counter <- renderText({
    paste("Number of codes selected:", length(page3_selected_codes()))
  })

  output$page3_atc_list <- renderUI({
    req(current_page() == "3_end")
    atc_with_desc <- atc_with_desc_from_codes(page3_selected_codes())
    if (length(atc_with_desc) == 0) {
      return(div("No ATC codes selected."))
    }
    tags$ul(
      lapply(atc_with_desc, tags$li)
    )
  })

  observeEvent(input$page3_toggle_atc, {
    toggle("page3_atc_wrap")
  }, ignoreInit = TRUE)

  output$page_progress_ui <- renderUI({
    progress_page <- if (identical(current_page(), "1_upload")) {
      "1_upload"
    } else if (identical(current_page(), "3_end")) {
      "3_end"
    } else {
      "2_viewer"
    }
    idx <- match(progress_page, page_order)
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
          style = paste0("width:", pct, "%; background-color:#4fa86a;")
        )
      )
    )
  })

  output$page_content <- renderUI({
    if (current_page() == "1_upload") {
      selected_viewer <- page1_viewer_choice()
      viewer_checklist_class <- if (identical(selected_viewer, "checklist")) {
        "btn-dpd-selected"
      } else if (identical(selected_viewer, "searchbar")) {
        "btn-dpd-faded"
      } else {
        ""
      }
      viewer_searchbar_class <- if (identical(selected_viewer, "searchbar")) {
        "btn-dpd-selected"
      } else if (identical(selected_viewer, "checklist")) {
        "btn-dpd-faded"
      } else {
        ""
      }

      tagList(
        div(
          style = "display:flex; align-items:center; gap:6px;",
          strong("Choose viewer mode:"),
          tags$span(
            icon("circle-question"),
            title = "Checklist: displays all ATCs at once in a hierarchy. Searchbar: allows searching through each ATC level.",
            style = "cursor:pointer;"
          )
        ),
        div(
          class = "dpd-choice-wrap",
          div(
            style = "flex:1;",
            actionButton(
              "viewer_checklist_btn",
              "Checklist",
              class = paste("dpd-choice-btn", viewer_checklist_class)
            )
          ),
          div(
            style = "flex:1;",
            actionButton(
              "viewer_searchbar_btn",
              "Searchbar",
              class = paste("dpd-choice-btn", viewer_searchbar_class)
            )
          )
        ),
        br(),
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
          )
        ),
        uiOutput("page1_atc_upload_status"),
        br(),
        div(
          style = "background-color:#f7f7f7; border:1px solid #ececec; border-radius:4px; padding:10px; margin-bottom:12px;",
          strong("Warning:"),
          " You are about to see the full WHO ATC checklist. Not all ATCs may be available in your country."
        ),
        uiOutput("page1_continue_btn")
      )
    } else if (current_page() == "2_atc_checklist") {
      tagList(
        h3("Select ATC Codes"),
        br(),
        strong(textOutput("selected_counter", inline = TRUE)),
        br(),
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
        div(
          style = "display:flex; align-items:center; gap:10px;",
          actionButton("back_btn1", "Back"),
          actionButton("continue_btn2_checklist", "Continue")
        )
      )
    } else if (current_page() == "2_atc_searchbar") {
      tagList(
        h3("Search ATC Codes"),
        p("To remove a selection, click on it (it will highlight in blue) and hit Delete."),
        div(
          style = "max-width:700px;",
          div(strong("Filter on 1st level:"), style = "margin-bottom:6px;"),
          selectizeInput(
            "finder_atc1",
            label = NULL,
            choices = setNames(character(0), character(0)),
            selected = character(0),
            multiple = TRUE,
            options = list(placeholder = "Nothing selected"),
            width = "100%"
          ),
          div(
            style = "display:flex; margin-bottom:12px;",
            actionButton(
              "finder_atc1_select_all",
              "Select All",
              class = "btn-clear",
              style = "flex:1; border:1px solid #d9d9d9; border-right:none; border-radius:4px 0px 0px 4px;"
            ),
            actionButton(
              "finder_atc1_deselect_all",
              "Deselect All",
              class = "btn-clear",
              style = "flex:1; border:1px solid #d9d9d9; border-radius:0px 4px 4px 0px;"
            )
          ),
          div(strong("Filter on 2nd level:"), style = "margin-bottom:6px;"),
          selectizeInput(
            "finder_atc2",
            label = NULL,
            choices = setNames(character(0), character(0)),
            selected = character(0),
            multiple = TRUE,
            options = list(placeholder = "Nothing selected"),
            width = "100%"
          ),
          div(
            style = "display:flex; margin-bottom:12px;",
            actionButton(
              "finder_atc2_select_all",
              "Select All",
              class = "btn-clear",
              style = "flex:1; border:1px solid #d9d9d9; border-right:none; border-radius:4px 0px 0px 4px;"
            ),
            actionButton(
              "finder_atc2_deselect_all",
              "Deselect All",
              class = "btn-clear",
              style = "flex:1; border:1px solid #d9d9d9; border-radius:0px 4px 4px 0px;"
            )
          ),
          div(strong("Filter on 3rd level:"), style = "margin-bottom:6px;"),
          selectizeInput(
            "finder_atc3",
            label = NULL,
            choices = setNames(character(0), character(0)),
            selected = character(0),
            multiple = TRUE,
            options = list(placeholder = "Nothing selected"),
            width = "100%"
          ),
          div(
            style = "display:flex; margin-bottom:12px;",
            actionButton(
              "finder_atc3_select_all",
              "Select All",
              class = "btn-clear",
              style = "flex:1; border:1px solid #d9d9d9; border-right:none; border-radius:4px 0px 0px 4px;"
            ),
            actionButton(
              "finder_atc3_deselect_all",
              "Deselect All",
              class = "btn-clear",
              style = "flex:1; border:1px solid #d9d9d9; border-radius:0px 4px 4px 0px;"
            )
          ),
          div(strong("Filter on 4th level:"), style = "margin-bottom:6px;"),
          selectizeInput(
            "finder_atc4",
            label = NULL,
            choices = setNames(character(0), character(0)),
            selected = character(0),
            multiple = TRUE,
            options = list(placeholder = "Nothing selected"),
            width = "100%"
          ),
          div(
            style = "display:flex; margin-bottom:12px;",
            actionButton(
              "finder_atc4_select_all",
              "Select All",
              class = "btn-clear",
              style = "flex:1; border:1px solid #d9d9d9; border-right:none; border-radius:4px 0px 0px 4px;"
            ),
            actionButton(
              "finder_atc4_deselect_all",
              "Deselect All",
              class = "btn-clear",
              style = "flex:1; border:1px solid #d9d9d9; border-radius:0px 4px 4px 0px;"
            )
          )
        ),
        br(),
        h4("Selected ATCs"),
        actionButton("finder_explode_atcs", "Explode ATCs"),
        br(),
        tableOutput("finder_selected_table"),
        br(),
        strong(textOutput("finder_selected_counter", inline = TRUE)),
        br(),
        br(),
        div(
          style = "display:flex; align-items:center; gap:10px;",
          actionButton("back_btn1_search", "Back"),
          actionButton("continue_btn2_searchbar", "Continue")
        )
      )
    } else if (current_page() == "3_end") {
      tagList(
        h3("Results"),
        tableOutput("page3_summary_table"),
        actionButton("page3_toggle_atc", "Show/Hide selected ATC codes"),
        hidden(
          div(
            id = "page3_atc_wrap",
            uiOutput("page3_atc_list")
          )
        ),
        downloadButton("page3_download_summary", "Download Summary Table (CSV)"),
        br(),
        br(),
        strong(textOutput("page3_selected_counter", inline = TRUE)),
        br(),
        downloadButton("page3_download_atcs", "Download ATCs (CSV)"),
        br(),
        br(),
        actionButton("back_btn3", "Back")
      )
    }
  })
}



# Run Shiny app ----------------------------------------------------------------

options(shiny.launch.browser = TRUE)
shinyApp(ui, server)
