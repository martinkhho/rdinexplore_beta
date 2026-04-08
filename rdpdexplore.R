# ------------------------------------------------------------------------------
# Shiny app for exploring Health Canada DPD
# ------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(here)
library(readr)
library(dplyr)
library(purrr)

source(here("./src/read_files.R"))
source(here("./src/dpd_load.R"))
source(here("./src/dpd_explore.R"))

DATA_DIR <- here("./data/")



# User Interface ---------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      .top-banner {
        background-color: #E26767;
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
        background-color: #F06D6D !important;
        border-color: #F06D6D !important;
        color: white !important;
      }

      .btn:hover {
        background-color: #E05F5F !important;
        border-color: #E05F5F !important;
        color: white !important;
      }

      .btn:disabled {
        background-color: #F5B8B8 !important;
        border-color: #F5B8B8 !important;
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
        background-color: #E05F5F !important;
        border-color: #E05F5F !important;
        opacity: 1 !important;
      }

      .btn-dpd-selected:hover {
        background-color: #E05F5F !important;
        border-color: #E05F5F !important;
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

      .shiny-notification .progress-bar {
        background-color: #E05F5F !important;
      }

    "))
  ),

  div(class = "top-banner", "rdpdexplore"),

  div(class = "content-wrapper",
      uiOutput("page_content"),
      uiOutput("page_progress_ui")
  )
)



# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  app_data_dir <- here("./data/")
  log_root_dir <- here("./log")
  run_id <- paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    substr(gsub("-", "", session$token), 1, 8)
  )
  output_root_dir <- here("./output/runs_rdpdexplore")
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
      log_file <- file.path(day_dir, paste0("rdpdexplore_", format(Sys.Date(), "%Y%m%d"), ".log"))

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

  current_page <- reactiveVal("1_dpd")
  page_order <- c("1_dpd", "2_end")

  dpd_choice <- reactiveVal(NULL)
  keep_f_choice <- reactiveVal(NULL)

  dpd_last_downloaded <- reactiveVal(NULL)
  dpd_last_updated <- reactiveVal(NULL)

  run_merged <- reactiveVal(NULL)
  run_problem_cols <- reactiveVal(list(all_na_cols = character(0), no_variability_cols = character(0)))

  log_event("INFO", "session_start")
  session$onSessionEnded(function() {
    log_event("INFO", "session_end", list(page = tryCatch(current_page(), error = function(e) "unknown")))
  })

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
  }

  collapse_or_none <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (length(x) == 0) {
      return("None")
    }
    paste(x, collapse = ", ")
  }

  summary_choice_label <- function() {
    if (identical(dpd_choice(), "download")) {
      "Download latest DPD"
    } else if (identical(dpd_choice(), "no_download")) {
      "Use existing DPD"
    } else {
      "Not selected"
    }
  }

  keep_f_label <- function() {
    if (identical(keep_f_choice(), "yes")) "Yes" else if (identical(keep_f_choice(), "no")) "No" else "Not selected"
  }

  build_page2_summary_rows <- function(problem_cols) {
    list(
      list(item = "DPD data", value = summary_choice_label(), tooltip = NULL),
      list(item = "DPD last downloaded", value = format_source_date(dpd_last_downloaded()), tooltip = NULL),
      list(item = "DPD last updated", value = format_source_date(dpd_last_updated()), tooltip = NULL),
      list(item = "Keep French columns", value = keep_f_label(), tooltip = NULL),
      list(
        item = "All-NA columns",
        value = collapse_or_none(problem_cols$all_na_cols),
        tooltip = "Columns where all values are NA after applying the French-column option."
      ),
      list(
        item = "No-variability columns",
        value = collapse_or_none(problem_cols$no_variability_cols),
        tooltip = "Columns where all non-NA values are identical after applying the French-column option."
      )
    )
  }

  build_page2_summary_df <- function(problem_cols) {
    rows <- build_page2_summary_rows(problem_cols)
    data.frame(
      Item = vapply(rows, function(x) x$item, character(1)),
      Selection = vapply(rows, function(x) x$value, character(1)),
      stringsAsFactors = FALSE
    )
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

  write_run_outputs <- function(merged_df, problem_cols) {
    tryCatch({
      dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)

      summary_df <- build_page2_summary_df(problem_cols)
      write_csv(summary_df, file.path(run_output_dir, "summary.csv"))
      write_csv(merged_df, file.path(run_output_dir, "combined.csv"))

      params <- list(
        run_id = run_id,
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        dpd_choice = dpd_choice(),
        keep_f_cols_choice = keep_f_choice(),
        dpd_last_downloaded = format_source_date(dpd_last_downloaded()),
        dpd_last_updated = format_source_date(dpd_last_updated()),
        all_na_cols = as.character(problem_cols$all_na_cols),
        no_variability_cols = as.character(problem_cols$no_variability_cols),
        merged_rows = nrow(merged_df),
        merged_cols = ncol(merged_df)
      )

      if (requireNamespace("jsonlite", quietly = TRUE)) {
        write_json <- getFromNamespace("write_json", "jsonlite")
        write_json(
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

  refresh_provenance_dates()

  observeEvent(current_page(), {
    if (identical(current_page(), "1_dpd")) {
      runjs("$('#page1_loading_inline').hide();")
    }
  })

  observeEvent(input$dpd_download_btn, {
    dpd_choice("download")
  })

  observeEvent(input$dpd_no_download_btn, {
    dpd_choice("no_download")
  })

  observeEvent(input$keep_f_cols_choice, {
    keep_f_choice(input$keep_f_cols_choice)
  }, ignoreInit = TRUE)

  observeEvent(input$continue_btn_dpd, {
    req(!is.null(dpd_choice()), !is.null(input$keep_f_cols_choice), nzchar(input$keep_f_cols_choice))

    keep_f_choice(input$keep_f_cols_choice)
    log_event("INFO", "run_start", list(dpd_choice = dpd_choice(), keep_f_cols_choice = keep_f_choice()))

    disable("continue_btn_dpd")
    on.exit({
      runjs("$('#page1_loading_inline').hide();")
      enable("continue_btn_dpd")
    }, add = TRUE)

    tryCatch({
      withProgress(message = "Loading DPD...", value = 0, {
        incProgress(0.1, detail = "Preparing source")

        download_dpd <- identical(dpd_choice(), "download")
        keep_f_cols <- identical(keep_f_choice(), "yes")

        dpd <- dpd_load_cache(download_dpd = download_dpd)

        incProgress(0.2, detail = "Applying options")

        dpd <- dpd_keep_f_cols(dpd, keep_f_cols = keep_f_cols)
        dpd$status <- dpd_filter_status_current(dpd$status, keep_f_cols = keep_f_cols)

        incProgress(0.3, detail = "Building table")

        merged <- dpd %>%
          dpd_combine(keep_f_cols = keep_f_cols) %>%
          dpd_rollup_code(keep_f_cols = keep_f_cols)

        problem_cols <- dpd_problem_cols(merged)
        run_problem_cols(problem_cols)
        run_merged(merged)
        write_run_outputs(merged, problem_cols)
        log_event("INFO", "run_complete", list(merged_rows = nrow(merged), merged_cols = ncol(merged)))

        incProgress(1, detail = "Done")
      })

      refresh_provenance_dates()
      current_page("2_end")
    }, error = function(e) {
      msg <- conditionMessage(e)
      msg <- gsub("\\x1b\\[[0-9;]*m", "", msg, perl = TRUE)
      log_event("ERROR", "run_error", list(message = msg))
      showModal(modalDialog(
        title = "Error loading DPD",
        paste0("Unable to continue: ", msg),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
  })

  observeEvent(input$back_btn_results, {
    current_page("1_dpd")
  })

  output$page2_summary_table <- renderUI({
    req(current_page() == "2_end")

    problem_cols <- run_problem_cols()
    rows <- build_page2_summary_rows(problem_cols)

    row_ui <- lapply(rows, function(x) {
      item_cell <- if (is.null(x$tooltip)) {
        x$item
      } else {
        tagList(
          x$item,
          tags$span(
            icon("circle-question"),
            title = x$tooltip,
            style = "margin-left:6px; cursor:pointer;"
          )
        )
      }

      tags$tr(
        tags$td(item_cell),
        tags$td(x$value)
      )
    })

    tags$table(
      class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Item"),
          tags$th("Selection")
        )
      ),
      tags$tbody(row_ui)
    )
  })

  output$page2_merged_rows <- renderText({
    req(current_page() == "2_end")
    req(!is.null(run_merged()))
    paste0("Rows: ", nrow(run_merged()))
  })

  output$page2_merged_cols_count <- renderText({
    req(current_page() == "2_end")
    req(!is.null(run_merged()))
    paste0("Columns: ", ncol(run_merged()))
  })

  output$page2_merged_columns <- renderUI({
    req(current_page() == "2_end")
    req(!is.null(run_merged()))

    cols <- names(run_merged())
    if (length(cols) == 0) {
      return(tags$ul(tags$li("None")))
    }

    tags$ul(lapply(cols, tags$li))
  })

  output$page2_download_merged <- downloadHandler(
    filename = function() {
      paste0("rdpdexplore_drugcodes_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv_with_notice(run_merged(), file, "Merged CSV download")
    }
  )

  page2_summary_download_df <- reactive({
    build_page2_summary_df(run_problem_cols())
  })

  output$page2_download_summary <- downloadHandler(
    filename = function() {
      paste0("rdpdexplore_summary_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv_with_notice(page2_summary_download_df(), file, "Summary CSV download")
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
          style = paste0("width:", pct, "%; background-color:#E66A6A;")
        )
      )
    )
  })

  output$page_content <- renderUI({
    if (current_page() == "1_dpd") {
      selected_dpd <- dpd_choice()
      can_continue <- !is.null(selected_dpd) && !is.null(keep_f_choice())

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

      tagList(
        div(
          style = "display:flex; align-items:center; gap:6px;",
          h3("HC Drug Product Database", style = "margin:0 0 8px 0;")
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
          style = "display:flex; align-items:center; gap:6px; margin-bottom:6px;",
          strong("Do you want to keep French columns:"),
          tags$span(
            icon("circle-question"),
            title = "Yes: keeps columns ending in '_F'. No: removes all columns ending in '_F'.",
            style = "cursor:pointer;"
          )
        ),
        radioButtons(
          "keep_f_cols_choice",
          label = NULL,
          choices = c("Yes" = "yes", "No" = "no"),
          selected = isolate(if (is.null(keep_f_choice())) character(0) else keep_f_choice()),
          inline = TRUE
        ),

        br(),
        div(
          style = "display:flex; align-items:center; gap:10px;",
          if (can_continue) {
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
            style = "display:none; color:#E05F5F; font-weight:600;",
            icon("spinner", class = "fa-spin"),
            " Loading..."
          )
        )
      )
    } else if (current_page() == "2_end") {
      tagList(
        h3("Results"),
        uiOutput("page2_summary_table"),
        downloadButton("page2_download_summary", "Download Summary Table (CSV)"),
        br(),
        br(),
        br(),
        strong(textOutput("page2_merged_rows", inline = TRUE)),
        br(),
        strong(textOutput("page2_merged_cols_count", inline = TRUE)),
        uiOutput("page2_merged_columns"),
        div(
          style = "background-color:#f7f7f7; border:1px solid #ececec; border-radius:4px; padding:10px; margin-bottom:12px;",
          tags$div(
            style = "margin-bottom:8px;",
            strong("Note:"),
            " There is no official DPD dictionary for these column names. Refer to ",
            tags$a(
              href = "https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database/read-file-drug-product-database-data-extract.html",
              target = "_blank",
              "https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database/read-file-drug-product-database-data-extract.html"
            ),
            " for a high-level overview of the data. Columns in this file may be in a different order compared to what is shown there, as commonly-used columns are moved to the left for convenience. The STRENGTH (and STRENGTH_F) columns have been modified to combine the original strength value/unit and dosage value/unit in order to roll up drugs, as it would otherwise be specific to each ingredient."
          ),
          tags$div(
            style = "margin-bottom:8px;",
            strong("Warning:"),
            " This file may contain products that are veterinary, do not have DINs, or do not conventionally count as drugs (e.g., allergens, contrast media, diagnostic agents)."
          ),
          tags$div(
            strong("Warning:"),
            " Only the current status of the drug is recorded in this file (if the DPD had multiple statuses on the same day, a tiebreaker was applied that prioritized CANCELLED statuses over DORMANT, MARKETED, or APPROVED). If you wish to explore historical statuses, use the raw DPD status files in the /data folder."
          )
        ),
        downloadButton("page2_download_merged", "Download DPD (CSV)"),
        br(),
        br(),
        actionButton("back_btn_results", "Back")
      )
    }
  })
}



# Run Shiny app ----------------------------------------------------------------

options(shiny.launch.browser = TRUE)
shinyApp(ui, server)
