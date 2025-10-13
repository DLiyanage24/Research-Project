
library(shiny)
library(r2d3)
library(shinyjs)
library(tibble)
library(dplyr)
library(DBI)
library(RSQLite)
library(jsonlite)
library(speechcollectr)

# DB setup
db_path <- file.path(
  "/Users/dinuwanthiliyanage/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Project/Combined App",
  "app_data.sqlite"
)

init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS recordings (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT    NOT NULL,
      session_id     TEXT    NOT NULL,
      trial_n        INTEGER NOT NULL,
      saved_at       TEXT    NOT NULL,
      filename       TEXT    NOT NULL,
      filepath       TEXT    NOT NULL
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS highlighted_regions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT,
      session_id     TEXT,
      timestamp      TEXT,
      plotIndex      INTEGER,
      selected_json  TEXT,
      n_selected     INTEGER,
      correct        INTEGER,
      draw_started_at_ms REAL,
      draw_ended_at_ms   REAL,
      draw_duration_ms   REAL
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS plot_annotations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT,
      session_id     TEXT,
      timestamp      TEXT,
      plotIndex      INTEGER,
      annotation     TEXT
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS clicks (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT,
      session_id     TEXT,
      timestamp      TEXT,
      plotIndex      INTEGER,
      selection_rank INTEGER,
      action        TEXT,
      time_from_start_sec REAL
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS demographics (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT,
      session_id     TEXT,
      age_range      TEXT,
      education_level TEXT
    )
  ")
  
  con
}


# Append a data frame to a DB table, after normalizing R types to DB-friendly strings/ints
append_df <- function(con, table, df) {
  if (!nrow(df)) return(invisible(TRUE))
  for (nm in names(df)) {
    if (inherits(df[[nm]], "POSIXt")) df[[nm]] <- format(df[[nm]], "%Y-%m-%d %H:%M:%S")
    else if (is.logical(df[[nm]]))   df[[nm]] <- as.integer(df[[nm]])
  }
  dbWriteTable(con, table, df, append = TRUE, row.names = FALSE)
  invisible(TRUE)
}

#  lineup data generation
generate_lineup_data <- function(a_target = 0.06, sd_null = 0.6, sd_target = 0.55) {
  n_panels <- 20; n_pts <- 60
  x <- seq(0, 15, length.out = n_pts)
  target_idx <- sample(seq_len(n_panels), 1)
  
  plots <- lapply(seq_len(n_panels), function(i) {
    if (i == target_idx) {
      x0 <- runif(1, 7, 12); m <- runif(1, 0.1, 0.4); b <- runif(1, -3, 3)
      y  <- a_target * (x - x0)^2 + m*x + b + rnorm(n_pts, sd = sd_target)
      list(x = as.numeric(x), y = as.numeric(y), type = "target")
    } else {
      m <- runif(1, 0.2, 0.6); b <- runif(1, -2, 2)
      y <- m*x + b + rnorm(n_pts, sd = sd_null)
      list(x = as.numeric(x), y = as.numeric(y), type = "null")
    }
  })
  list(plots = plots, target = target_idx)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# server 
server <- function(input, output, session) {
  
  con <- init_db()
  onStop(function() try(dbDisconnect(con), silent = TRUE))
  
  participant_id <- paste0("P-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(session$token, 1, 6))
  session_id     <- paste0("S-", format(Sys.time(), "%Y%m%dT%H%M%S"), "-", substr(session$token, 1, 6))
  
  sessionStart <- reactiveVal(Sys.time())
  
  buf <- reactiveValues(
    lineup  = generate_lineup_data(),
    regions = list()
  )
  rvs <- reactiveValues(trial_n = 0)
  
  # Reser summary tab
  reset_summary_inputs <- function() {
    updateSelectInput(session, "sum_sel1", selected = "-")
    updateTextAreaInput(session, "sum_why1", value = "")
    updateSelectInput(session, "sum_sel2", selected = "-")
    updateTextAreaInput(session, "sum_why2", value = "")
  }
  
  
  
  # Track last clicked plot for highlight guard
  last_clicked <- reactiveVal(NULL)
  
  # Click ranking (per Highlight session, increments on every click)
  rv_click <- reactiveValues(event_rank = 0)
  
  JS_PATH <- "you-draw-it-v2.js"  
  
  #Consent to Demographics 
  observeEvent(input$consent_continue, {
    if (isTRUE(input$consent_ok)) {
      updateTabsetPanel(session, "topnav", selected = "Demographics")
    } else {
      showModal(modalDialog("Please check “I agree” to continue.", easyClose = TRUE))
    }
  })
  
  #  Demographics to Lineup 
  observeEvent(input$demo_continue, {
    demo_row <- tibble(
      participant_id = participant_id,
      session_id     = session_id,
      age_range      = input$demo_exp %||% NA_character_,
      education_level= input$education %||% NA_character_
    )
    append_df(con, "demographics", demo_row)
    updateTabsetPanel(session, "topnav", selected = "Lineup")
  })
  
  #  Talk Aloud 
  observeEvent(input$choose_talk, {
    # Make Talk tab visible and set initial button visibility/status text
    showTab("topnav", "Talk")
    updateTabsetPanel(session, "topnav", selected = "Talk")
    shinyjs::show("rec_start")
    shinyjs::hide("rec_stop")
    shinyjs::hide("talk_lineup_wrap")
    shinyjs::hide("goto_highlight")
    output$rec_status <- renderText("")
  })
  
  observeEvent(input$rec_start, {
    # Start a new recording trial and render the lineup in record mode
    rvs$trial_n <- rvs$trial_n + 1
    output$lineup_talk <- renderD3({
      r2d3(data = buf$lineup, script = JS_PATH, options = list(mode = "record", annotation = FALSE))
    })
    # Start microphone recording via speechcollectr
    msg <- tryCatch({ startRec(); NULL }, error = function(e) as.character(e))
    if (is.null(msg)) {
      shinyjs::disable("rec_start")
      shinyjs::show("rec_stop")
      shinyjs::show("talk_lineup_wrap")
      output$rec_status <- renderText("Recording… Click 'Stop & save' when you're done.")
    } else {
      output$rec_status <- renderText(paste("Could not start recorder:", msg))
    }
  })
  
  observeEvent(input$rec_stop, {
    shinyjs::enable("rec_start"); shinyjs::hide("rec_stop")
    output$rec_status <- renderText("Stopping and saving…")
    ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
    fname <- sprintf("%s_rec_%s_%03d.wav", participant_id, ts, rvs$trial_n)
    msg <- tryCatch({ stopRec(filename = fname, finishedId = "rec_done"); NULL },
                    error = function(e) as.character(e))
    if (!is.null(msg)) {
      output$rec_status <- renderText(paste("Could not stop/save recorder:", msg))
    }
  })
  
  observeEvent(input$rec_done, {
    wav_path <- normalizePath(as.character(input$rec_done), winslash = "/", mustWork = FALSE)
    if (!nzchar(wav_path) || !file.exists(wav_path)) {
      output$rec_status <- renderText("Recorder finished, but no file was found. If you denied mic permission, allow it and try again.")
      return()
    }
    # Log the recording data
    saved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
    dbExecute(
      con,
      "INSERT INTO recordings (participant_id, session_id, trial_n, saved_at, filename, filepath)
       VALUES (?, ?, ?, ?, ?, ?);",
      params = list(participant_id, session_id, rvs$trial_n, saved_at, basename(wav_path), wav_path)
    )
    output$rec_status <- renderText("File saved! Thank you.")
    shinyjs::show("goto_highlight")
  })
  
  # Highlight
  observeEvent(input$choose_highlight, {
    # Enter Highlight mode, clear buffers & counters, render lineup with highlight tools
    showTab("topnav", "Highlight")
    updateTabsetPanel(session, "topnav", selected = "Highlight")
    buf$regions <- list()
    last_clicked(NULL)
    rv_click$event_rank <- 0
    output$lineup_highlight <- renderD3({
      r2d3(data = buf$lineup, script = JS_PATH, options = list(mode = "highlight", annotation = FALSE))
    })
  })
  observeEvent(input$goto_highlight, {
    showTab("topnav", "Highlight")
    updateTabsetPanel(session, "topnav", selected = "Highlight")
    buf$regions <- list()
    last_clicked(NULL)
    rv_click$event_rank <- 0
    output$lineup_highlight <- renderD3({
      r2d3(data = buf$lineup, script = JS_PATH, options = list(mode = "highlight", annotation = FALSE))
    })
  })
  
  # When user clicks Undo drop the last region for that plot
  observeEvent(input$highlight_cleared, {
    info <- input$highlight_cleared
    if (is.null(info$plotIndex)) return()
    pidx <- as.integer(info$plotIndex)
    
    # buf$regions is a list of 1-row tibbles; remove the last one for this plot
    if (length(buf$regions)) {
      matches <- which(vapply(
        buf$regions,
        function(tb) tryCatch(as.integer(tb$plotIndex[1]) == pidx, error = function(...) FALSE),
        logical(1)
      ))
      if (length(matches)) {
        # remove the most recent (last) match
        buf$regions <- buf$regions[-tail(matches, 1)]
      }
    }
  }, ignoreInit = TRUE)
  
  
  
  

  
  # Log all clicks (select + deselect) with running rank
  observeEvent(input$plot_clicked, {
    pc <- input$plot_clicked
    idx <- if (is.list(pc)) as.integer(pc$plotIndex) else as.integer(pc)
    act <- if (is.list(pc) && !is.null(pc$action)) as.character(pc$action) else "select"
    
    # increment running rank for every click
    rv_click$event_rank <- rv_click$event_rank + 1
    ts_now <- Sys.time()
    
    dbExecute(
      con,
      "INSERT INTO clicks
     (participant_id, session_id, timestamp, plotIndex, selection_rank, action, time_from_start_sec)
     VALUES (?, ?, ?, ?, ?, ?, ?);",
      params = list(
        participant_id,
        session_id,
        format(ts_now, "%Y-%m-%d %H:%M:%S"),
        idx,
        rv_click$event_rank,  
        act,                  
        as.numeric(difftime(ts_now, sessionStart(), units = 'secs'))
      )
    )
    
    # keep last_clicked only for selects
    if (identical(act, "select")) {
      last_clicked(idx)
    } else {
      last_clicked(NULL)
    }
  }, ignoreInit = TRUE)
  
  
  # Polygons from JS
  observeEvent(input$highlighted_region, {
    ann <- input$highlighted_region
    if (is.null(ann) || is.null(ann$selectedPoints)) return()
    buf$regions <- append(buf$regions, list(
      tibble(
        participant_id     = participant_id,
        session_id         = session_id,
        timestamp          = Sys.time(),
        plotIndex          = ann$plotIndex,
        selected_json      = as.character(jsonlite::toJSON(ann$selectedPoints, auto_unbox = TRUE)),
        n_selected         = length(ann$selectedPoints),
        correct            = buf$lineup$target,
        draw_started_at_ms = ann$drawStartedAt %||% NA_real_,
        draw_ended_at_ms   = ann$drawEndedAt   %||% NA_real_,
        draw_duration_ms   = ann$durationMs    %||% NA_real_
      )
    ))
  }, ignoreInit = TRUE)
  
  # enforce highlight the selected plot rule before moving to forward
  observeEvent(input$save_highlight, {
    regions_df <- if (length(buf$regions)) bind_rows(buf$regions) else tibble()
    if (!nrow(regions_df)) {
      showModal(modalDialog(
        title = "Please highlight before continuing",
        "You must highlight at least one plot before clicking “Save & Next”.",
        easyClose = TRUE
      ))
      return()
    }
    if (!is.null(last_clicked())) {
      if (!any(regions_df$plotIndex == last_clicked())) {
        showModal(modalDialog(
          title = "Highlight the selected plot",
          sprintf("Please highlight key features on plot %d (the one you selected) before continuing.", last_clicked()),
          easyClose = TRUE
        ))
        return()
      }
    }
    append_df(con, "highlighted_regions", regions_df)
    buf$regions <- list()
    showTab("topnav", "Summary")
    updateTabsetPanel(session, "topnav", selected = "Summary")
    reset_summary_inputs()
    output$lineup_summary <- renderD3({
      r2d3(data = buf$lineup, script = JS_PATH, options = list(mode = "plain", annotation = FALSE))
    })
  })
  
  
  
  # Summary / Done
  observeEvent(input$choose_summary, {
    showTab("topnav", "Summary")
    updateTabsetPanel(session, "topnav", selected = "Summary")
    reset_summary_inputs()
    output$lineup_summary <- renderD3({
      r2d3(data = buf$lineup, script = JS_PATH, options = list(mode = "plain", annotation = FALSE))
    })
  })
  

  
  observeEvent(input$submit_summary, {
    # Normalize "-" into NA
    sel1 <- if (input$sum_sel1 == "-" || is.null(input$sum_sel1)) NA_integer_ else as.integer(input$sum_sel1)
    sel2 <- if (input$sum_sel2 == "-" || is.null(input$sum_sel2)) NA_integer_ else as.integer(input$sum_sel2)
    
    why1 <- trimws(input$sum_why1 %||% "")
    why2 <- trimws(input$sum_why2 %||% "")
    
    #  Selection 1 is mandatory (plot + reason)
    if (is.na(sel1) || !nzchar(why1)) {
      showModal(modalDialog(
        title = "First selection required",
        "Please select at least one plot and provide a reason before submitting.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Selection 2 optional; but if chosen, must give reason
    if (!is.na(sel2) && !nzchar(why2)) {
      showModal(modalDialog(
        title = "Reason required for Selection 2",
        "You selected a second plot. Please provide a reason for Selection 2 before submitting.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Build rows
    rows <- list(
      tibble(
        participant_id = participant_id,
        session_id     = session_id,
        timestamp      = Sys.time(),
        plotIndex      = sel1,
        annotation     = why1
      )
    )
    
    if (!is.na(sel2) && nzchar(why2)) {
      rows <- append(rows, list(tibble(
        participant_id = participant_id,
        session_id     = session_id,
        timestamp      = Sys.time(),
        plotIndex      = sel2,
        annotation     = why2
      )))
    }
    
    append_df(con, "plot_annotations", dplyr::bind_rows(rows))
    updateTabsetPanel(session, "topnav", selected = "Done")
  })

  
  # Back buttons
  observeEvent(input$talk_back,      { updateTabsetPanel(session, "topnav", selected = "Lineup") })
  observeEvent(input$highlight_back, { updateTabsetPanel(session, "topnav", selected = "Lineup") })
  observeEvent(input$summary_back,   { updateTabsetPanel(session, "topnav", selected = "Lineup") })
}
