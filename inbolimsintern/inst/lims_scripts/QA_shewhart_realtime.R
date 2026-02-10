##################################
# ELC - Make QC Chart HTML Report
# Optimized version with improved performance and diagnostics
##################################

#// SETUP ENVIRONMENT
##=====================

# Initialize logging
logfile <- "D:/LABO_FS_PRD/_PIETER/qccharts.log"
# 
# # Graphics device configuration for server-side execution
# options(bitmapType = 'cairo')
# if (!interactive()) {
#   pdf(NULL)
# }

#// SESSION SETUP
##=====================

library(htmltools)
library(inbolimsintern)

#log_message("Setting up R session...")
args <- commandArgs(trailingOnly = TRUE)
#library(inbolimsintern);library(htmltools); args <- c("LWL8PRD", "11268", "PIETERVS")
fig_height <- 600

setup <- try(r_session_setup(args), silent = TRUE)
if (inherits(setup, "try-error")) {
  #log_message(paste("ERROR in session setup:", as.character(setup)))
  write_db_log("Session setup failed", "E")
  stop("Problem with session setup")
}

invisible(list2env(setup, envir = .GlobalEnv))
#log_message("Session setup complete")
write_db_log("R session initialized", "P")

#// RETRIEVE ARGUMENTS
##=====================

#log_message("Retrieving parameters...")
e <- try({
  maxpoints_orig <- 30
  sqlfile  <- params |> dplyr::filter(ARG_NAME == "SQL_FILE") |> dplyr::pull(VALUE)
  htmlfile <- params |> dplyr::filter(ARG_NAME == "HTML_FILE") |> dplyr::pull(VALUE)
  maxpoints <- params |> 
    dplyr::filter(ARG_NAME == "MAX_POINTS") |> 
    dplyr::pull(VALUE) |> 
    unique() |> 
    as.integer()
  
  if (inherits(maxpoints, "try-error") || !length(maxpoints)) {
    maxpoints <- maxpoints_orig
  }
  if (length(maxpoints) > 1) {
    maxpoints <- max(maxpoints)
  }
}, silent = TRUE)

if (inherits(e, "try-error")) {
  #log_message(paste("ERROR retrieving parameters:", as.character(e)))
  write_db_log("Parameter retrieval failed", "E")
  stop(e)
}

#log_message(paste("SQL file:", sqlfile))
#log_message(paste("HTML file:", htmlfile))
#log_message(paste("Max points:", maxpoints))
write_db_log("Parameters retrieved", "P")

#// IMPORT DATA
##================

#log_message("Importing data from database...")
write_db_log("Starting data import", "P")

start_time <- Sys.time()
# e <- try(alldata <- get_ELC_data(conn, sqlfile, keep = maxpoints), silent = TRUE)
# if (inherits(e, "try-error")) {
#   #log_message(paste("ERROR importing data:", as.character(e)))
#   write_db_log("Data import failed", "E")
#   stop(e)
# }

e <- try(alldata <- DBI::dbGetQuery(conn, paste(readLines(sqlfile), collapse = "\n")))
if (inherits(e, "try-error")) {
  #log_message(paste("ERROR importing data:", as.character(e)))
  write_db_log("Data import failed", "E")
  stop(e)
}
write_db_log(paste("Data imported:", nrow(alldata), "rijen"), "P")

import_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat("data import tijd: ", round(import_time,2), "s\n")

#log_message(paste("Data imported successfully in", round(import_time, 2), "seconds"))
#log_message(paste("Total rows:", nrow(alldata)))

# Parse combinations
#log_message("Parsing combinations...")
combis <- alldata |>
  dplyr::select(combi) |>
  dplyr::distinct() |>
  tidyr::separate(col = "combi", into = c("ana", "qc", "comp"), sep = "---", remove = FALSE) |>
  dplyr::arrange(ana, comp, qc)

#log_message(paste("Total combinations:", nrow(combis)))
#log_message(paste("Combinations:", paste(combis$combi, collapse = ", ")))

#// CREATE WIDGETS
##==================

#log_message("Creating plot widgets...")
write_db_log("Starting widget creation", "P")

plot_widgets <- vector("list", nrow(combis))
names(plot_widgets) <- combis$combi

start_time <- Sys.time()

for (i in seq_len(nrow(combis))) {
  comb <- combis$combi[i]
  #log_message(paste("  Processing widget", i, "of", nrow(combis), ":", comb))
  write_db_log(paste("start creating widget", comb), "P")
  cat(comb, "\n")
  tryCatch({
    # Prepare subtitle
    subtitle <- paste(
      "\n",
      paste0("analyse:   ", combis$ana[i]),
      paste0("qc sample: ", combis$qc[i]),
      paste0("component: ", combis$comp[i]),
      sep = " "
    )
    
    # Filter and process data
    write_db_log("before filter combi", "P")
    plotdata <- alldata |> dplyr::filter(combi == comb)
    write_db_log("before elc htmldata", "P")
    htmldata <- elc_htmldata(plotdata)
    write_db_log("after html data, before ELC-shewhart-plot", "P")
    #log_message(paste("    Rows:", nrow(plotdata)))
    
    # Create plot
    plot_widgets[[comb]][["fig"]] <- ELC_shewhart_plot(
      subdata = htmldata[["plot"]],
      interactive = TRUE,
      title = subtitle,
      fig_height = fig_height
    )
    write_db_log("after html data, after ELC-shewhart-plot", "P")
    
    
    # Create tables
    plot_widgets[[comb]][["smry"]] <- DT::datatable(htmldata[["summary"]], options = list(pageLength = 10))
    plot_widgets[[comb]][["data"]] <- DT::datatable(htmldata[["tabel"]], options = list(pageLength = 25))
    plot_widgets[[comb]][["out3s"]] <- DT::datatable(htmldata[["out3s"]], options = list(pageLength = 10))
    
    #log_message(paste("    Widget created successfully"))
    
  }, error = function(err) {
    #log_message(paste("    ERROR creating widget:", comb, "-", err$message))
    write_db_log(paste("Widget creation failed for", comb), "E")
    stop(err)
  })
  write_db_log(paste("finished creating widget", comb), "P")
  
}

widget_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#log_message(paste("All widgets created in", round(widget_time, 2), "seconds"))
write_db_log("Widgets created successfully", "P")

#// LOAD CSS
##================

#log_message("Loading custom CSS...")
custom_css <- tryCatch({
  inbolimsintern::get_labware_css()
}, error = function(err) {
  #log_message(paste("Warning: Could not load custom CSS:", err$message))
  ""
})
#log_message(paste("CSS loaded, size:", nchar(custom_css), "characters"))

#// BUILD HTML STRUCTURE
##==================

#log_message("Building HTML structure...")
write_db_log("Building HTML content", "P")
cat("building html content \n")

start_time <- Sys.time()

# Pre-allocate lists
toc_items <- vector("list", length(plot_widgets))
content_blocks <- vector("list", length(plot_widgets))

# Build TOC and content blocks efficiently
#log_message(paste("  Processing", length(plot_widgets), "combinations for HTML..."))

for (i in seq_along(plot_widgets)) {
  comb <- names(plot_widgets)[i]
  #log_message(paste("  Building HTML for", i, "of", length(plot_widgets), ":", comb))
  
  tryCatch({
    # Parse combination components
    qc <- sub(".*?---(.*?)---.*", "\\1", comb)
    comp <- sub(".*---", "", comb)
    section_id <- gsub("[^a-zA-Z0-9]", "_", comb)
    
    # Build TOC item
    toc_items[[i]] <- htmltools::tags$li(
      htmltools::tags$a(href = paste0("#", section_id), paste(comp, "-", qc))
    )
    
    # Build content block
    has_outliers <- nrow(plot_widgets[[comb]][["out3s"]]$x$data) > 0
    
    content_blocks[[i]] <- htmltools::tags$div(
      id = section_id,
      class = "qc-section",
      htmltools::tags$h2(paste0("Component: ", comp)),
      htmltools::tags$h3(paste0("QC: ", qc)),
      htmltools::tags$div(
        class = "plot-container",
        style = paste0("height: ", fig_height, "px;"),
        plot_widgets[[comb]][["fig"]]
      ),
      htmltools::tags$h4("Samenvattende gegevens"),
      plot_widgets[[comb]][["smry"]],
      htmltools::tags$h4("Bijhorende tabel"),
      plot_widgets[[comb]][["data"]],
      if (has_outliers) {
        tagList(
          htmltools::tags$h4("Buiten 3s limieten"),
          plot_widgets[[comb]][["out3s"]]
        )
      }
    )
    
    #log_message(paste("    HTML block created"))
    
  }, error = function(err) {
    #log_message(paste("    ERROR building HTML for:", comb, "-", err$message))
    write_db_log(paste("HTML building failed for", comb), "E")
    stop(err)
  })
}

html_build_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#log_message(paste("HTML blocks built in", round(html_build_time, 2), "seconds"))

#// ASSEMBLE FINAL LAYOUT
##==================

cat("Assembling final layout...\n")

# Build TOC
toc_widget <- htmltools::tags$ul(class = "toc-list", toc_items)

# Assemble complete layout
layout <- tagList(
  htmltools::tags$head(
    htmltools::tags$title("QC Chart Report"),
    htmltools::tags$meta(charset = "UTF-8"),
    htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    if (nchar(custom_css) > 0) htmltools::tags$style(HTML(custom_css))
  ),
  htmltools::tags$body(
    # Fixed sidebar TOC
    htmltools::tags$div(
      class = "vw_nav_panel sidebar-toc",
      style = "position: fixed; top: 0; left: 0; width: 280px; padding: 20px 10px; background-color: #f9f9f9; border-right: 1px solid #ccc; height: 100vh; overflow-y: auto;",
      htmltools::tags$h3("Inhoudsopgave"),
      toc_widget
    ),
    # Main content area
    htmltools::tags$div(
      class = "main-content",
      style = "margin-left: 300px; padding: 20px;",
      htmltools::tags$h1("Controlekaart Rapport"),
      htmltools::tags$h2("Leeswijzer"),
      htmltools::tags$p(
        "De blauwe punten worden niet gebruikt bij de berekeningen. ",
        "Hoe donkerder de blauwe bol, hoe eerder in de batch die voorkomt. ",
        "Overtredingen van regels worden in de figuur en tabel aangeduid."
      ),
      htmltools::tags$ul(
        htmltools::tags$li("Punt telt niet mee: blauwe bol, eval = ."),
        htmltools::tags$li("Correct punt: groene bol, eval = ------"),
        htmltools::tags$li("R1: Buiten 3 sigma: rode bol, eval = R1"),
        htmltools::tags$li("R2a: 2 opeenvolgend buiten 2 sigma, zelfde kant: rode bol, eval = R2"),
        htmltools::tags$li("R3: 9 opeenvolgende buiten aan zelfde kant gemiddelde: gele bol, eval = R3"),
        htmltools::tags$li("R4: 6 opeenvolgende met toenemede of dalende trend: gele bol, eval = R4")
      ),
      htmltools::tags$hr(),
      content_blocks
    )
  )
)

#log_message("Layout assembled")

#// SAVE HTML FILE
##==================

#log_message("Saving HTML file...")
write_db_log("Saving HTML file", "P")

start_time <- Sys.time()

e <- try({
  # Create placeholder widget
  placeholder <- htmlwidgets::createWidget("html", list(), package = "htmlwidgets")
  
  # Combine content with placeholder
  output <- htmlwidgets::prependContent(placeholder, layout)
  
  # Save to file
  save_report_widget(output, filename = htmlfile)
}, silent = TRUE)

if (inherits(e, "try-error")) {
  #log_message(paste("ERROR saving HTML:", as.character(e)))
  write_db_log("HTML save failed", "E")
  stop(e)
}

save_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#log_message(paste("HTML saved successfully in", round(save_time, 2), "seconds"))
#log_message(paste("File location:", htmlfile))

write_db_log("QC charts saved in HTML", "C")

#// PERFORMANCE SUMMARY
##==================

total_time <- import_time + widget_time + html_build_time + save_time
#log_message("=== Performance Summary ===")
#log_message(paste("  Data import:    ", round(import_time, 2), "sec"))
#log_message(paste("  Widget creation:", round(widget_time, 2), "sec"))
#log_message(paste("  HTML building:  ", round(html_build_time, 2), "sec"))
#log_message(paste("  File saving:    ", round(save_time, 2), "sec"))
#log_message(paste("  TOTAL:          ", round(total_time, 2), "sec"))
#log_message("=== Script Complete ===")

write_db_log(paste("totale duur: ", total_time), "C")
# Open file if running interactively

if (interactive()) {
  shell.exec(htmlfile)
}
