


###############################################################################

generate_qc_report_logic <- function(env, call_id, user_name, sqlfile, htmlfile, maxpoints) {
  require(htmltools)
  # 1. Validation & Setup
  #---------------------------
  if (sqlfile == "" || htmlfile == "") {
    stop("Missing required file paths (sqlfile or htmlfile)")
  }
  # Ensure the global connection is alive
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = env)
  } 
  conn <- global_conn #global_conn is establlished with launcher script or the line above
  e <- try(inbolimsintern::write_db_log("Starting data import from Plumber", "P"))
  
  #return(paste("gotten just before data processing", e))
  e <- try(pandoc_dir <- get_lims_constant(conn, "PANDOC_DIR"))
  if (inherits(e, "try-error")) return(e)
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
  Sys.setenv(PATH = paste(pandoc_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  
  
  
  #2. data processing
  #-------------------------
  alldata <- DBI::dbGetQuery(global_conn, paste(readLines(sqlfile), collapse = "\n"))
  # 3. Processing (The core of your script)
  combis <- alldata |>
    dplyr::select(combi) |>
    dplyr::distinct() |>
    tidyr::separate(col = "combi", into = c("ana", "qc", "comp"), sep = "---", remove = FALSE) |>
    dplyr::arrange(ana, comp, qc)
  
  plot_widgets <- vector("list", nrow(combis))
  names(plot_widgets) <- combis$combi
  fig_height <- 600
  
  # Loop through combinations to create widgets
  for (i in seq_len(nrow(combis))) {
    comb <- combis$combi[i]
    tryCatch({
      subtitle <- paste("analyse:", combis$ana[i], "qc sample:", combis$qc[i], "component:", combis$comp[i])
      plotdata <- alldata |> dplyr::filter(combi == comb)
      htmldata <- elc_htmldata(plotdata)
      
      fig <- ELC_shewhart_plot(
        subdata = htmldata[["plot"]],
        interactive = TRUE,
        title = subtitle,
        fig_height = fig_height
      )
      #fig <- plotly::toWebGL(fig) #convert to webgl for speed, maar kan self-contained breken
      
      plot_widgets[[comb]][["fig"]] <- fig
      
      plot_widgets[[comb]][["smry"]] <- DT::datatable(htmldata[["summary"]], options = list(pageLength = 10))
      plot_widgets[[comb]][["data"]] <- DT::datatable(htmldata[["tabel"]], options = list(pageLength = 25))
      plot_widgets[[comb]][["out3s"]] <- DT::datatable(htmldata[["out3s"]], options = list(pageLength = 10))
    }, error = function(e) {
      write_db_log(paste("Widget failed:", comb), "E")
    })
  }
  
  #return(paste("looping finished, data was ", nrow(alldata)))
  
  # 4. Build HTML & Save
  
  
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
  
  
  
  
  
  # (Include your CSS and Layout assembly code here...)
  e <- try(custom_css <- inbolimsintern::get_labware_css())
  if (inherits(e, "try-error")) return(e)
  
  # Build TOC
  e <- try(toc_widget <- htmltools::tags$ul(class = "toc-list", toc_items))
  if (inherits(e, "try-error")) return("toc build", e)
  
  # Assemble complete layout
  e <- try(
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
  )
  if (inherits(e, "try-error")) return(e)
  
  
  # Final Save
  placeholder <- htmlwidgets::createWidget("html", list(), package = "htmlwidgets")
  output <- htmlwidgets::prependContent(placeholder, layout)
  
  e <- try(target_dir <- dirname(htmlfile))
  if (inherits(e, "try-error")) return(paste("target dir", e))
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  filename <- normalizePath(htmlfile, mustWork = FALSE)
  
  e <- try(
    htmlwidgets::saveWidget(
      widget = output,
      file = filename,
      selfcontained = TRUE,
      libdir = NULL
    ))
  saveRDS(output, file = "output.RDS")
  #return(paste("saving widgets", e, "\nclass output:", class(output)))
  
  
  write_db_log("QC charts saved via Plumber", "C")
  return(paste(status = "Success\n file = ", htmlfile))
}

      
      
      
      
      
      
      
      


################################################################################

multiply_logic <- function(a, b) {
  as.numeric(a) * as.numeric(b)
}


################################################################################

batch_file_grabber_logic <- function(env, scheduler_path) {
  library(tidyverse)
  library(inbolimsintern)
  library(readxl)
  
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = env)
  } 
  conn <- global_conn #global_conn is establlished with launcher script or the line above
  
  if (scheduler_path == "") {
    stop("Missing required scheduler path")
  }
  
  e <- try({
    scheduler_base_dir <- scheduler_path #_scheduler/ANA
    grabloc_batchimport <- paste0(scheduler_base_dir, "\\BATCH_IMPORT")
    grabloc_projaanvraag <- paste0(scheduler_base_dir, "\\PROJECTAANVRAAG")
    grabloc_sampreg <- paste0(scheduler_base_dir, "\\STAALONTVANGST")
    grabloc_veldformulier <- paste0(scheduler_base_dir, "\\VELDFORMULIER")
  })
}



################################################################################
################################################################################
################################################################################

# generate_qc_report_logic_working <- function(env, call_id, user_name, sqlfile, htmlfile, maxpoints) {
#   require(htmltools)
#   # 1. Validation & Setup
#   #---------------------------
#   if (sqlfile == "" || htmlfile == "") {
#     stop("Missing required file paths (sqlfile or htmlfile)")
#   }
#   # Ensure the global connection is alive
#   if (!DBI::dbIsValid(global_conn)) {
#     global_conn <<- inbolimsintern::limsdb_connect(env = env)
#   } 
#   conn <- global_conn #global_conn is establlished with launcher script or the line above
#   e <- try(inbolimsintern::write_db_log("Starting data import from Plumber", "P"))
#   
#   #return(paste("gotten just before data processing", e))
#   e <- try(pandoc_dir <- get_lims_constant(conn, "PANDOC_DIR"))
#   if (inherits(e, "try-error")) return(e)
#   Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
#   Sys.setenv(PATH = paste(pandoc_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
#   
#   
#   
#   #2. data processing
#   #-------------------------
#   alldata <- DBI::dbGetQuery(global_conn, paste(readLines(sqlfile), collapse = "\n"))
#   # 3. Processing (The core of your script)
#   combis <- alldata |>
#     dplyr::select(combi) |>
#     dplyr::distinct() |>
#     tidyr::separate(col = "combi", into = c("ana", "qc", "comp"), sep = "---", remove = FALSE) |>
#     dplyr::arrange(ana, comp, qc)
#   
#   plot_widgets <- vector("list", nrow(combis))
#   names(plot_widgets) <- combis$combi
#   fig_height <- 600
#   
#   # Loop through combinations to create widgets
#   for (i in seq_len(nrow(combis))) {
#     comb <- combis$combi[i]
#     tryCatch({
#       subtitle <- paste("analyse:", combis$ana[i], "qc sample:", combis$qc[i], "component:", combis$comp[i])
#       plotdata <- alldata |> dplyr::filter(combi == comb)
#       htmldata <- elc_htmldata(plotdata)
#       
#       fig <- ELC_shewhart_plot(
#         subdata = htmldata[["plot"]],
#         interactive = TRUE,
#         title = subtitle,
#         fig_height = fig_height
#       )
#       fig <- plotly::toWebGL(fig) #convert to webgl for speed
#       
#       plot_widgets[[comb]][["fig"]] <- fig
#       
#       plot_widgets[[comb]][["smry"]] <- DT::datatable(htmldata[["summary"]], options = list(pageLength = 10))
#       plot_widgets[[comb]][["data"]] <- DT::datatable(htmldata[["tabel"]], options = list(pageLength = 25))
#       plot_widgets[[comb]][["out3s"]] <- DT::datatable(htmldata[["out3s"]], options = list(pageLength = 10))
#     }, error = function(e) {
#       write_db_log(paste("Widget failed:", comb), "E")
#     })
#   }
#   
#   #return(paste("looping finished, data was ", nrow(alldata)))
#   
#   # 4. Build HTML & Save
#   
#   
#   #// BUILD HTML STRUCTURE
#   ##==================
#   
#   #log_message("Building HTML structure...")
#   write_db_log("Building HTML content", "P")
#   cat("building html content \n")
#   
#   start_time <- Sys.time()
#   
#   # Pre-allocate lists
#   toc_items <- vector("list", length(plot_widgets))
#   content_blocks <- vector("list", length(plot_widgets))
#   
#   # Build TOC and content blocks efficiently
#   #log_message(paste("  Processing", length(plot_widgets), "combinations for HTML..."))
#   
#   for (i in seq_along(plot_widgets)) {
#     comb <- names(plot_widgets)[i]
#     #log_message(paste("  Building HTML for", i, "of", length(plot_widgets), ":", comb))
#     
#     tryCatch({
#       # Parse combination components
#       qc <- sub(".*?---(.*?)---.*", "\\1", comb)
#       comp <- sub(".*---", "", comb)
#       section_id <- gsub("[^a-zA-Z0-9]", "_", comb)
#       
#       # Build TOC item
#       toc_items[[i]] <- htmltools::tags$li(
#         htmltools::tags$a(href = paste0("#", section_id), paste(comp, "-", qc))
#       )
#       
#       # Build content block
#       has_outliers <- nrow(plot_widgets[[comb]][["out3s"]]$x$data) > 0
#       
#       content_blocks[[i]] <- htmltools::tags$div(
#         id = section_id,
#         class = "qc-section",
#         htmltools::tags$h2(paste0("Component: ", comp)),
#         htmltools::tags$h3(paste0("QC: ", qc)),
#         htmltools::tags$div(
#           class = "plot-container",
#           style = paste0("height: ", fig_height, "px;"),
#           plot_widgets[[comb]][["fig"]]
#         ),
#         htmltools::tags$h4("Samenvattende gegevens"),
#         plot_widgets[[comb]][["smry"]],
#         htmltools::tags$h4("Bijhorende tabel"),
#         plot_widgets[[comb]][["data"]],
#         if (has_outliers) {
#           tagList(
#             htmltools::tags$h4("Buiten 3s limieten"),
#             plot_widgets[[comb]][["out3s"]]
#           )
#         }
#       )
#       
#       #log_message(paste("    HTML block created"))
#       
#     }, error = function(err) {
#       #log_message(paste("    ERROR building HTML for:", comb, "-", err$message))
#       write_db_log(paste("HTML building failed for", comb), "E")
#       stop(err)
#     })
#   }
#   
#   html_build_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#   #log_message(paste("HTML blocks built in", round(html_build_time, 2), "seconds"))
#   
#   
#   
#   
#   
#   # (Include your CSS and Layout assembly code here...)
#   e <- try(custom_css <- inbolimsintern::get_labware_css())
#   if (inherits(e, "try-error")) return(e)
#   
#   # Build TOC
#   e <- try(toc_widget <- htmltools::tags$ul(class = "toc-list", toc_items))
#   if (inherits(e, "try-error")) return("toc build", e)
#   
#   # Assemble complete layout
#   e <- try(
#     layout <- tagList(
#       htmltools::tags$head(
#         htmltools::tags$title("QC Chart Report"),
#         htmltools::tags$meta(charset = "UTF-8"),
#         htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
#         if (nchar(custom_css) > 0) htmltools::tags$style(HTML(custom_css))
#       ),
#       htmltools::tags$body(
#         # Fixed sidebar TOC
#         htmltools::tags$div(
#           class = "vw_nav_panel sidebar-toc",
#           style = "position: fixed; top: 0; left: 0; width: 280px; padding: 20px 10px; background-color: #f9f9f9; border-right: 1px solid #ccc; height: 100vh; overflow-y: auto;",
#           htmltools::tags$h3("Inhoudsopgave"),
#           toc_widget
#         ),
#         # Main content area
#         htmltools::tags$div(
#           class = "main-content",
#           style = "margin-left: 300px; padding: 20px;",
#           htmltools::tags$h1("Controlekaart Rapport"),
#           htmltools::tags$h2("Leeswijzer"),
#           htmltools::tags$p(
#             "De blauwe punten worden niet gebruikt bij de berekeningen. ",
#             "Hoe donkerder de blauwe bol, hoe eerder in de batch die voorkomt. ",
#             "Overtredingen van regels worden in de figuur en tabel aangeduid."
#           ),
#           htmltools::tags$ul(
#             htmltools::tags$li("Punt telt niet mee: blauwe bol, eval = ."),
#             htmltools::tags$li("Correct punt: groene bol, eval = ------"),
#             htmltools::tags$li("R1: Buiten 3 sigma: rode bol, eval = R1"),
#             htmltools::tags$li("R2a: 2 opeenvolgend buiten 2 sigma, zelfde kant: rode bol, eval = R2"),
#             htmltools::tags$li("R3: 9 opeenvolgende buiten aan zelfde kant gemiddelde: gele bol, eval = R3"),
#             htmltools::tags$li("R4: 6 opeenvolgende met toenemede of dalende trend: gele bol, eval = R4")
#           ),
#           htmltools::tags$hr(),
#           content_blocks
#         )
#       )
#     )
#   )
#   if (inherits(e, "try-error")) return(e)
#   
#   
#   # Final Save
#   placeholder <- htmlwidgets::createWidget("html", list(), package = "htmlwidgets")
#   output <- htmlwidgets::prependContent(placeholder, layout)
#   
#   e <- try(target_dir <- dirname(htmlfile))
#   if (inherits(e, "try-error")) return(paste("target dir", e))
#   if (!dir.exists(target_dir)) {
#     dir.create(target_dir, recursive = TRUE)
#   }
#   
#   filename <- normalizePath(htmlfile, mustWork = FALSE)
#   
#   e <- try(
#     htmlwidgets::saveWidget(
#       widget = output,
#       file = filename,
#       selfcontained = TRUE,
#       libdir = NULL
#     ))
#   saveRDS(output, file = "output.RDS")
#   #return(paste("saving widgets", e, "\nclass output:", class(output)))
#   
#   
#   write_db_log("QC charts saved via Plumber", "C")
#   return(paste(status = "Success\n file = ", htmlfile))
# }
# 
