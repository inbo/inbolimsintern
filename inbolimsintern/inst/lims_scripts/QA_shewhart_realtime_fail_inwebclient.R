##################################
#ELC - Make QC chart html report
##################################

#// setup environment
##=====================

logfile = "D:/LABO_FS_PRD/_PIETER/qccharts.log"
#cat("starting Qc charts\n", file = logfile)
#cat("working dir:", getwd(), "\n", file = logfile, append = TRUE)

#library(tidyverse)
library(inbolimsintern)
#library(htmltools)

# Force a null graphics device for server-side execution
options(bitmapType = 'cairo')
pdf(NULL)

#cat("libraries loaded\n", file = logfile, append = TRUE)
#cat("args:\n", commandArgs(trailingOnly = TRUE), "\n", file = logfile, append = TRUE)
args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#args <- c("PRD", "11126", "TEST_INT"); setup <- try(r_session_setup(args))
#library(inbolimsintern);args <- c("PRD", "11186", "PIETERVS"); setup <- try(r_session_setup(args))
fig_height <- 600

if (inherits(setup, "try-error")) {
  #cat("probleem bij setup script", "\n", file = logfile, append = TRUE)
  stop("probleem bij setup script")
}
invisible(list2env(setup, envir = .GlobalEnv))

#cat("finished setting environment \n", file = logfile, append = TRUE)
write_db_log("finished setting R session", 'P')

# NEW: Fetch CSS from package inst/css/inbo_labware_style.css
# This ensures the style is available as a string to be injected
custom_css <- inbolimsintern::get_labware_css() 


#cat("custom css loaded \n", file = logfile, append = TRUE)
#// retrieve arguments
##=====================
#cat('start retrieving arguments\n', file = logfile, append = TRUE)
e <- try({
  maxpoints_orig <- 30 
  sqlfile  <- try(dplyr::filter(params, ARG_NAME == "SQL_FILE") |> dplyr::pull(VALUE))
  #cat(sqlfile, '\n', file = logfile, append = TRUE)
  htmlfile <- try(dplyr::filter(params, ARG_NAME == "HTML_FILE") |> dplyr::pull(VALUE))
  #cat(htmlfile, '\n', file = logfile, append = TRUE)
  maxpoints <- try(dplyr::filter(params, ARG_NAME == "MAX_POINTS") |> dplyr::pull(VALUE) |> unique() |>  as.integer())
  #cat(maxpoints, '\n', file = logfile, append = TRUE)
  if (inherits(maxpoints, "try-error") | !length(maxpoints)) {
    maxpoints <- maxpoints_orig
  }
  if (length(maxpoints) > 1) {
    maxpoints <- max(maxpoints)
  }
  #cat(maxpoints, '\n', file = logfile, append = TRUE)
})
#cat('finished retrieving arguments\n', file = logfile, append = TRUE)

if (inherits(e, "try-error")) {
  #cat("Probleem with arguments: ", e, "\n", file = logfile, append = TRUE)
  write_db_log(e, "E")
  stop(e)
} else {
  write_db_log("Argumenten ingelezen", "P")
}

#// Import data
##================

#cat('start reading dataset\n', file = logfile, append = TRUE)
write_db_log("start import data from db", "P")
e <- try(alldata <- get_ELC_data(conn, sqlfile, keep = maxpoints))
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
}
#cat("start combis\n", file =  logfile, append = TRUE)
combis <- alldata |>
  dplyr::select(combi) |>
  dplyr::distinct() |>
  tidyr::separate(col = "combi", into = c("ana", "qc", "comp"), sep = "---", remove = FALSE) |>
  dplyr::arrange(ana, comp, qc)

#// CREATE WIDGETS
#==================
#cat("start widgets\n", file =  logfile, append = TRUE)
plot_widgets <- list()
write_db_log("creating widgets", "P")

for (i in 1:nrow(combis)) {
  comb <- combis$combi[i]
  subtitle = paste("\n",paste0("analyse:   ", combis$ana[i]),
                   paste0("qc sample: ", combis$qc[i]),
                   paste0("component: ",combis$comp[i]), sep = " ")
  plotdata <- alldata |> dplyr::filter(combi == comb)
  htmldata <- elc_htmldata(plotdata)
  
  plot_widgets[[comb]][["fig"]] <- ELC_shewhart_plot(subdata = htmldata[["plot"]], 
                                                     interactive = TRUE, 
                                                     title = subtitle, 
                                                     fig_height = fig_height)
  plot_widgets[[comb]][["smry"]] <- DT::datatable(htmldata[['summary']])
  plot_widgets[[comb]][["data"]] <- DT::datatable(htmldata[['tabel']])
  plot_widgets[[comb]][["out3s"]] <- DT::datatable(htmldata[['out3s']])
}

#// CREATE HTML
#=================
#cat("start html\n", file =  logfile, append = TRUE)
write_db_log("finished creating widgets", "P")
placeholder <- htmlwidgets::createWidget("html", list(), package = "htmlwidgets")
toc_items <- list()
content_blocks <- list()

#cat("start looping widgets and adding tags\n", file =  logfile, append = TRUE)
#cat(paste(names(plot_widgets), collapse = "\n"), "\n", file = logfile, append = TRUE)
for (comb in names(plot_widgets)) {
  write_db_log(paste("start", comb), 'P')
  #cat("start ", comb, '\n', file = logfile, append = TRUE)
  qc         <- sub(".*?---(.*?)---.*", "\\1", comb)
  comp       <- sub(".*---", "", comb)
  section_id <- gsub("[^a-zA-Z0-9]", "_", comb)
  
  toc_items[[length(toc_items) + 1]] <- tags$li(
    tags$a(href = paste0("#", section_id), paste(comp, "-", qc))
  )
  
  if (nrow(plot_widgets[[comb]][["out3s"]]$x$data) > 0) {
    content_blocks[[length(content_blocks) + 1]] <- tags$div(
      id = section_id,
      tags$h2(paste0("Component: ", comp)),
      tags$h3(paste0("QC: ", qc)),
      tags$div(style = paste0("height: ",fig_height, "px;"), plot_widgets[[comb]][["fig"]]),
      tags$h4("Samenvattende gegevens"), plot_widgets[[comb]][["smry"]],
      tags$h4("Bijhorende tabel"), plot_widgets[[comb]][["data"]],
      tags$h4("Buiten 3s limieten"), plot_widgets[[comb]][["out3s"]]
    )
  } else {
    content_blocks[[length(content_blocks) + 1]] <- tags$div(
      id = section_id,
      tags$h2(paste0("Component: ", comp)),
      tags$h3(paste0("QC: ", qc)),
      tags$div(style = paste0("height: ",fig_height, "px;"), plot_widgets[[comb]][["fig"]]),
      tags$h4("Samenvattende gegevens"), plot_widgets[[comb]][["smry"]],
      tags$h4("Bijhorende tabel"), plot_widgets[[comb]][["data"]]
    )
  }
  write_db_log(paste("end", comb), 'P')
  #cat("finished widget ", comb, '\n', file = logfile, append = TRUE)
}

write_db_log("finished building html content", "P")

toc_widget <- tags$ul(toc_items)


#cat("assemble layout\n", file =  logfile, append = TRUE)
# Assemble full layout
layout <- tagList(
  tags$head(
    tags$title("Self-contained Report"),
    # INJECTION: CSS is now part of the HTML file itself
    tags$style(HTML(custom_css)) #-temporarily commentend out
  ),
  tags$div(
    tags$body(
      # Sidebar TOC with vw_nav_panel class applied
      tags$div(
        class = "vw_nav_panel",
        style = "position: fixed; top: 60px; left: 0; width: 280px; padding: 10px; background-color: #f9f9f9; border-right: 1px solid #ccc; height: 100%; overflow-y: auto;",
        tags$h3("Table of Contents"),
        toc_widget
      ),
      # Main content area
      tags$div(
        style = "margin-left: 280px; padding: 20px;",
        tags$h1("Controlekaart Rapport"),
        tags$h2("Leeswijzer"),
        tags$p("De blauwe punten worden niet gebruikt..."),
        tags$ul(
          tags$li("Punt telt niet mee: blauwe bol, eval = ."),
          tags$li("Correct punt: groene bol, eval = ------"),
          tags$li("R1: Buiten 3 sigma: rode bol, eval = R1"),
          tags$li("R2a: 2 opeenvolgend buiten 2 sigma: rode bol, eval = R2"),
          tags$li("R3: 9 opeenvolgende zelfde kant: gele bol, eval = R3"),
          tags$li("R4: 6 opeenvolgende trend: gele bol, eval = R4")
        ),
        content_blocks
      )
    )
  )
)

write_db_log("finished assembling html layout", "P")

#cat("start saving as html\n", file = logfile, append = TRUE)
# Combine and save
e <- try({
  output <- htmlwidgets::prependContent(placeholder, layout)
  save_report_widget(output, filename = htmlfile)
})

if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
}

write_db_log("QC charts saved in html", "C")
if (interactive()) {
  shell.exec(htmlfile)
}

