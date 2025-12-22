##################################
#ELC - Make QC chart html report
##################################

#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
library(htmltools)
fig_height <- 600

args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#args <- c("LWL8DEV", "9986", "TEST_INT"); setup <- try(r_session_setup(args, test_mode = TRUE))
#args <- c("LWL8UAT", "10430", "TEST_2");setup <- try(r_session_setup(args, test_mode = TRUE))
if (inherits(setup, "try-error")) {
  stop("probleem bij setup script")
}
invisible(list2env(setup, envir = .GlobalEnv))


#// retrieve arguments
##=====================

e <- try({
  maxpoints_orig <- 30 #indien max_points bestaat wordt dit overschreven door die waarde
  sqlfile  <- try(dplyr::filter(params, ARG_NAME == "SQL_FILE") %>% pull(VALUE))
  htmlfile <- try(dplyr::filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE))
  maxpoints <- try(dplyr::filter(params, ARG_NAME == "MAX_POINTS") %>% pull(VALUE) %>% unique() %>%  as.integer())
  if (inherits(maxpoints, "try-error") | !length(maxpoints)) {
    maxpoints <- maxpoints_orig
  }
  if (length(maxpoints) > 1) {
    maxpoints <- max(maxpoints)
  }
})
print(maxpoints)
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
}

htmlrootshort <- substring(htmlfile,
                           max(unlist(gregexpr("\\\\", htmlfile))) + 1,
                           nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile))))


#// Import data
##================

write_db_log("start import data from db", "P")
e <-
  try(
    alldata <- get_ELC_data(conn, sqlfile, keep = maxpoints)
  )
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
}

combis <- alldata %>%
  dplyr::select(combi) %>%
  dplyr::distinct()

message(paste(combis %>% dplyr::pull(combi), collapse = "\n"))
combis <- combis %>%
  bind_cols(tidyr::separate(combis,
                     col = "combi",
                     into = c("ana", "qc", "comp"),
                     sep = "---")) %>%
  arrange(ana, comp, qc)

#// CREATE WIDGETS
#==================

plot_widgets <- list()
write_db_log("creating widgets", "P")

for (i in 1:nrow(combis)) {
  #prepare data
  pltly <-  plotdata <- htmldata <- NULL
  comb <- combis$combi[i]
  subtitle = paste("\n",paste0("analyse:   ", combis$ana[i]),
                paste0("qc sample: ", combis$qc[i]),
                paste0("component: ",combis$comp[i]),
                sep = " ")
  plotdata <- alldata %>% dplyr::filter(combi == comb)
  htmldata <- elc_htmldata(plotdata)
  message(comb, ": ", "records: " , nrow(plotdata))

  #create plot
  pltly <- ELC_shewhart_plot(subdata = htmldata[["plot"]],
                         interactive = TRUE,
                         title = subtitle,
                         fig_height = fig_height)
  plot_widgets[[comb]][["fig"]] <- pltly

  #create tables
  plot_widgets[[comb]][["smry"]] <- DT::datatable(htmldata[['summary']])
  plot_widgets[[comb]][["data"]] <- DT::datatable(htmldata[['tabel']])
  plot_widgets[[comb]][["out3s"]] <- DT::datatable(htmldata[['out3s']])
}
write_db_log("widgets created, start creating html", "P")


#// CREATE HTML
#=================

# Create placeholder widget
placeholder <- htmlwidgets::createWidget("html", list(), package = "htmlwidgets")

# Initialize lists to hold TOC and content
toc_items <- list()
content_blocks <- list()

# loop through widgets and add them to content blocks
for (comb in names(plot_widgets)) {
  analysis <- sub("---.*", "", comb)
  qc       <- sub(".*?---(.*?)---.*", "\\1", comb)
  comp     <- sub(".*---", "", comb)
  section_id <- gsub("[^a-zA-Z0-9]", "_", comb)  # Safe ID for anchor tags

  # Add to TOC
  toc_items[[length(toc_items) + 1]] <- tags$li(
    tags$a(href = paste0("#", section_id), paste(comp, "-", qc))
  )

  # add content blocks
    #case: with outlier block
  if (nrow(plot_widgets[[comb]][["out3s"]]$x$data) > 0) {
    content_blocks[[length(content_blocks) + 1]] <- tags$div(
      id = section_id,
      tags$h2(paste0("Component: ", comp)),
      tags$h3(paste0("QC: ", qc)),
      tags$div(
        style = paste0("height: ",fig_height, "px;"),
        plot_widgets[[comb]][["fig"]]
      ),
      tags$h4("Samenvattende gegevens"),
      plot_widgets[[comb]][["smry"]],
      tags$h4("Bijhorende tabel"),
      plot_widgets[[comb]][["data"]],
      tags$h4("Buiten 3s limieten"),
      plot_widgets[[comb]][["out3s"]]
    )
    #case: without outlier block
  } else { #content section without outliers
    content_blocks[[length(content_blocks) + 1]] <- tags$div(
      id = section_id,
      tags$h2(paste0("Component: ", comp)),
      tags$h3(paste0("QC: ", qc)),
      tags$div(
        style = paste0("height: ",fig_height, "px;"),
        plot_widgets[[comb]][["fig"]]
      ),
      tags$h4("Samenvattende gegevens"),
      plot_widgets[[comb]][["smry"]],
      tags$h4("Bijhorende tabel"),
      plot_widgets[[comb]][["data"]]
    )
  }
}

# Wrap TOC
toc_widget <- tags$ul(toc_items)

# Assemble full layout
layout <- tagList(
  tags$div(
    tags$head(tags$title("Self-contained Report")),
    tags$body(
      # Sidebar TOC
      tags$div(
        style = "position: fixed; top: 60px; left: 0; width: 280px; padding: 10px; background-color: #f9f9f9; border-right: 1px solid #ccc; height: 100%; overflow-y: auto;",
        tags$h3("Table of Contents"),
        toc_widget
      ),
      # Main content area
      tags$div(
        style = "margin-left: 280px; padding: 20px;",
        tags$h1("Controlekaart Rapport"),
        tags$h2("Leeswijzer"),
        tags$p(paste0("De blauwe punten worden niet gebruikt bij de berekeningen.",
                      " Hoe donkderder de blauwe bol, hoe eerder in de batch die voorkomt.",
                      " Overtredingen van regels worden in de figuur en tabel aangeduid.")),
        tags$ul(
          tags$li("Punt telt niet mee: blauwe bol, eval = ."),
          tags$li("Correct punt: groene bol, eval = ------"),
          tags$li("R1: Buiten 3 sigma: rode bol, eval = R1"),
          tags$li("R2a: 2 opeenvolgend buiten 2 sigma, zelfde kant: rode bol, eval = R2"),
          tags$li("R3: 9 opeenvolgende buiten aan zelfde kant gemiddelde: gele bol, eval = R3"),
          tags$li("R4: 6 opeenvolgende met toenemede of dalende trend: gele bol, eval = R4")
        ),
        content_blocks  # dynamically generated sections
      )
    )
  )
)
write_db_log("widgets saved in content blocks", "P")

# Combine and save
e <- try(output <- htmlwidgets::prependContent(placeholder, layout))
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
}

e <- try(save_report_widget(output, filename = htmlfile))
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
}

write_db_log("QC charts saved in html", "C")

### html tonen
shell.exec(htmlfile)







