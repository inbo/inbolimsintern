
##################################
#Show existing QC period charts
##################################

#call from labware: \\Inbo-limsbg-prd-labware8.inbo.be\LWL8PRD_Data\R_SCRIPTS\QA_shewhart_period_show.R LWL8PRD 12288 PIETERVS

#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
library(htmltools)
fig_height <- 600

args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#args <- c("LWL8DEV", "10023", "TEST_INT"); setup <- try(r_session_setup(args, test_mode = TRUE))
invisible(list2env(setup, envir = .GlobalEnv))
if (inherits(setup, "try-error")) {
  write_db_log(paste("Problem setting up R script", setup), "E")
  stop("probleem bij setup script")
} else {
  write_db_log("R session setup finished", "P")
}

#// retrieve arguments
##=====================

e <- try({
  kaartlabel <- filter(params, ARG_NAME == "KAARTLABEL") %>% pull(VALUE)
  htmlfile <- filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE)
  htmlrootshort <- substring(htmlfile, max(unlist(gregexpr("\\\\", htmlfile))) + 1, nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
  htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile)))) #including last backslash
})
if (inherits(e, "try-error")){
  write_db_log(paste("argumenten niet gelezen", e), "E")
  stop(e)
} else {
  write_db_log("argumenten ingelezen", "P")
}


#// Read data
##=================

message(kaartlabel)
#cat(kaartlabel, file = "d:\\pieter\\log.log", append = TRUE)
e <- try({
  sqlcode <- paste0("select * from C_CTR_ARCHIVE where LABEL = '", kaartlabel, "'")
  dbdata <- DBI::dbGetQuery(conn, sqlcode) %>%
    mutate(EVAL = CHECK_RULES,
           ORDER_NUMBER = row_number())
})

if (inherits(e, "try-error")) {
  write_db_log(paste("probleem inlezen data", e), "E")
} else {
  write_db_log(paste("Aantal records", nrow(dbdata)), "P")
}

alldata <- dbdata %>%
  rename_with(toupper) %>%
  rename(PRODUCT_VERSION = LIMIT_VERSION)

combis <- alldata %>%
  dplyr::select(COMBI) %>%
  dplyr::distinct()

message(paste(combis %>% dplyr::pull(COMBI), collapse = "\n"))
combis <- combis %>%
  bind_cols(tidyr::separate(combis,
                            col = "COMBI",
                            into = c("ana", "qc", "comp"),
                            sep = "---")) %>%
  arrange(ana, comp, qc)


#// Nieuwe methode met htmlwidgets zoals andere kaarten
##======================================================

plot_widgets <- list()
write_db_log("creating widgets", "P")

for (i in 1:nrow(combis)) {
  #prepare data
  pltly <-  plotdata <- htmldata <- NULL
  comb <- combis$COMBI[i]
  subtitle = paste("\n",paste0("analyse:   ", combis$ana[i]),
                   paste0("qc sample: ", combis$qc[i]),
                   paste0("component: ",combis$comp[i]),
                   sep = " ")

  plotdata <- alldata %>% dplyr::filter(COMBI == comb)
  htmldata <- elc_htmldata(plotdata, check_rules = FALSE)
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

write_db_log("content blocks created", "P")

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

