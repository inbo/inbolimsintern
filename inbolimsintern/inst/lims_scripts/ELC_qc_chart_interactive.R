#ELC - Make QC chart html report

### R libraries

library(inbolimsintern)
library(DBI)
library(tidyverse)
library(plotly)
library(DT)
library(htmltools)
library(htmlwidgets)

### Init Logfile


### Read LIMS arguments

call_id <- 0 #call_id <- 10016
logfile <- logfile_start(prefix = "ELC_Shewhart")
writeLines(con = logfile, paste0("ELC_Shewhart\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "\n\nparams:\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

try({
  maxpoints_orig <- 30 #indien max_points bestaat wordt dit overschreven door die waarde
  sqlfile  <- try(filter(params, ARG_NAME == "SQL_FILE") %>% pull(VALUE))
  htmlfile <- try(filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE))
  maxpoints <- try(filter(params, ARG_NAME == "MAX_POINTS") %>% pull(VALUE) %>% as.integer())
  if (inherits(maxpoints, "try-error") | !length(maxpoints)) maxpoints <- maxpoints_orig
  # archive_label <- try(filter(params, ARG_NAME == "ARCHIVE_LABEL") %>% pull(VALUE))
  #  if (class(archive_label == 'try-error')) {
  #    archive_label <- NULL
  #  }
}, outFile = logfile)

htmlrootshort <- substring(htmlfile,
                           max(unlist(gregexpr("\\\\", htmlfile))) + 1,
                           nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile))))

writeLines(con = logfile, "\nhtml:\n")
cat(paste(htmlrootshort, htmlpath, sep = "\n"), sep = "\n", file = logfile, append = TRUE)

### Data import

alldata <- get_ELC_data(conn, sqlfile, keep = maxpoints, logfile = logfile)
if (nrow(alldata) == 0) cat("\nGEEN DATA\n", file = logfile, append = TRUE)

writeLines(con = logfile, "\ncombis\n------\n")
cat(unique(alldata$combi), sep = "\n", file = logfile, append = TRUE)

combis <- data.frame(combi = unique(alldata$combi))
combis <- cbind(combis, separate(combis,
                                 col = "combi",
                                 into = c("ana", "qc", "comp"),
                                 sep = "---")) %>%
  arrange(ana, comp, qc)

writeLines(con = logfile, "\ncombis after elimination mu\n------\n")
cat(combis$combi, sep = "\n", file = logfile, append = TRUE)


###############################################################################
### CREATE WIDGETS
###############################################################################
plot_widgets <- list()
for (i in 1:nrow(combis)) {
  pltly <-  plotdata <- htmldata <- NULL
  comb <- combis$combi[i]
  print(comb)
  subtitle = paste("\n",paste0("analyse:   ", combis$ana[i]),
                paste0("qc sample: ", combis$qc[i]),
                paste0("component: ",combis$comp[i]),
                sep = " ")
  plotdata <- alldata %>% filter(combi == comb)
  cat("\nrijen plotdata: " , nrow(plotdata),file = logfile, append = TRUE)
  htmldata <- elc_htmldata(plotdata)
  cat("\nrijen htmldata: ", nrow(htmldata), file = logfile, append = TRUE)

  pltly <- ELC_shewhart_plot(subdata = htmldata[["plot"]],
                         interactive = TRUE,
                         title = subtitle)
  plot_widgets[[comb]][["fig"]] <- pltly
  plot_widgets[[comb]][["smry"]] <- datatable(htmldata[['summary']])
  plot_widgets[[comb]][["data"]] <- datatable(htmldata[['tabel']])
  plot_widgets[[comb]][["out3s"]] <- datatable(htmldata[['out3s']])
}

###############################################################################
### CREATE HTML
###############################################################################

# Create placeholder widget
placeholder <- htmlwidgets::createWidget("html", list(), package = "htmlwidgets")

# Initialize lists to hold TOC and content
toc_items <- list()
content_blocks <- list()

for (comb in names(plot_widgets)) {
  analysis <- sub("---.*", "", comb)
  qc       <- sub(".*?---(.*?)---.*", "\\1", comb)
  comp     <- sub(".*---", "", comb)
  section_id <- gsub("[^a-zA-Z0-9]", "_", comb)  # Safe ID for anchor tags

  # Add to TOC
  toc_items[[length(toc_items) + 1]] <- tags$li(
    tags$a(href = paste0("#", section_id), paste(comp, "-", qc))
  )

  # Add content section with outliesrs
  if (!is.null(plot_widgets[[comb]][["out3s"]])) {
    content_blocks[[length(content_blocks) + 1]] <- tags$div(
      id = section_id,
      tags$h2(paste0("Component: ", comp)),
      tags$h3(paste0("QC: ", qc)),
      plot_widgets[[comb]][["fig"]],
      tags$h4("Samenvattende gegevens"),
      plot_widgets[[comb]][["smry"]],
      tags$h4("Bijhorende tabel"),
      plot_widgets[[comb]][["data"]],
      plot_widgets[[comb]][["out3s"]]
    )
  } else {
    content_blocks[[length(content_blocks) + 1]] <- tags$div(
      id = section_id,
      tags$h2(paste0("Component: ", comp)),
      tags$h3(paste0("QC: ", qc)),
      plot_widgets[[comb]][["fig"]],
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

# Combine and save
output <- htmlwidgets::prependContent(placeholder, layout)
htmlwidgets::saveWidget(output, htmlfile, selfcontained = TRUE)


### html tonena
shell.exec(htmlfile)







