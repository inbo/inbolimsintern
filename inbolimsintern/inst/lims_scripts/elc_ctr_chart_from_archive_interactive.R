
# QC chart to archive

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "CTR_SHOW")
writeLines(con = logfile, paste0("Tonen archiefkaarten\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 5366 5397 8375 8970 9048
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

try({
  kaartlabel <- filter(params, ARG_NAME == "KAARTLABEL") %>% pull(VALUE)
  htmlfile <- filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE)
  htmlrootshort <- substring(htmlfile, max(unlist(gregexpr("\\\\", htmlfile))) + 1, nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
  htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile)))) #including last backslash
}, outFile = logfile)
writeLines(con = logfile, "files (sql, html, htmlroot, path) completed\n------\n")
cat(paste(kaartlabel, htmlfile, htmlrootshort, htmlpath, sep = "\n"), sep = "\n", file = logfile, append = TRUE)

### Haal de data binnen

try({
  sqlcode <- paste0("select * from C_CTR_ARCHIVE where LABEL = '", kaartlabel, "'")
  cat("\n", sqlcode, "\n", file = logfile, append = TRUE)
  plotdata <- DBI::dbGetQuery(conn, sqlcode) %>%
    mutate(EVAL = CHECK_RULES)
  cat(nrow(plotdata),  " rijen\n", file = logfile, append = TRUE)
}, outFile = logfile)

writeLines(con = logfile, "SQL CODE\n")
cat(sqlcode, sep = "\n", file = logfile, append = TRUE)

writeLines(con = logfile, "plotdata\n")
cat(str(plotdata), sep = "\n", file = logfile, append = TRUE)


#Maak de html met de plots

htmlstart <- paste0(
  '<HTML>\n',
  paste0(readLines(file.path(system.file(package = "inbolimsintern"),
                             "resources",
                             "plotly_head.txt")),
         collapse = "\n"),
  "\n<BODY>call_id:", call_id, "\n")

cat(htmlstart, file = htmlfile, append = FALSE)
cat("<H1>Controlekaarten VOOR LABEL", plotdata$LABEL[1] , "</H1>",  "\n", sep = "\n", file = htmlfile, append = TRUE)
combis <- unique(plotdata$COMBI)

writeLines(con = logfile, "detected combinations\n")
cat(paste(combis, sep = "\n"), sep = "\n", file = logfile, append = TRUE)

writeLines(con = logfile, "Combi loop\n")
for (comb in combis) {
  print(comb)
  cat("\n", comb, sep = "\n", file = logfile, append = TRUE)
  figpathshort <- paste0(htmlrootshort, "_", make.names(comb), ".png")
  figpath <- paste0(htmlpath, "\\", figpathshort)
  subdata <- plotdata %>% filter(COMBI == comb)
  cat(str(subdata), sep = "\n", file = logfile, append = TRUE)

  # Add the interactive plot
  p <- ELC_shewhart_plot(subdata, base_color = NULL, interactive = TRUE)
  plot_html <- extract_plotly_html(p)
  cat(plot_html$plot_content, file = htmlfile, append = TRUE)

  #add functionality for table x and s (preliminary°)
  cat(file = htmlfile, append = TRUE,
      knitr::kable(subdata %>% filter(EVAL = TRUE) %>%
                     summarise(CERTIF = max(C_CERTIFIED_VALUE),
                               XBAR = max(C_CTR_X),
                               SD = max(C_CTR_SD),
                               AVG_DATA = round(mean(ENTRY, na.rm = TRUE),3),
                               SD_DATA = round(sd(ENTRY, na.rm = TRUE),3)),
                   format = "html", table.attr = "style='width:40%;'") %>%
        kableExtra::kable_styling(position = "left", bootstrap_options = "bordered"))

  cat("<p></p><p></p>", file = htmlfile, append = TRUE)

  #add table for evaluated points (preliminary)
  cat(file = htmlfile, append = TRUE,
      knitr::kable(subdata %>%
                     filter(EVAL == TRUE) %>%
                     transmute(NR = 1:n(),BATCH, ENTERED_ON, ENTRY = round(ENTRY,4)),
                   format = "html", table.attr = "style='width:40%;'") %>%
        kableExtra::kable_styling(position = "left", bootstrap_options = "bordered"))
}

#Afronden file en html tonen
cat('\n</BODY></HTML>', file = htmlfile, append = TRUE)
shell.exec(htmlfile)
