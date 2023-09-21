
# QC chart to archive

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "CTR_SHOW")
writeLines(con = logfile, paste0("Tonen archiefkaarten\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 5366 5397
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

try({
  sqlfile <- filter(params, ARG_NAME == "SQL_FILE") %>% pull(VALUE)
  htmlfile <- filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE)
  htmlrootshort <- substring(htmlfile, max(unlist(gregexpr("\\\\", htmlfile))) + 1, nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
  htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile)))) #including last backslash
}, outFile = logfile)

### Haal de data binnen

try({
sqlcode <- paste(readLines(sqlfile), collapse = '\n')
plotdata <- DBI::dbGetQuery(conn, sqlcode) %>%
  mutate(EVAL = CHECK_RULES)
cat(nrow(plotdata),  " rijen\n", file = logfile, append = TRUE)
}, outFile = logfile)

#Maak de html met de plots


htmlstart <- paste0('<HTML>\n<HEAD> call_id: ', args["call_id"], '</HEAD>\n<BODY>\n')
cat(htmlstart, file = htmlfile, append = FALSE)
cat("<H1>Controlekaarten VOOR LABEL", plotdata$LABEL[1] , "</H1>",  "\n", sep = "\n", file = htmlfile, append = TRUE)
combis <- unique(plotdata$COMBI)


for (comb in combis) {
  print(comb)
  figpathshort <- paste0(htmlrootshort, "_", make.names(comb), ".png")
  figpath <- paste0(htmlpath, "\\", figpathshort)
  cat("\n", comb, file = logfile, append = TRUE)
  subdata <- plotdata %>% filter(COMBI == comb)

  p <- ELC_shewhart_plot(subdata, base_color = NULL)
  ggsave(plot = p, filename = figpath, height = 4.5, width = 6, dpi = 300)
  cat(paste0("\n<H2>", comb, "</H2>\n"), file = htmlfile, append = TRUE)
  cat(paste0("\n<IMG SRC = \"", figpathshort, "\">\n"), file = htmlfile, append = TRUE)


}

#Afronden file en html tonen
cat('\n</BODY></HTML>', file = htmlfile, append = TRUE)
shell.exec(htmlfile)
