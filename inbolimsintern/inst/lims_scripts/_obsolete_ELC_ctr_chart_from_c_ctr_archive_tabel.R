#QC charts from archive

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "ELC_CTR_ARCHIVE")
writeLines(con = logfile, paste0("ELC_CTR_ARCHIVE\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 4208
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

try({
  label  <- try(filter(params, ARG_NAME == "LABEL") %>% pull(VALUE))
  analyse  <- try(filter(params, ARG_NAME == "ANALYSIS") %>% pull(VALUE))
  htmlfile <- try(filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE))
}, outFile = logfile)

## Data

q = paste0("select * from C_CTR_ARCHIVE where LABEL = '", label, "'",
           " and ANALYSIS = '", analyse, "'")
alldata <- dbGetQuery(conn, q)
if (nrow(alldata) == 0) cat("\nGEEN DATA\n", file = logfile, append = TRUE)
combis <- unique(alldata$combi)

##HTML

#paden om figuren te bewaren
htmlrootshort <- substring(htmlfile,
                           max(unlist(gregexpr("\\\\", htmlfile))) + 1,
                           nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile))))

#start HTML
htmlstart <- paste0('<HTML>\n<HEAD> call_id: ', args["call_id"], '</HEAD>\n<BODY>\n')
cat(htmlstart, file = htmlfile, append = FALSE)

#loop door alle gekozen figuren
for (comb in combis) {
  paste0('<H1>', comb, "</H1>")
  figpathshort <- paste0(htmlrootshort, "_", make.names(comb), ".png")
  figpath <- paste0(htmlpath, "\\", figpathshort)
  cat("\n", comb, file = logfile, append = TRUE)

  plotdata <- alldata %>% filter(comb == combi)

  p <- ELC_shewhart_plot(subdata = plotdata)
  ggsave(plot = p, filename = figpath, height = 4.5, width = 6, dpi = 300)

  cat(paste0("\n<IMG SRC = \"", figpathshort, "\">\n"),
      file = htmlfile, append = TRUE)
}

cat('\n</BODY></HTML>', file = htmlfile, append = TRUE)
shell.exec(htmlfile)
