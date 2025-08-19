############################################################################
# PLAATLAYOUT GENEREREN VOR CRYSTAL REPORT
############################################################################

### >>> Configure R session

library(dplyr)
library(tidyr)
library(inbolimsintern)
library(DBI)

logfile <- logfile_start(prefix = "PLATELAYOUT")
call_id <- 0 #630 505 515 516 518 8109

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

### >>> Data inlezen en brondata klaarzetten

try({
  DNArunID <- filter(params, ARG_NAME == "DNA_RUN_ID") %>% pull(VALUE)
}, outFile = logfile)

### >>> Brondata klaarzetten

try({
  dfDesign <- inbolimsintern::plate_read_dna_run(conn, DNArunID)
}, outFile = logfile)


### >>> Resultatendataset aanmaken
try({
  dfResult <- inbolimsintern::plate_create_report(dfDesign, Capilar = LETTERS[1:8], Lane = 1:12)
}, outFile = logfile)

### >>> Invullen Resultatendataset
try({
  DBI::dbGetQuery(conn, "delete from C_DNA_RUN_REPORT_RESULTS")
  check <- DBI::dbWriteTable(conn, "C_DNA_RUN_REPORT_RESULTS", dfResult, append = TRUE)
  print(check)
  write.csv2(file = "platereport.csv", dfResult)
}, outFile = logfile)

close(logfile)

