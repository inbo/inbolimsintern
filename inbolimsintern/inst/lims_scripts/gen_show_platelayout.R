############################################################################
# PLAATLAYOUT GENEREREN VOR CRYSTAL REPORT
############################################################################

### >>> Configure R session

library(dplyr)
library(tidyr)
library(inbolimsintern)
library(DBI)

logfile <- try(logfile <- "D:\\LABO_FS\\LIMS\\LOGS\\platelayout_log.txt")
try(sink(logfile))

#args <- inbolimsintern::prepare_session(call_id = "630") #505 515 516 518
args <- inbolimsintern::prepare_session()
conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
print(params)
sink()


### >>> Data inlezen en brondata klaarzetten

try(sink(logfile, append = TRUE))
DNArunID <- filter(params, ARG_NAME == "DNA_RUN_ID") %>% pull(VALUE)
print(DNArunID)
sink()

### >>> Brondata klaarzetten

try(sink(logfile, append = TRUE))
dfDesign <- inbolimsintern::plate_read_dna_run(conn, DNArunID)

print(dfDesign)
sink()


### >>> Resultatendataset aanmaken
try(sink(logfile, append = TRUE))
dfResult <- inbolimsintern::plate_create_report(dfDesign, Capilar = LETTERS[1:8], Lane = 1:12)

print(dfResult)
sink()


### >>> Invullen Resultatendataset
try(sink(logfile, append = TRUE))
DBI::dbGetQuery(conn, "delete from C_DNA_RUN_REPORT_RESULTS")
check <- DBI::dbWriteTable(conn, "C_DNA_RUN_REPORT_RESULTS", dfResult, append = TRUE)
print(check)
write.csv2(file = "platereport.csv", dfResult)
sink()

