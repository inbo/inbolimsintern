############################################################################
# PLAATLAYOUT GENEREREN VOR CRYSTAL REPORT
############################################################################

### >>> Configure R session

library(dplyr)
library(tidyr)
library(inbolims)
library(DBI)

sink("D:\\LABO_FS\\log.txt")

#args <- inbolimsintern::prepare_session(call_id = "311")
args <- inbolimsintern::prepare_session()
conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
params <- inbolimsintern::read_db_arguments(conn, args["call_id"])

print(args)
print(conn)
print(params)

sink()


### >>> Data inlezen en brondata klaarzetten

sink("D:\\LABO_FS\\log.txt", append = TRUE)
DNArunID <- filter(params, ARG_NAME == "DNA_RUN_ID") %>% pull(VALUE)
print(DNArunID)
sink()

### >>> Brondata klaarzetten

sink("D:\\LABO_FS\\log.txt", append = TRUE)
dfDesign <- inbolimsintern::gen_plate_read_dna_run(conn, DNArunID)

print(dfDesign)
sink()


### >>> Resultatendataset aanmaken
sink("D:\\LABO_FS\\log.txt", append = TRUE)
dfResult <- inbolimsintern::gen_plate_create_report(dfDesign, Capilar = LETTERS[1:8], Lane = 1:12)

print(dfResult)
sink()


### >>> Invullen Resultatendataset
sink("D:\\LABO_FS\\log.txt", append = TRUE)
DBI::dbGetQuery(conn, "delete from C_DNA_RUN_REPORT_RESULTS")
check <- DBI::dbWriteTable(conn, "C_DNA_RUN_REPORT_RESULTS", dfResult, append = TRUE)
print(check)
write.csv2(file = "platereport.csv", dfResult)
sink()

