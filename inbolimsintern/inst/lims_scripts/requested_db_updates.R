#Requested DB updates

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "REQUESTED_DB_UPDATES")
writeLines(con = logfile, paste0("REQUESTED_DB_UPDATES\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 2252 #2186 #2181 #2138 #2128 #1647 1982
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

file_name <- params %>% filter(ARG_NAME =="FILE") %>% pull(VALUE)
gebruiker <- params %>% filter(ARG_NAME =="USER") %>% pull(VALUE)

#Lees de csv file in
try({
  dfChanges <- read_csv2(file_name, guess_max = 100000)
} , outFile = logfile)
cat(nrow(dfChanges), ' records\n', file = logfile, append = TRUE)
colnames(dfChanges) <- toupper(colnames(dfChanges))

#Zet de csv om naar  bruikbaar data formaat
dfUpdates <- dfChanges %>%
  rowwise() %>%
  do(db_update_get_data(., conn = conn))

#Maak de queries aan voor de logtabel
queries <- db_update_worklist_table(conn, as.data.frame(dfUpdates))
sapply(queries, dbGetQuery, conn = conn)







