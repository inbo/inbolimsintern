#Requested DB updates
#
#voorbeeldcsv (semicolon-delimited)
#LabSampleID;FieldSampleID;FieldSamplingDate;veld_te_wijzigen;LIMSAnalysisName;originele_waarde;nieuwe_waarde;opmerking_MilKlim;partim
#24-007337;K12;7/04/2024 0:00;FieldSampleID;NA;K12;KESR009X;FieldSampleID aanpassen;sample
#24-007344;Z6;7/04/2024 0:00;FieldSampleID;NA;Z6;KESR005X;FieldSampleID aanpassen;sample

#
#
#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
library(htmltools) #must be loaded for tags functionality
fig_height <- 600

args <- commandArgs(trailingOnly = TRUE)
#args <- c("LWL8DEV", "", "TEST_INT")

username <- args[3]
call_id <- args[2]
odbc <- args[1]

message("call_id = ", call_id)
setup <- try(r_session_setup(call_id, odbc))
if (inherits(setup, "try-error")) {
  stop("probleem bij setup script")
}
invisible(list2env(setup, envir = .GlobalEnv))
message("session prepared")

status <- inbolimsintern::read_db_log(conn, call_id)
write_db_log(conn, call_id, "P", "Started db update requests", user = username)


file_name <- params %>% filter(ARG_NAME =="FILE") %>% pull(VALUE)
gebruiker <- params %>% filter(ARG_NAME =="USER") %>% pull(VALUE)


#//Read csv file

e <- try({
  dfChanges <- read_csv2(file_name, guess_max = 100000)
} , outFile = logfile)
if (inherits(e, "try-error")) {
  write_db_log(conn, call_id, "E", paste("csv file could not be read:", e), user = username)
} else {
  write_db_log(conn, call_id, "P", paste("csv read:", nrow(dfChanges), " records"), user = username)
}
colnames(dfChanges) <- toupper(colnames(dfChanges))

#Zet de csv om naar  bruikbaar data formaat
e <- try({
  dfUpdates <- dfChanges %>%
  rowwise() %>%
  do(db_update_get_data(., conn = conn))
})
if (inherits(e, "try-error")) {
  write_db_log(conn, call_id, "E", paste("Error converting data:", e), user = username)
} else {
  write_db_log(conn, call_id, "P", paste("updates to be processed:", nrow(dfUpdates)), user = username)
}

#Maak de queries aan voor de logtabel
queries <- db_update_worklist_table(conn, as.data.frame(dfUpdates))
e <- try(sapply(queries, DBI::dbGetQuery, conn = conn))
if (inherits(e, "try-error")) {
  write_db_log(conn, call_id, "E", e, user = username)
} else{
  write_db_log(conn, call_id, "C", 'updates added to the update table', user = username)
}








