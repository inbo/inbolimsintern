#Requested DB updates
#
#voorbeeldcsv (semicolon-delimited)
#LabSampleID;FieldSampleID;FieldSamplingDate;veld_te_wijzigen;LIMSAnalysisName;originele_waarde;nieuwe_waarde;opmerking_MilKlim;partim
#24-007337;K12;7/04/2024 0:00;FieldSampleID;NA;K12;KESR009X;FieldSampleID aanpassen;sample
#24-007344;Z6;7/04/2024 0:00;FieldSampleID;NA;Z6;KESR005X;FieldSampleID aanpassen;sample


#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)

args <- commandArgs(trailingOnly = TRUE)
setup <- try(r_session_setup(args))
#args <- c("LWL8DEV", "0000", "TEST_INT"); setup <- try(r_session_setup(args, test_mode = TRUE))
if (inherits(setup, "try-error")) {
  stop("probleem bij setup script")
}
invisible(list2env(setup, envir = .GlobalEnv))


#//Read parameters
##================

file_name <- params %>% filter(ARG_NAME =="FILE") %>% pull(VALUE)
gebruiker <- params %>% filter(ARG_NAME =="USER") %>% pull(VALUE)

#import file
e <- try({
  dfChanges <- read_csv2(file_name, guess_max = 100000)
} , outFile = logfile)
if (inherits(e, "try-error")) {
  write_db_log(paste("csv file could not be read:", e), "E")
} else {
  write_db_log(paste("csv read:", nrow(dfChanges), " records"), "P")
}
colnames(dfChanges) <- toupper(colnames(dfChanges))

#Zet de csv om naar  bruikbaar data formaat
e <- try({
  dfUpdates <- dfChanges %>%
  rowwise() %>%
  do(db_update_get_data(., conn = conn))
})
if (inherits(e, "try-error")) {
  write_db_log(paste("Error converting data:", e), "E")
} else {
  write_db_log(paste("updates to be processed:", nrow(dfUpdates)), "C")
}

#// Maak de queries aan in de lims update tabel
##================================================

queries <- db_update_worklist_table(conn, as.data.frame(dfUpdates))
e <- try(sapply(queries, DBI::dbGetQuery, conn = conn))
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
} else{
  write_db_log("updates added to the update table", "C")
}








