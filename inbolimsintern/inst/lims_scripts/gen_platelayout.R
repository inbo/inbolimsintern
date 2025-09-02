############################################################################
# PLAATLAYOUT GENEREREN VOR CRYSTAL REPORT
############################################################################


#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
library(DBI)
args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#args <- c("LWL8DEV", "10083", "TEST_INT"); setup <- try(r_session_setup(args, test_mode = TRUE))
invisible(list2env(setup, envir = .GlobalEnv))

if (inherits(setup, "try-error")) {
  write_db_log(paste("Problem setting up R script", setup), "E")
  stop("probleem bij setup script:", e)
} else {
  write_db_log("R session setup finished", "P")
}

### >>> Data inlezen en brondata klaarzetten

e <- try({
  DNArunID <- dplyr::filter(params, ARG_NAME == "DNA_RUN_ID") %>% pull(VALUE)
})
if (inherits(e, 'try-error')) {
  write_db_log(paste("Probleem bij inlezen DNA run ID", e ), "E")
  stop(e)
}

e <- try({
  dfDesign <- inbolimsintern::plate_read_dna_run(conn, DNArunID)
})
if (inherits(e, 'try-error')) {
  write_db_log(paste("Probleem bij aanmaken dataset: ", e ), "E")
  stop(e)
} else {
  if (nrow(dfDesign) == 0) {
    write_db_log(paste("Dataset ingelezen: geen records gevonden"), "E")
    stop("Geen data gevonden")
  } else {
    write_db_log(paste("Dataset ingelezen:", nrow(dfDesign), "records" ), "P")
  }
}

### >>> Resultatendataset aanmaken
e <- try({
  dfResult <- inbolimsintern::plate_create_report(dfDesign, Capilar = LETTERS[1:8], Lane = 1:12)
})
if (inherits(e, 'try-error')) {
  write_db_log(paste("Probleem bij aanmaken resultatendataset: ", e ), "E")
  stop(e)
} else {
  if (nrow(dfResult) == 0) {
    write_db_log(paste("Geen resultaten kunnen aanmaken"), "E")
    stop("Geen resultaten kunnen bouwen")
  } else {
    write_db_log(paste("Resultatendata aangemaakt:", nrow(dfDesign), "records" ), "P")
  }
}

### >>> Invullen Resultatendataset
e <- try({
  DBI::dbGetQuery(conn, "delete from C_DNA_RUN_REPORT_RESULTS")
  check <- DBI::dbWriteTable(conn, "C_DNA_RUN_REPORT_RESULTS", dfResult, append = TRUE)
  write.csv2(file = paste0("platereport_run_", DNArunID, "_callid_",call_id,".csv"), dfResult)
})
if (inherits(e, 'try-error')) {
  write_db_log(paste("Probleem bij invullen resultatendataset in de database: ", e ), "E")
  stop(e)
}
write_db_log("R routine succesvol afgerond", "C")

