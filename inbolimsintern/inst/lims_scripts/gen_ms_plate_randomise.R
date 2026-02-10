###################################################
###########   Randomise Microsat Plates ###########
###################################################

#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
library(DBI)
args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#args <- c("LWLPRD", "11288", "PIETERVS"); setup <- try(r_session_setup(args))
invisible(list2env(setup, envir = .GlobalEnv))

if (inherits(setup, "try-error")) {
  write_db_log(paste("Problem setting up R script", setup), "E")
  stop("probleem bij setup script:", e)
} else {
  write_db_log("R session setup finished", "P")
}

#// Lees data in
##=================

e <- try({
  DNA <- DBI::dbReadTable(conn = conn, name = "C_DNA_EXTRACTION")
})
if (inherits(e, 'try-error')) {
  write_db_log(paste("Probleem bij aanmaken dataset: ", e ), "E")
  stop(e)
} else {
  if (nrow(DNA) == 0) {
    write_db_log(paste("Dataset ingelezen: geen records gevonden"), "E")
    stop("Geen data gevonden")
  } else {
    write_db_log(paste("Dataset ingelezen:", nrow(DNA), "records" ), "P")
  }
}

### >>> Genereer de plaatdata

e <- try({
  dfPlates <- inbolimsintern::gen_ms_create_plates(DNA)
})
if (inherits(e, 'try-error')) {
  write_db_log(paste("Probleem bij genereren plaatdata: ", e ), "E")
  stop(e)
} else {
  if (nrow(dfPlates) == 0) {
    write_db_log(paste("Plaatdata gegenereerd: geen records"), "E")
    stop("Geen records in plaatdata")
  } else {
    write_db_log(paste("plaatdata gegenereerd:", nrow(dfPlates), "records" ), "P")
  }
}

### >>> Schrijf de data weg in de databank

e <- try({
  DBI::dbGetQuery(conn, "delete from C_DNA_EXTRACTION")
  DBI::dbWriteTable(conn, name = "C_DNA_EXTRACTION", value = dfPlates, overwrite = TRUE, append = FALSE)
})
if (inherits(e, "try-error")) {
  write_db_log(paste("Fout bij wegschrijven data in databank", e), "E")
  stop(e)
}
write_db_log("R Routine afgerond", "C")
DBI::dbDisconnect(conn)
