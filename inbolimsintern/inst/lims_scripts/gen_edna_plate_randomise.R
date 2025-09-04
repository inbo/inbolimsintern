###################################
#### Randomise E-DNA plates #######
###################################

#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
library(DBI)
args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#args <- c("LWL8DEV", "10089", "TEST_INT"); setup <- try(r_session_setup(args, test_mode = TRUE))
invisible(list2env(setup, envir = .GlobalEnv))

if (inherits(setup, "try-error")) {
  write_db_log(paste("Problem setting up R script", setup), "E")
  stop("probleem bij setup script:", e)
} else {
  write_db_log("R session setup finished", "P")
}

#// Retrieve and define variables
##===================================
##
e <- try({
  plate_size  <- 96
  min_filled  <- 24
  column_size <- 8
  qc_pos      <- 38

  qcms <- params %>% filter(ARG_NAME == "QC_METHOD") %>% pull(VALUE) %>% strsplit(split = ",") %>% unlist()
  n_qcm <- length(qcms)
  qcm_pos <- seq(from = qc_pos, to = plate_size, length = n_qcm)

  n_blank <- params %>% filter(ARG_NAME == "N_BLANKS") %>% pull(VALUE) %>% as.numeric()
  samp_reps <- params %>% filter(ARG_NAME == "REPS") %>% pull(VALUE) %>% as.numeric()
  max_reps_per_sample <-  params %>% filter(ARG_NAME == "MAX_REPS") %>% pull(VALUE) %>% as.numeric()
})
if (inherits(e, "try-error")) {
  write_db_log(paste("Problems getting parameters:", e), "E")
  stop(e)
} else {
  write_db_log("Parameters retrieved", "P")
}

### >>> Lees data in
e <- try({
  DNA <- DBI::dbReadTable(conn = conn, name = "C_DNA_EXTRACTION")
  DNA <-
    DNA %>%
    mutate(REP_SAMPLE_NUMBER = 0,
           MUST_HAVE = TRUE)
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

### >>> Zet Plaatdata klaar
e <- try({
  dfWork <- DNA %>%
    select(sample = "SAMPLE_NUMBER", origsample = "REP_SAMPLE_NUMBER")
})
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
}

### >>> Maak substalen aan
e <- try({
  filldata <- get_edna_plate_data(dfWork, rep_fractie = samp_reps / 100)
  aantalplaten <- ceiling(nrow(filldata) / (plate_size - n_qcm - n_blank))
})
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
} else {
  write_db_log(paste("Data met substalen:", nrow(filldata), "records"), "P")
}

### >>> Randomiseer de data op platen
e <- try({
  dfPlates <- set_edna_plate_positions(data = filldata,
                                       n_plates = aantalplaten,
                                       plate_size = plate_size,
                                       column_size = column_size,
                                       qcm_pos = qcm_pos)
  dfPlates <- dfPlates %>%
    transmute(DNA_ID = 1:nrow(.),
              DNA_RUN_ID = params %>% filter(ARG_NAME == "DNA_RUN_ID") %>% pull(VALUE) %>% as.numeric(),
              SAMPLE_NUMBER = as.numeric(sample),
              SAMPLE_TYPE = type,
              PLATE_SEQ = Plate,
              PLATE_POSITION = as_character_plate_pos(Position),
              REP_SAMPLE_NUMBER = as.numeric(origsample))
})
if (inherits(e, "try-error")) {
  write_db_log(paste("Probleem bij randomiseren:", e), "E")
  stop(e)
} else {
  write_db_log(paste("Data gerandomiseerd:", nrow(dfPlates), "records"), "P")
}

### >>> Schrijf de data weg in de databank
#velden: DNA_ID DNA_RUN_ID SAMPLE_NUMBER SAMPLE_TYPE PLATE_SEQ PLATE_POSITION REP_SAMPLE_NUMBER

e <- try({
  DBI::dbGetQuery(conn, "delete from C_DNA_EXTRACTION")
  DBI::dbWriteTable(conn, name = "C_DNA_EXTRACTION", value = dfPlates, overwrite = TRUE, append = FALSE)
})
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
}
write_db_log("Rscript afgerond", "C")
DBI::dbDisconnect(conn)


