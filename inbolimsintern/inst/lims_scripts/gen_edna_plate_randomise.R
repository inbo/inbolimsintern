###################################
#### Randomise E-DNA plates #######
###################################

### >>> Configure R session

library(dplyr)
library(inbolimsintern)

logfile <- logfile_start(prefix = "EDNA_RUN")
call_id <- 0 #258

try({
  args   <- inbolimsintern::prepare_session(call_id)
  conn   <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)


### >>> Lees data in
try({
  DNA <- DBI::dbReadTable(conn = conn, name = "C_DNA_EXTRACTION")
  DNA <-
    DNA %>%
    mutate(REP_SAMPLE_NUMBER = 0,
           MUST_HAVE = TRUE)
}, outFile = logfile)


### >>> Definieer variabelen
try({
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
}, outFile = logfile)


### >>> Zet Plaatdata klaar
try({
  dfWork <- DNA %>%
    select(sample = "SAMPLE_NUMBER", origsample = "REP_SAMPLE_NUMBER")
}, outFile = logfile)


### >>> Maak substalen aan
try({
  filldata <- get_edna_plate_data(dfWork, rep_fractie = samp_reps / 100)
  aantalplaten <- ceiling(nrow(filldata) / (plate_size - n_qcm - n_blank))



### >>> Randomiseer de data op platen
try({
  dfPlates <- set_edna_plate_positions(filldata, aantalplaten, plate_size = plate_size,
                                       column_size = column_size, qcm_pos = qcm_pos)
  dfPlates <- dfPlates %>%
    transmute(DNA_ID = 1:nrow(.),
              DNA_RUN_ID = params %>% filter(ARG_NAME == "DNA_RUN_ID") %>% pull(VALUE) %>% as.numeric(),
              SAMPLE_NUMBER = as.numeric(sample),
              SAMPLE_TYPE = type,
              PLATE_SEQ = Plate,
              PLATE_POSITION = as_character_plate_pos(Position),
              REP_SAMPLE_NUMBER = as.numeric(origsample))
}, outFile = logfile)

### >>> Schrijf de data weg in de databank
#velden: DNA_ID DNA_RUN_ID SAMPLE_NUMBER SAMPLE_TYPE PLATE_SEQ PLATE_POSITION REP_SAMPLE_NUMBER

try({
  DBI::dbGetQuery(conn, "delete from C_DNA_EXTRACTION")
  DBI::dbWriteTable(conn, name = "C_DNA_EXTRACTION", value = dfPlates, overwrite = TRUE, append = FALSE)
  DBI::dbDisconnect(conn)
}, outFile = logfile)

close(logfile)

