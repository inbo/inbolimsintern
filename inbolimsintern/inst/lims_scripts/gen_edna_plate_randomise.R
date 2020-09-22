###################################
#### Randomise E-DNA plates #######
###################################

### >>> Configure R session

library(dplyr)
library(inbolimsintern)

#args <- inbolimsintern::prepare_session(call_id = 258)
args   <- inbolimsintern::prepare_session()
conn   <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
params <- inbolimsintern::read_db_arguments(conn, args["call_id"])

### >>> Lees data in

DNA <- DBI::dbReadTable(conn = conn, name = "C_DNA_EXTRACTION")
DNA <-
  DNA %>%
  mutate(REP_SAMPLE_NUMBER = 0,
         MUST_HAVE = TRUE)

### >>> Definieer variabelen

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

### >>> Zet Plaatdata klaar

dfWork <- DNA %>%
  select(sample = "SAMPLE_NUMBER", origsample = "REP_SAMPLE_NUMBER")

### >>> Maak substalen aan

filldata <- get_edna_plate_data(dfWork, rep_fractie = samp_reps / 100)
aantalplaten <- ceiling(nrow(filldata) / (plate_size - n_qcm - n_blank))

### >>> Randomiseer de data op platen

dfPlates <- set_edna_plate_positions(filldata, aantalplaten, plate_size = plate_size, column_size = column_size, qcm_pos = qcm_pos)

dfPlates <- dfPlates %>%
  transmute(DNA_ID = 1:nrow(.),
         DNA_RUN_ID = params %>% filter(ARG_NAME == "DNA_RUN_ID") %>% pull(VALUE) %>% as.numeric(),
         SAMPLE_NUMBER = as.numeric(sample),
         SAMPLE_TYPE = type,
         PLATE_SEQ = Plate,
         PLATE_POSITION = as_character_plate_pos(Position),
         REP_SAMPLE_NUMBER = as.numeric(origsample))

### >>> Schrijf de data weg in de databank

#DNA_ID DNA_RUN_ID SAMPLE_NUMBER SAMPLE_TYPE PLATE_SEQ PLATE_POSITION REP_SAMPLE_NUMBER

DBI::dbGetQuery(conn, "delete from C_DNA_EXTRACTION")
DBI::dbWriteTable(conn, name = "C_DNA_EXTRACTION", value = dfPlates, overwrite = TRUE, append = FALSE)

DBI::dbDisconnect(conn)

######################

#lastpositions <- dfPlateLast %>% pull(Position)

