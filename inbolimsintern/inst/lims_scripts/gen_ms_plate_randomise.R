###################################################
###########   Randomise Microsat Plates ###########
###################################################

### >>> Configure R session

library(dplyr)
library(inbolimsintern)

logfile <- logfile_start(prefix = "MICROSATS_RUN")
call_id <- 0 #40

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
},outFile = logfile)

### >>> Lees data in

try({
  DNA <- DBI::dbReadTable(conn = conn, name = "C_DNA_EXTRACTION")
},outFile = logfile)

### >>> Genereer de plaatdata

try({
  dfPlates <- inbolimsintern::gen_ms_create_plates(DNA)
},outFile = logfile)

### >>> Schrijf de data weg in de databank

try({
  DBI::dbGetQuery(conn, "delete from C_DNA_EXTRACTION")
  DBI::dbWriteTable(conn, name = "C_DNA_EXTRACTION", value = dfPlates, overwrite = TRUE, append = FALSE)
  DBI::dbDisconnect(conn)
},outFile = logfile)

close(logfile)

