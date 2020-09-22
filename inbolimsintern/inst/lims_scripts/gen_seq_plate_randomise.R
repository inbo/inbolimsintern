###################################################
###########   Randomise Sequencing Plates #########

### >>> Configure R session

library(dplyr)
library(inbolimsintern)

#args <- inbolimsintern::prepare_session(call_id = 289)
args <- inbolimsintern::prepare_session()
conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
params <- inbolimsintern::read_db_arguments(conn, args["call_id"])


### >>> Lees data in

DNA <- DBI::dbReadTable(conn = conn, name = "C_DNA_EXTRACTION")

### >>> Genereer de plaatdata

dfPlates <- inbolimsintern::gen_seq_create_plates(DNA, params)



### >>> Schrijf de data weg in de databank

DBI::dbGetQuery(conn, "delete from C_DNA_EXTRACTION")
DBI::dbWriteTable(conn, name = "C_DNA_EXTRACTION", value = dfPlates, overwrite = TRUE, append = FALSE)
DBI::dbDisconnect(conn)
