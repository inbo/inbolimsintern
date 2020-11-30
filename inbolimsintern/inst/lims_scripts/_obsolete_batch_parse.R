###################################################
###########   Parse Instrument Files ##############
###################################################


### >>> Configure R session

#input parameters: INPUTFILE, OUTPUTDIR, TYPE, SHEET, CELL, MAXCOL

library(dplyr)          #nodig voor de bewerkingen
library(readxl)         #nodig om excel files in te lezen
library(readr)          #nodig om text bestanden in te lezen
library(inbolimsintern) #bevat de parse functies

logfile <- logfile_start(prefix = "BATCH_PARSE")
call_id <- 0 #indien NULL, komt uit argumenten, anders test: 627

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])

  userName = params$VALUE[params$ARG_NAME == "USER"]
  inputfile = params$VALUE[params$ARG_NAME == "INPUTFILE"]
  outputdir = params$VALUE[params$ARG_NAME == "OUTPUTDIR"]
  filetype = params$VALUE[params$ARG_NAME == "TYPE"]
  sheet = params$VALUE[params$ARG_NAME == "SHEET"]
  cell = params$VALUE[params$ARG_NAME == "CELL"]
  maxcol = as.numeric(params$VALUE[params$ARG_NAME == "MAXCOL"])
  split <- 200
}, outFile = logfile)


### >>> FIND FILES AND PARSE THEM IN AN OBJECT

try({
  if (filetype == "xls_default") {
    print("type xls_default")
    import_data <- inbolimsintern::batch_parse_xls_default(
      file = inputfile,
      sheet = sheet,
      user = userName)
  } else {
    if (filetype == "xls---") {
      print("type xls---")
      import_data <- inbolimsintern::batch_parse_xls_tripledash(
        file = inputfile,
        sheet = sheet,
        cell = cell,
        maxcol = maxcol,
        user = userName)
    } else {
      print("niet gekend type")
    }
  }
}, outFile = logfile)


### >>> PLACE PARSED FILES IN C_RESULT_IMPORT IN LIMS DB
try({
  first_test_number <- as.numeric(import_data[1,2])
}, outFile = logfile)


### >>> Lees de data
try({

  q <- paste0("select top(10) b.NAME, b.TEMPLATE, b.OWNER from batch b inner join batch_objects bo on b.NAME = bo.BATCH",
              " where bo.OBJECT_ID = ", first_test_number)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  batch_info <- DBI::dbGetQuery(conn, q)
}, outFile = logfile)


### >>>> GET BATCH INFO
try({
  batchName <- batch_info$NAME[1]
  if (!length(userName)) userName <- "TEST"
  timestamp <- Sys.time()
  userName <- "TEST"
  data_for_db <- import_data %>%
    select(SAMPLE_NUMBER = 1,
           TEXT_ID = 3,
           TEST_NUMBER = 2,
           COMPONENT = 4,
           ENTRY = 5) %>%
    mutate(BATCH = batchName,
           DATE_PLACED = timestamp,
           IMPORTED = "F")
}, outFile = logfile)


### >>> SCHRIJF DATA WEG
try({
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  for (i in 1:nrow(data_for_db)) {
    qry <-  "insert into C_RESULT_IMPORT (BATCH, [USER], TEXT_ID, SAMPLE_NUMBER, TEST_NUMBER, COMPONENT, ENTRY, DATE_PLACED, IMPORTED) VALUES ("
    qry <- paste0(qry,
                  "'", data_for_db$BATCH[i],    "'",       ", ",
                  "'", userName,                "'",       ", ",
                  "'", data_for_db$TEXT_ID[i],  "'",       ", ",
                  data_for_db$SAMPLE_NUMBER[i],        ", ",
                  data_for_db$TEST_NUMBER[i],         ", ",
                  "'", data_for_db$COMPONENT[i], "'",      ", ",
                  "'", data_for_db$ENTRY[i],     "'",      ", ",
                  "'", timestamp,                "'",      ", ",
                  "'", "F",                      "'", ")")
    DBI::dbGetQuery(conn, qry)
  }
})
close(logfile)
