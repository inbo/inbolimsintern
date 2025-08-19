###################################################
###########   QC CHARTS       #####################
###################################################


### >>> Configure R session

library(dplyr)
library(ggplot2)
library(inbolimsintern)

logfile <- logfile_start(prefix = "QC_CHART")
call_id <- 0 #324

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

### >>> Declare variables

try({
  analyse <-
    params %>%
    filter(ARG_NAME == "ANALYSIS") %>%
    pull(VALUE)

  components <-
    params %>%
    filter(ARG_NAME == "COMP_NAMES") %>%
    pull(VALUE) %>%
    strsplit(split = ",") %>%
    unlist() %>%
    convert_to_simple_ascii()

  batch <-  #batch nodig om controlestalen te zoeken
    params %>%
    filter(ARG_NAME == "BATCH") %>%
    pull(VALUE)

  num <-
    params %>%
    filter(ARG_NAME == "NUM") %>%
    pull(VALUE)
}, outFile = logfile)



### >>> Execute Core Code

#Haal de resultaten uit de databank
try({
  dfResultaten <- inbolimsintern::select_control_samples(conn, num, batch, analyse, components)
}, outFile = logfile)

#Maak het HTML rapport
try({
  inbolimsintern::html_qc_report(dfResultaten)
}, outFile = logfile)


