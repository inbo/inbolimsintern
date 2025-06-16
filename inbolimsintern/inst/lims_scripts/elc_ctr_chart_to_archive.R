
# QC chart to archive

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "CTR_SAVE")
writeLines(con = logfile, paste0("Bewaren archiefkaarten\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 4269 call_id <- 4275 call_id <- 5388 5590 8964
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

try({
  samplingpoint = 'NONE'
  sqlfile <- filter(params, ARG_NAME == "SQL_FILE") %>% pull(VALUE)
  htmlfile <- filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE)
  chartlabel <- filter(params, ARG_NAME == "LABEL") %>% pull(VALUE)
  product <- filter(params, ARG_NAME == "PRODUCT") %>% pull(VALUE)
  productversie <- filter(params, ARG_NAME == "PRODUCT_VERSION") %>% pull(VALUE)
  label <- filter(params, ARG_NAME == "LABEL") %>% pull(VALUE)
  datetime <- filter(params, ARG_NAME == "DATE_TIME") %>% pull(VALUE)
  user <- filter(params, ARG_NAME == "USER") %>% pull(VALUE)
  htmlrootshort <- substring(htmlfile, max(unlist(gregexpr("\\\\", htmlfile))) + 1, nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
  htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile)))) #including last backslash
}, outFile = logfile)


### data inlezen

try({
  #haal sqlcode op
  sqlcode <- readLines(sqlfile)
  sqlcode <- paste(sqlcode, collapse = "\n")
  cat("\n", sqlcode, file = logfile, append = TRUE)

  #haal data binnen (deze bevat reeds de limieten)
  alldata<- get_ELC_data(conn, sqlfile, keep = Inf, logfile = logfile) %>%
    filter(C_CTR_ADD == 'T') %>%
    mutate(CALL_ID = call_id)  #voeg callID toe
}, outFile = logfile)

if (nrow(alldata) == 0) cat("\nGEEN DATA\n", file = logfile, append = TRUE)
combis <- unique(alldata$combi)

archive_data <- NULL
for (comb in combis) {
  print(comb)
  plotdata <- alldata %>% filter(comb == combi)
  htmldata <- elc_htmldata(plotdata)
  archive_data <- rbind(archive_data, htmldata$plot)
}
archive_data_export <- archive_data %>%
  transmute(LABEL = label, DATE = datetime, USER = user,
            PRODUCT = product, LIMIT_VERSION = VERSION,
            SAMPLING_POINT = samplingpoint, SAMPLE_NAME,
            BATCH, BATCHNR, CHECK_RULES,  ANALYSIS, NAME, ENTERED_ON, ENTRY, UNITS,
            C_CTR_X, C_CTR_SD, C_CERTIFIED_VALUE, C_CERTIFIED_SD,
            OUT3S, WARN, OUT2S, DRIFT, BIAS, COLOR, SIZE,
            LCL3S, LCL2S, LCL1S, UCL1S, UCL2S, UCL3S, COMBI = combi,
            BATCH_POSITION = ORDER_NUMBER, CALL_ID) %>%
  arrange(PRODUCT, SAMPLING_POINT, ANALYSIS, NAME, SAMPLE_NAME, BATCHNR)

DBI::dbWriteTable(conn, name = "C_CTR_ARCHIVE", value = archive_data_export,
                  overwrite = FALSE, append = TRUE)

