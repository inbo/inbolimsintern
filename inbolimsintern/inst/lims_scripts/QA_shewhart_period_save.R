
##################################
#Save QC period charts
##################################

#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#args <- c("LWL8DEV", "10020", "TEST_INT"); setup <- try(r_session_setup(args, test_mode = TRUE))
invisible(list2env(setup, envir = .GlobalEnv))

if (inherits(setup, "try-error")) {
  write_db_log(paste("Problem setting up R script", setup), "E")
  stop("probleem bij setup script")
} else {
  write_db_log("R session setup finished", "P")
}


#// retrieve arguments
##=====================

e  <- try({
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
})
if (inherits(e, "try-error")) {
  write_db_log(paste("problem retrieving parameters", e), "E")
  stop(e)
} else {
  write_db_log("parameters retrieved", "P")
}


### data inlezen

e <- try({
  alldata <- get_ELC_data(conn, sqlfile, keep = Inf) %>%
    filter(C_CTR_ADD == 'T') %>%
    mutate(CALL_ID = call_id)  #voeg callID toe
}, outFile = logfile)
if(inherits(e, "try-error")) {
  write_db_log(paste("data kon niet ingelezen worden:", e), "E")
  stop(e)
}
if (nrow(alldata) == 0) {
  stop("geen data records gevonden")
  write_db_log("geen data records gevonden", "E")
} else {
  write_db_log(paste("aantal records:", nrow(alldata)), "P")
}

combis <- unique(alldata$combi)

archive_data <- NULL
for (comb in combis) {
  message(comb)
  plotdata <- alldata %>% filter(comb == combi)
  htmldata <- elc_htmldata(plotdata)
  archive_data <- rbind(archive_data, htmldata$plot)
}
if (nrow(archive_data) == 0) {
  write_db_log(" geen plot records om te bewaren", "E")
  stop("geen data rijen")
} else {
  write_db_log(paste("plot records om te bewaren", nrow(archive_data)), "P")
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


e <- try(
  DBI::dbWriteTable(conn, name = "C_CTR_ARCHIVE", value = archive_data_export,
                    overwrite = FALSE, append = TRUE)
)
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
} else {
  write_db_log(paste("archiefgegevens bewaard, aantal punten:", nrow(archive_data_export)), "C")
}

