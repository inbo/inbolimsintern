#ELC import limits

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)
library(readxl)

### Logfile
logfile <- logfile_start(prefix = "ELC_IMPORT_LIMITS")
writeLines(con = logfile, paste0("ELC_IMPORT_LIMITS\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 1752
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

label <- params %>% filter(ARG_NAME =="LABEL") %>% pull(VALUE)
gebruiker <- params %>% filter(ARG_NAME =="USER") %>% pull(VALUE)
datum <- params %>% filter(ARG_NAME =="DATUM") %>% pull(VALUE)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

importdata <- read_excel(params %>% filter(ARG_NAME =="FILE") %>% pull(VALUE))
cat(nrow(importdata), " records\n", file = logfile, append = TRUE)

###SCHRIJF WEG NAAR C_QC_LIMITS

queries <- NULL
for (i in 1:nrow(importdata)) {
  sdval <- unlist(importdata[i, "Sd"])
  sdval <- ifelse(is.na(sdval) | sdval == "NA", 0, sdval)
  queries[[i]] <- paste0(
  "insert into C_QC_LIMITS (LABEL, QC_SAMPLE, ANALYSIS, COMPONENT, PRODUCT, PRODUCT_VERSION, AVG_MEASURED, SD_MEASURED) VALUES (",
  "'", label, "'",",",
  "'", unlist(importdata[i, "SAMPLE_NAME"]), "'",",",
  "'", unlist(importdata[i, "ANALYSIS"]), "'",",",
  "'", unlist(importdata[i, "NAME"]), "'",",",
  "'", unlist(importdata[i, "PRODUCT"]), "'",",",
       unlist(importdata[i, "MAX_VERSION"]),",",
       unlist(importdata[i, "Avg"]),",",
       sdval,
  ")"
  )
}
for (i in 1:length(queries)) {
  dbGetQuery(conn, queries[[i]])
}



###SCHRIJF WEG NAAR PRODUCT_SPECS

update_queries <- character(nrow(importdata))
for (i in 1:nrow(importdata)) {
  data <- importdata[i, ]
  data$C_CERTIFIED_VALUE = as.numeric(data$C_CERTIFIED_VALUE)
  data$C_CERTIFIED_VALUE = as.numeric(data$C_CERTIFIED_SD)
  data$Avg = as.numeric(data$Avg)

  Cval <- as.numeric(data$C_CERTIFIED_VALUE)
  Csd <- as.numeric(data$C_CERTIFIED_SD)
  Avg <- as.numeric(data$Avg)

  Cval <- ifelse(is.na(Cval) | Cval == "NA", "Null", as.numeric(Cval))
  Csd <- ifelse(is.na(Csd) | Csd == "NA", 0, as.numeric(Csd))
  Avg <- ifelse(is.na(Avg) | Avg == "NA", 0, as.numeric(Avg))
  Sd <- ifelse(is.na(data$Sd) | data$Sd == "NA", 0, as.numeric(data$Sd))

  q = paste0("update PRODUCT_SPEC set ",
             " C_CTR_X = ", Avg,
             " ,C_CTR_SD = ", Sd,
             " ,MIN_VALUE = ", Avg - 2 * Sd,
             " ,MAX_VALUE = ", Avg + 2 * Sd,
             " ,C_CERTIFIED_VALUE = ", Cval,
             " ,C_CERTIFIED_SD = ", Csd,
             " WHERE  PRODUCT = '", data$PRODUCT, "'",
             " and VERSION = ", data$MAX_VERSION, "",
             " and GRADE = '", data$SAMPLE_NAME, "'",
             " and ANALYSIS = '", data$ANALYSIS, "'",
             " and COMPONENT = '", data$NAME, "'")
  update_queries[i] <- q
}

for (i in 1:length(update_queries)) {
  DBI::dbGetQuery(conn, update_queries[i])
}




