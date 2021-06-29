#ELC - ELC CALC LIMITS

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "ELC_CALC_LIMITS")
writeLines(con = logfile, paste0("ELC_CALC_LIMITS\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 1748
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

### SQL code ophalen
sqlcode <- paste(readLines(params %>% filter(ARG_NAME == "SQLFILE") %>% pull(VALUE),
                           encoding = "UTF-8"),
                  collapse = "\n")
cat(sqlcode,  "\n", file = logfile, append = TRUE)

### Data inlezen
try({
  dataOrig <- dbGetQuery(conn, stringi::stri_enc_toascii(sqlcode)) %>%
    mutate(rownr = 1:nrow(.)) %>%
    arrange(ANALYSIS, NAME, BATCH, ORDER_NUMBER)
  print(dim(dataOrig))
  cat(nrow(dataOrig),  " rijen\n", file = logfile, append = TRUE)
}, outFile = logfile)

### De eerste waarde in de batch zoeken
firstinbatch <- dataOrig %>%
  group_by(BATCH, SAMPLE_NAME, ANALYSIS, NAME) %>%
  summarise(chosen_row = min(rownr))

data <- dataOrig %>%
  mutate(value = as.numeric(ENTRY)) %>%
  filter(rownr %in% (firstinbatch %>% pull(chosen_row)),
         !is.na(value))
cat(nrow(data),  " geselecteerde rijen (enkel eerste in batch)\n", file = logfile, append = TRUE)


dfLimits <- data %>%
  group_by(PRODUCT, PRODUCT_VERSION, SAMPLE_NAME, ANALYSIS, NAME) %>%
  summarise(aantal = n(),
            C_CTR_X = max(C_CTR_X, na.rm = TRUE),
            C_CTR_SD = max(C_CTR_SD, na.rm = TRUE),
            C_CERTIFIED_VALUE = max(C_CERTIFIED_VALUE, na.rm = TRUE),
            C_CERTIFIED_SD = max(C_CERTIFIED_SD, na.rm = TRUE))

### Statistieken bereken en exporteren
#productversie wordt genegeerd,
#dus de gemiddeldes kunnen over verschillende productversies gaan
dfStats <- data %>%
  group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME) %>%
  do(calc_elc_stats(.$value)) %>%
  mutate(startdatum = params %>% filter(ARG_NAME == "START") %>% pull(VALUE),
         einddatum = params %>% filter(ARG_NAME == "EIND") %>% pull(VALUE),
         gebruiker = params %>% filter(ARG_NAME == "USER") %>% pull(VALUE)) %>%
  left_join(data %>% group_by(PRODUCT) %>% summarize(MAX_VERSION = max(PRODUCT_VERSION))) %>%
  left_join(dfLimits %>% select(-aantal),
            by = c("PRODUCT",
                   "MAX_VERSION" = "PRODUCT_VERSION",
                   "ANALYSIS",
                   "NAME",
                   "SAMPLE_NAME"))

cat(nrow(dfStats),  " rijen statistieken\n", file = logfile, append = TRUE)

csvpath <- params %>% filter(ARG_NAME == "SQLFILE") %>% pull(VALUE)
csvpath <- gsub("\\.sql", replacement = ".csv", x = csvpath)
csvpath <- gsub("sqlcode", replacement = "export", x = csvpath)

write_excel_csv2(dfStats, path = csvpath)



