#ELC - ELC CALC LIMITS

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "ELC_CALC_LIMITS")
writeLines(con = logfile, paste0("ELC_CALC_LIMITS\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 1748 #call_id <- 3664 #vernieuwde query
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

### SQL code ophalen

sqlcode_new <- paste(readLines(params %>% filter(ARG_NAME == "SQLFILE_NEW") %>%
                                 pull(VALUE),
                           encoding = "UTF-8"),
                  collapse = "\n")
sqlcode_comp <- paste(readLines(params %>% filter(ARG_NAME == "SQLFILE_COMP") %>%
                                  pull(VALUE),
                           encoding = "UTF-8"),
                 collapse = "\n")
cat(sqlcode_new,  "\n", file = logfile, append = TRUE)

### Data inlezen
try({
  dataOrig <- dbGetQuery(conn, stringi::stri_enc_toascii(sqlcode_new)) %>%
    mutate(rownr = 1:nrow(.)) %>%
    arrange(ANALYSIS, NAME, BATCH, ORDER_NUMBER)
  print(dim(dataOrig))
  cat(nrow(dataOrig),  " rijen\n", file = logfile, append = TRUE)
}, outFile = logfile)

try({
  dataOrigComp <- dbGetQuery(conn, stringi::stri_enc_toascii(sqlcode_comp)) %>%
    mutate(rownr = 1:nrow(.)) %>%
    arrange(ANALYSIS, NAME, BATCH, ORDER_NUMBER)
  print(dim(dataOrigComp))
  cat(nrow(dataOrigComp),  " rijen\n", file = logfile, append = TRUE)
}, outFile = logfile)

### De eerste waarde in de batch zoeken
firstinbatch <- dataOrig %>%
  group_by(BATCH, SAMPLE_NAME, ANALYSIS, NAME) %>%
  summarise(chosen_row = min(rownr))

firstinbatch_comp <- dataOrigComp %>%
  group_by(BATCH, SAMPLE_NAME, ANALYSIS, NAME) %>%
  summarise(chosen_row = min(rownr))

dataOrigTotal <- dataOrig %>%
  group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME) %>%
  summarise(N_tot = n())


data <- dataOrig %>%
  filter(rownr %in% (firstinbatch %>% pull(chosen_row)),
         !is.na(ENTRY)) %>%
  mutate(VALUE = as.numeric(ENTRY),
         period = "new")
cat(nrow(data),  " geselecteerde rijen (enkel eerste in batch)\n", file = logfile, append = TRUE)



dataComp <- dataOrigComp %>%
  filter(rownr %in% (firstinbatch %>% pull(chosen_row)),
         !is.na(ENTRY)) %>%
  mutate(VALUE = as.numeric(ENTRY),
         period = "old")
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
  do(calc_elc_stats(.$VALUE)) %>%
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
#Statistische testen tussen periodes

compare_with_t <- function(x){
  res <- try(t.test(VALUE ~ period, data = x))
  if (class(res) == "try-error"){
    #print("error")
    return (NA)
  }
  #print(c(checkT, nrow(x)))
  res$p.value
}

compare_with_F <- function(x){
  res <- try(var.test(VALUE ~ period, data = x))
  if (class(res) == "try-error"){
    #print("error")
    return (NA)
  }
  #print(c(checkF, nrow(x)))
  res$p.value
}

dataCombined <- bind_rows(data, dataComp) %>%
  group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME) %>%
  do(n1 =nrow(.[.$period == "old", , drop = FALSE]),
     n2 =nrow(.[.$period == "new", , drop = TRUE]),
     p_t_test = compare_with_t(.),
     p_F_test = compare_with_F(.)) %>%
  mutate(n1 = n1[[1]],
         n2 = n2[[1]],
         p_t_test = p_t_test[[1]],
         p_F_test = p_F_test[[1]])

dfStatsAll <- dfStats %>%
  left_join(dataCombined) %>%
  left_join(dataOrigTotal)


cat(nrow(dfStats),  " rijen statistieken\n", file = logfile, append = TRUE)

csvpath <- params %>% filter(ARG_NAME == "SQLFILE_NEW") %>% pull(VALUE)
csvpath <- gsub("\\.sql", replacement = ".csv", x = csvpath)
csvpath <- gsub("sqlcode", replacement = "export", x = csvpath)

write_excel_csv2(dfStatsAll, path = csvpath)



