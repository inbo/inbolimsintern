#ELC - ELC CALC LIMITS

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)
library(lubridate)
library(stringr)

### Logfile
logfile <- logfile_start(prefix = "ELC_CALC_LIMITS")
writeLines(con = logfile, paste0("ELC_CALC_LIMITS\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 1748 #call_id <- 3728 #vernieuwde query call_id <- 5323,
#verbeterde quary call_id <- 5222
try({
  args <- inbolimsintern::prepare_session(call_id) #hier is call_id nog niet van belang, wordt in de functie gezet
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

### Export file ophalen
out_file <- params %>% filter(ARG_NAME == "EXPORTFILE") %>% pull(VALUE)

### SQL code ophalen

sql_file <- paste(readLines(params %>% filter(ARG_NAME == "SQLFILE") %>%
                                 pull(VALUE),
                           encoding = "UTF-8"),
                  collapse = "\n")
cat(sql_file,  "\n", file = logfile, append = TRUE)


### Data inlezen
print("even geduld, data uit de db aan het laden. Duurt een 30-tal seconden")
ylast <- params %>% filter(ARG_NAME == "EIND") %>% pull(VALUE)
ymin1 <- ymd(ylast) - years(1)
ymin2 <- ymd(ylast) - years(2)
ystart <- ymd(ylast) - years(3)
try({
  dataOrig <- dbGetQuery(conn, stringi::stri_enc_toascii(sql_file)) %>%
    mutate(rownr = 1:nrow(.),
           PERIOD = ifelse(ENTERED_ON > ymin1,
                         "LAST",
                         ifelse(ENTERED_ON > ymin2,
                                "PREVIOUS",
                                "TWO_AGO"))) %>%
    arrange(ANALYSIS, NAME, BATCH, ORDER_NUMBER)

  dataOrig <- dataOrig %>%
    left_join(dataOrig %>%
                group_by(BATCH, SAMPLE_NAME, ANALYSIS, NAME) %>%
                summarise(chosen_row = min(rownr)) %>%
                mutate(IS_FIRST = TRUE),
              by = c("rownr" = "chosen_row", "BATCH", "SAMPLE_NAME", "ANALYSIS", "NAME")
                ) %>%
    mutate(IS_FIRST = ifelse(is.na(IS_FIRST), FALSE, IS_FIRST))

  print(dim(dataOrig))
  cat(nrow(dataOrig),  " rijen\n", file = logfile, append = TRUE)
}, outFile = logfile)

### aantallen

dataSummary <- dataOrig %>%
  filter(!is.na(PRODUCT)) %>%
  group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME, PERIOD) %>%
  summarise(n_tot = n(),
            n_batch = sum(IS_FIRST, na.rm = TRUE))

dataSummaryWide <- pivot_wider(dataSummary,
            id_cols = c(PRODUCT, SAMPLE_NAME , ANALYSIS, NAME),
            names_from = PERIOD,
            values_from = c(n_tot, n_batch), values_fill = 0)


### Productinformatie

productinfo <- dataOrig %>%
  select(LIMIT_PRODUCT, LIMIT_VERSION, LIMIT_GRADE, LIMIT_ANALYSIS, LIMIT_NAME,
         starts_with("C_")) %>%
  distinct()

#BEREKENINGEN

dfStats <- dataOrig %>%
  filter(IS_FIRST) %>%
  mutate(VALUE = as.numeric(ENTRY)) %>%
  group_by(LIMIT_STAGE, LIMIT_SAMPLING_POINT, LIMIT_PRODUCT, LIMIT_GRADE, LIMIT_ANALYSIS, LIMIT_NAME, C_CTR_ADD) %>%
  do(calculate_elc_periodic_stats(.))

#zet vlag voor waarden die niet berekend moeten worden
dfStats <- dfStats %>%
  mutate(USE_CALCULATION = ifelse(regexpr("\\_PBL", LIMIT_GRADE)>0, FALSE, TRUE))

cat(nrow(dfStats)," rijen statistieken\n", file = logfile, append = TRUE)

# csvpath <- params %>% filter(ARG_NAME == "SQLFILE") %>% pull(VALUE)
# csvpath <- gsub("\\.sql", replacement = ".csv", x = csvpath)
# csvpath <- gsub("sqlcode", replacement = "export", x = csvpath)

#write_excel_csv2(dfStatsAll, path = csvpath)
write_excel_csv2(dfStats %>%
                   select(STAGE = LIMIT_STAGE,
                          SAMPLING_POINT = LIMIT_SAMPLING_POINT,
                          PRODUCT = LIMIT_PRODUCT,
                          GRADE = LIMIT_GRADE,
                          ANALYSIS = LIMIT_ANALYSIS,
                          COMPONENT = LIMIT_NAME,
                          C_CTR_ADD,
                          USE_CALCULATION,
                          N_ORIG = n_orig,
                          MEAN_ORIG = mean_orig,
                          SD_ORIG = sd_orig,
                          N = n,
                          MEAN = mean,
                          SD = sd,
                          N_PREVIOUS = n_prev,
                          MEAN_PREVIOUS = mean_prev,
                          SD_PREVIOUS = sd_prev,
                          PVAL_MEAN = pval_t,
                          PVAL_SD = pval_f
                          ),
                 file = out_file)

