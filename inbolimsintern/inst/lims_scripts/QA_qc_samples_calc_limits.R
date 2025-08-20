#########################
#ELC - ELC CALC LIMITS
#########################
#Maybe superseeded by ELC_qc_trend_evaluation


#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)

args <- commandArgs(trailingOnly = TRUE)
setup <- try(r_session_setup(args))
#args <- c("LWL8DEV", "9953", "TEST_INT"); setup <- try(r_session_setup(args, test_mode = TRUE))
if (inherits(setup, "try-error")) {
  stop("probleem bij setup script")
}
invisible(list2env(setup, envir = .GlobalEnv))

#// parameters ophalen
##=====================

#exportfile ophalen
out_file <- try({
  params %>% filter(ARG_NAME == "EXPORTFILE") %>% pull(VALUE)
})
if (inherits(out_file, "try-error")) {
  write_db_log("Could not find export file", "E")
  stop(out_file)
}

#sql code ophalen
sql_code <- try({
  paste(readLines(params %>% filter(ARG_NAME == "SQLFILE") %>% pull(VALUE),
                  encoding = "UTF-8"),
                  collapse = "\n")
})
if (inherits(sql_code, "try-error")) {
  write_db_log("Could not retrieve sql code", "E")
  stop(sql_code)
}

#jaartallen ophalen
ylast <- try(params %>% filter(ARG_NAME == "EIND") %>% pull(VALUE))
if (inherits(ylast, "try-error")) {
  write_db_log("Could not retrieve the year information", "E")
  stop(ylast)
}

#// data_import
##=====================

### Data inlezen
message("even geduld, data uit de db aan het laden. Tot max 30-tal seconden")

ymin1 <-  lubridate::ymd(ylast) - lubridate::years(1)
ymin2 <-  lubridate::ymd(ylast) - lubridate::years(2)
ystart <- lubridate::ymd(ylast) - lubridate::years(3)
e <- try({
  dataOrig <- DBI::dbGetQuery(conn, stringi::stri_enc_toascii(sql_code)) %>%
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
                summarise(chosen_row = min(rownr),
                          .groups = "drop_last") %>%
                mutate(IS_FIRST = TRUE),
              by = c("rownr" = "chosen_row", "BATCH", "SAMPLE_NAME", "ANALYSIS", "NAME")
                ) %>%
    mutate(IS_FIRST = ifelse(is.na(IS_FIRST), FALSE, IS_FIRST))
})
if (inherits(e, "try-error")) {
  write_db_log("Could not read data", "E")
  stop(e)
}

if (nrow(dataOrig) == 0) {
  write_db_log("No records found in data", "E")
  stop(e)
} else {
  write_db_log(paste0("Aantal records in data: ", nrow(dataOrig)), "P")
}

### aantallen

dataSummary <- dataOrig %>%
  filter(!is.na(PRODUCT)) %>%
  group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME, PERIOD) %>%
  summarise(n_tot = n(),
            n_batch = sum(IS_FIRST, na.rm = TRUE),
            .groups = "drop")

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

message("start berekeningen (kan eventjes duren)")
dfStats <- dataOrig %>%
  filter(IS_FIRST) %>%
  mutate(VALUE = as.numeric(ENTRY)) %>%
  group_by(LIMIT_STAGE, LIMIT_SAMPLING_POINT, LIMIT_PRODUCT, LIMIT_GRADE, LIMIT_ANALYSIS, LIMIT_NAME, C_CTR_ADD) %>%
  do(calculate_elc_periodic_stats(.))

#zet vlag voor waarden die niet berekend moeten worden
dfStats <- dfStats %>%
  mutate(USE_CALCULATION = ifelse(regexpr("\\_PBL", LIMIT_GRADE)>0, FALSE, TRUE))

write_db_log( paste0("Berekeningen uigevoerd, records: ", nrow(dfStats)), "P")

e <- try({
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
})
if (inherits(e, "try-error")) {
  write_db_log(paste0("wegschrijven file mislukt: ", e), "E")
} else {
  write_db_log(paste0("resultaat bewaard als: ", out_file), "C")
}
