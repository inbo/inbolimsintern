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
call_id <- 0 #call_id <- 1748 #call_id <- 3728 #vernieuwde query call_id <- 4994,
#verbeterde quary call_id <- 5222
try({
  args <- inbolimsintern::prepare_session(call_id) #hier is call_id nog niet van belang, wordt in de functie gezet
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

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
  group_by(LIMIT_PRODUCT, LIMIT_GRADE, LIMIT_ANALYSIS, LIMIT_NAME) %>%
  do(calculate_elc_periodic_stats(.))

cat(nrow(dfStats)," rijen statistieken\n", file = logfile, append = TRUE)

csvpath <- params %>% filter(ARG_NAME == "SQLFILE") %>% pull(VALUE)
csvpath <- gsub("\\.sql", replacement = ".csv", x = csvpath)
csvpath <- gsub("sqlcode", replacement = "export", x = csvpath)

#write_excel_csv2(dfStatsAll, path = csvpath)
write_excel_csv2(dfStats, file = csvpath)



### aantalstatistieken
# aantallen <-
#   dataOrig %>%
#     group_by(LIMIT_PRODUCT, LIMIT_VERSION, LIMIT_GRADE, LIMIT_ANALYSIS, LIMIT_NAME, PERIOD) %>%
#     filter(!is.na(PRODUCT)) %>%
#     summarise(aantal = n(),
#             aantal_batches = n_distinct(BATCH))
#
#
#
#
#
# data_recent_gemeten <- dataOrig %>%
#   filter(jaar == max(jaar)) %>%
#   group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME) %>%
#   summarise(aantal = n()) %>%
#   filter(aantal > 0) %>%
#   mutate(gemeten_in_laatste_jaar = TRUE)
#
# data <- dataOrig %>%
#   left_join(firstinbatch %>% ungroup %>% select(chosen_row, is_first),
#             by = c("rownr" = "chosen_row")) %>%
#   mutate(ifelse(is.na(is_first), FALSE, is_first),
#          value = as.numeric(ENTRY)) %>%
#   left_join(data_recent_gemeten)
#
# jaren <- sort(unique(data$jaar))
#
#
# dataSummary <- data %>%
#   group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME, jaar) %>%
#   summarise(n_tot = n(),
#             n_first = sum(is_first, na.rm = TRUE))
#
# dataSummaryWide = pivot_wider(dataSummary,
#             id_cols = c(PRODUCT, SAMPLE_NAME , ANALYSIS, NAME),
#             names_from = jaar,
#             values_from = c(n_tot, n_first), values_fill = 0)
#
# dfLimits <- data %>%
#   group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME) %>%
#   summarise(aantal = n(),
#             C_CTR_X = max(C_CTR_X, na.rm = TRUE), #if missing -Inf
#             C_CTR_SD = max(C_CTR_SD, na.rm = TRUE),
#             C_CERTIFIED_VALUE = max(C_CERTIFIED_VALUE, na.rm = TRUE),
#             C_CERTIFIED_SD = max(C_CERTIFIED_SD, na.rm = TRUE)) %>%
#   mutate(across(starts_with("C_"), function(x) ifelse(x == -Inf, NA, x)))

### Statistieken bereken en exporteren
#productversie wordt genegeerd,
#dus de gemiddeldes kunnen over verschillende productversies gaan

# dfStatsFirst <- data %>%
#   filter(jaar == max(data$jaar),
#          is_first == TRUE) %>%
#   group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME, jaar) %>%
#   do(calc_elc_stats(.$value)) %>%
#   select(-N_all, -Avg_all, -Sd_all, -N_used, -min3s, -max3s, -jaar,
#          Avg_first = Avg, Sd_first = Sd)
#
#
# dfStats <- data %>%
#   group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME, jaar) %>%
#   do(calc_elc_stats(.$value)) %>%
#   mutate(startdatum = params %>% filter(ARG_NAME == "START") %>% pull(VALUE),
#          einddatum = params %>% filter(ARG_NAME == "EIND") %>% pull(VALUE),
#          gebruiker = params %>% filter(ARG_NAME == "USER") %>% pull(VALUE)) %>%
#   left_join(dataSummary) %>%
#   left_join(dfLimits %>% select(-aantal),
#             by = c("PRODUCT",
#                    "ANALYSIS",
#                    "NAME",
#                    "SAMPLE_NAME"))
# #Statistische testen tussen periodes
#
# yrange <- range(data$jaar)
# kolnamen <- c("PRODUCT", "SAMPLE_NAME" ,"ANALYSIS", "NAME",
#               paste0("n_tot_", yrange[1]:yrange[2]),
#               paste0("n_first_", yrange[1]:yrange[2]),
#               paste0("N_used_", yrange[2]),
#               paste0("Avg_", yrange[1]:yrange[2]),
#               paste0("Sd_", yrange[1]:yrange[2]),
#               paste0("min3s_", yrange[2]),
#               paste0("max3s_", yrange[2]),
#               paste0("C_CTR_X_", yrange[2]),
#               paste0("C_CTR_SD_", yrange[2]),
#               paste0("C_CERTIFIED_VALUE_", yrange[2]),
#               paste0("C_CERTIFIED_SD_", yrange[2]))
#
#
# dfStatsWide <- dfStats %>%
#   pivot_wider(
#     id_cols = c(PRODUCT, SAMPLE_NAME , ANALYSIS, NAME),
#     names_from = jaar,
#     values_from = c(n_tot, n_first, N_used, Avg, Sd, min3s, max3s,
#                     C_CTR_X, C_CTR_SD, C_CERTIFIED_VALUE, C_CERTIFIED_SD),
#     values_fill = 0) %>%
#   select(all_of(kolnamen))
#
#
#
# dataCombined <- data %>%
#   group_by(PRODUCT, SAMPLE_NAME, ANALYSIS, NAME) %>%
#   do(compare_with_tf(., years = sort(unique(data$jaar))))
#
# dfStatsAll <- dfStatsWide %>%
#   left_join(dataCombined) %>%
#   left_join(dfStatsFirst %>% select(-jaar)) %>%
#   mutate(across(where(is.numeric), round, 6))
#
#
# check_for_abnormality <- function(data, years) {
#   check_pval <- function(x) {
#     #print(x)
#     if (is.na(x)) return ("_")
#     if (x <= 0.01) return("*")
#     if (x <= 0.05) return(".")
#     return("_")
#   }
#   colsp = paste0("pval_t_", years[4], "-", years[3:1])
#   colsf = paste0("pval_F_", years[4], "-", years[3:1])
#   pt1 <- data[[colsp[1]]]
#   pt2 <- data[[colsp[2]]]
#   pt3 <- data[[colsp[3]]]
#   pf1 <- data[[colsf[1]]]
#   pf2 <- data[[colsf[2]]]
#   pf3 <- data[[colsf[3]]]
#   message = ""
#   message <- paste0(message, check_pval(pt1), check_pval(pt2), check_pval(pt3), "|")
#   message <- paste0(message, check_pval(pf1), check_pval(pf2), check_pval(pf3))
#   if (!grepl('\\*', message)) {
#     if(!is.na(data[[paste0("C_CTR_X_", max(years))]])) {
#       voorstel_x = data[[paste0("C_CTR_X_", max(years))]]
#       voorstel_sd = data[[paste0("C_CTR_SD_", max(years))]]
#     } else {
#       voorstel_x = data[[paste0("Avg_", max(years))]]
#       voorstel_sd = data[[paste0("Sd_", max(years))]]
#     }
#   } else {
#     voorstel_x = NA
#     voorstel_sd = NA
#   }
#   print(c(message, voorstel_x, voorstel_sd))
#   data.frame(drift = message, C_CTR_X_NEW = voorstel_x, C_CTR_SD_NEW = voorstel_sd)
# }
#
# dfStatsExport <- dfStatsAll %>%
#   group_by_all() %>%
#   do(check_for_abnormality(., years = jaren)) %>%
#   left_join(data_recent_gemeten)
#


# cat(nrow(dfStats)," rijen statistieken\n", file = logfile, append = TRUE)
#
# csvpath <- params %>% filter(ARG_NAME == "SQLFILE") %>% pull(VALUE)
# csvpath <- gsub("\\.sql", replacement = ".csv", x = csvpath)
# csvpath <- gsub("sqlcode", replacement = "export", x = csvpath)
#
# #write_excel_csv2(dfStatsAll, path = csvpath)
# write_excel_csv2(dfStatsExport, path = csvpath)
#

