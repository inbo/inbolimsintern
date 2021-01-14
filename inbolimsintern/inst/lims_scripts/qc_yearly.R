
library(dplyr)
library(tidyr)
library(inbolimsintern)
library(DBI)

### >>> INIT

logfile <- logfile_start(prefix = "QC_YEARLY")
call_id <- 0 #

try({
  args <- inbolimsintern::prepare_session(call_id) #675
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

startdate <- filter(params, ARG_NAME == "DATE_FIRST") %>% pull(VALUE)
if (!length(startdate)) startdate <- Sys.Date() - 365

enddate <- filter(params, ARG_NAME == "DATE_LAST") %>% pull(VALUE)
if (!length(enddate)) enddate <- Sys.Date()

maxentries <- filter(params, ARG_NAME == "N_MAX") %>% pull(VALUE)
if (!length(maxentries)) maxentries <- 60

### PROCESS

df_qc_results <- NULL
df_qc_stats <- NULL

try(df_qc_results <- qc_select_results(conn, min_date = startdate, max_date = enddate, n_max = 100000), outFile = logfile) #GET QC RESULTS
try(df_qc_results <- df_qc_results %>% mutate(numENTRY = as.numeric(gsub(",", ".", ENTRY)) %>%  filter(!is.na(numENTRY))), outFile = logfile) #convert to numeric
try(df_qc_stats <- df_qc_results %>%
      nest_by(UID) %>%
      group_by(UID) %>%
      mutate(statistics = qc_calc_stats(data)) %>%
      select(-data) %>%
      separate(col = "UID", sep = "---", into = c("PRODUCT", "ANALYSIS", "COMPONENT", "QC_SAMPLE")), outFile = logfile) #bereken statistieken
try(df_qc_stats <-
      bind_cols(df_qc_stats %>%  select(1:4),
                df_qc_stats$statistics) %>%
      mutate_at(vars(avg_orig:ucl3s), round, digits = 5),
    outFile = logfile)

#2019 weinig data ivgl 2020
df_qc_results2019 <- df_qc_stats2019 <- NULL
df_qc_results2019    <- qc_select_results(conn, min_date = '2019-01-01', max_date = '2019-12-31') %>% mutate(numENTRY = as.numeric(ENTRY)) %>%  filter(!is.na(numENTRY))
df_qc_stats2019 <- df_qc_results2019 %>% nest_by(UID) %>% group_by(UID) %>%
  mutate(statistics = qc_calc_stats(data)) %>% select(-data) %>%
  separate(col = "UID", sep = "---", into = c("PRODUCT", "ANALYSIS", "COMPONENT", "QC_SAMPLE"))
df_qc_stats2019 <- bind_cols(df_qc_stats2019 %>%  select(1:4),df_qc_stats2019$statistics) %>% mutate_at(vars(avg_orig:ucl3s), round, digits = 5)
readr::write_excel_csv2(df_qc_stats2019, path = "Q:/QC_2019.csv")

# 2020 weinig data ivgl 2020
df_qc_results2020 <- df_qc_stats2020 <- NULL
df_qc_results2020    <- qc_select_results(conn, min_date = '2020-01-01', max_date = '2020-12-31') %>% mutate(numENTRY = as.numeric(ENTRY)) %>%  filter(!is.na(numENTRY))
df_qc_stats2020 <- df_qc_results2020 %>% nest_by(UID) %>% group_by(UID) %>%
  mutate(statistics = qc_calc_stats(data)) %>% select(-data) %>%
  separate(col = "UID", sep = "---", into = c("PRODUCT", "ANALYSIS", "COMPONENT", "QC_SAMPLE"))
df_qc_stats2020 <- bind_cols(df_qc_stats2020 %>%  select(1:4),df_qc_stats2020$statistics) %>% mutate_at(vars(avg_orig:ucl3s), round, digits = 5)
readr::write_excel_csv2(df_qc_stats2020, path = "Q:/QC_2020.csv")



################

qc_calc_stats <- function(data, max_entries = Inf, rules = c("rule01", "rule05")) {
  df <- data[[1]]
  stdata <- df %>%  arrange(desc(ENTERED_ON))
  stdata <- stdata %>% slice(1:(min(nrow(stdata), max_entries)))

  #bereken originele statistieken
  rv <- stdata %>%
    summarize(n_orig = nrow(.),
              n_NA = sum(is.na(numENTRY)),
              avg_orig = mean(numENTRY, na.rm = TRUE),
              sd_orig = sd(numENTRY, na.rm = TRUE),
              min_orig = min(numENTRY, na.rm = TRUE),
              q.25_orig = quantile(numENTRY, probs = 0.25, , na.rm = TRUE),
              q.50_orig = median(numENTRY, na.rm = TRUE),
              q.75_orig = quantile(numENTRY, probs = 0.25, na.rm = TRUE),
              max_orig = max(numENTRY),
              n_out = 0,
              avg = avg_orig,
              sd = sd_orig,
              lcl3s = avg  - 3 * sd ,
              lcl2s = avg  - 2 * sd ,
              lcl1s = avg  - 1 * sd ,
              ucl1s = avg  + 1 * sd ,
              ucl2s = avg  + 2 * sd ,
              ucl3s = avg  * 3 * sd
    )

  #bereken resultaten zonder outliers
  stdata_nona <- stdata %>% filter(!is.na(numENTRY))
  entries <- stdata_nona$numENTRY
  rv_iter <- rv
  outliers <- TRUE
  n_out <- 0
  while (outliers) {
    out3s <- qcc_rule01(entries, rv_iter$lcl3s, rv_iter$ucl3s)
    #out2s <- qcc_rule05(entries, rv_iter$lcl2s, rv_iter$ucl2s)
    out2s <- FALSE #dummy
    out <- out3s & out2s
    if (out > 0) print(out)
    n_out <- n_out + sum(out)
    if (sum(out) > 0) {
      outliers <- TRUE
      entries <- entries[!out]
      rv_iter$n_out <- n_out
      rv_iter$avg <- mean(entries, na.rm = TRUE)
      rv_iter$sd <- sd(entries, na.rm = TRUE)
      rv_iter <- rv_iter %>%
        mutate(lcl3s = avg  - 3 * sd ,
               lcl2s = avg  - 2 * sd ,
               lcl1s = avg  - 1 * sd ,
               ucl1s = avg  + 1 * sd ,
               ucl2s = avg  + 2 * sd ,
               ucl3s = avg  * 3 * sd
        )
    } else {
      outliers <- FALSE
    }
  }
  rv_iter
}

###

qc_select_results <- function(conn, min_date = as.Date("2000-01-01"), max_date = Sys.Date(), n_max = 100000) {
  qry_res <- paste0(
    " select top(", n_max + 1, ") s.SAMPLE_NUMBER, r.TEST_NUMBER, r.RESULT_NUMBER, s.TEXT_ID, s.SAMPLE_NAME, ",
    " s.PRODUCT, s.PRODUCT_VERSION, QC_SAMPLE = qcs.NAME, ",
    " t.BATCH, r.ANALYSIS, r.NAME, r.ENTERED_ON, r.STATUS, r.ENTRY",
    " from result r",
    " inner join test t on t.test_number = r.test_number",
    " inner join sample s on r.SAMPLE_NUMBER = s.SAMPLE_NUMBER",
    " inner join QC_SAMPLES qcs on qcs.NAME = s.PRODUCT_GRADE",
    " where ENTERED_ON >= '", min_date, "' and ENTERED_ON <= '", max_date, "'",
    " and product in ('QC_VAST', 'QC_WATER')",
    " and sample_name not in ('DUPLO_VAST', 'DUPLO_WATER')",
    " and r.status  in ('E', 'M', 'A')",
    " and r.REPORTABLE = 'T'",
    " ORDER BY qcs.NAME, r.ANALYSIS, r.NAME, r.ENTERED_ON"
  )
  qry <<- qry_res
  cat(qry_res, "\n")
  rv <- dbGetQuery(conn, qry_res) %>%
    arrange(PRODUCT, ANALYSIS, NAME, SAMPLE_NAME, desc(ENTERED_ON)) %>%
    mutate(UID = paste(PRODUCT, ANALYSIS, NAME, SAMPLE_NAME, sep = "---"))
  if (nrow(rv) > n_max) stop(paste0("teveel kandidaatstalen, limiet staat op ", n_max, " resultaten"))
  cat("aantal verschillende groepen:", length(unique(rv$UID)))
  rv
}
#df_qc_results_20    <- qc_select_results(conn, min_date = '2020-01-01', max_date = '2020-12-31')


###



###

# qc_select_qc_samples <- function(conn) {
#
#   qry_candidates <- "
# select
# pg.PRODUCT
# , pg.VERSION
# , pg.SAMPLING_POINT
# , pg.GRADE
# , qcs.QC_TYPE
# , ps.STAGE
# , ps.ANALYSIS
# , ps.COMPONENT
# , ps.MIN_VALUE
# , ps.MAX_VALUE
# , ps.NOMINAL_VALUE
# , ps.LO_CONTROL_1
# , ps.HI_CONTROL_1
# , cp.REPORTABLE
# from PRODUCT_GRADE pg
# inner join QC_SAMPLES qcs on pg.GRADE = qcs.NAME
# inner join VERSIONS v on v.NAME = pg.PRODUCT and v.VERSION = pg.VERSION
# inner join PRODUCT_SPEC ps on ps.GRADE = pg.GRADE
# and ps.VERSION = pg.VERSION
# and ps.PRODUCT = pg.PRODUCT
# inner join
# (	select component.analysis, component.name, component.version, component.reportable
#   from component  inner join versions on component.VERSION = versions.VERSION
#   and component.ANALYSIS = versions.NAME
#   and versions.TABLE_NAME = 'ANALYSIS'
# ) cp  on cp.ANALYSIS = ps.ANALYSIS
# and cp.NAME = ps.COMPONENT
# where pg.product in ('QC_WATER', 'QC_VAST')
# and v.TABLE_NAME = 'PRODUCT'
# and cp.REPORTABLE = 'T'
# "
#
#   qc_samples <- dbGetQuery(conn, qry_candidates)
#   qc_samples
# }

