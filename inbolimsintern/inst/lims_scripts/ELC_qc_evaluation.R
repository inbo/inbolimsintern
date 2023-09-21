#ELC - Evaluatie QC charts over the years

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "ELC_qc_eval")
writeLines(con = logfile, paste0("ELC_Evaluatie\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

### inlezen gegevens

lastyear <- 2023
qcproduct <- "QC_WATER"
ts <- paste0("{ts '", lastyear - 2, "-01-01 00:00:00'}")

query <- paste0("
select s.SAMPLE_NUMBER, s.TEXT_ID, s.SAMPLE_TYPE, s.SAMPLE_NAME , bo.BATCH, bo.ORDER_NUMBER
, r.ANALYSIS, r.NAME, r.ENTRY, r.ENTERED_ON, r.UNITS
, COMBI = CONCAT(r.ANALYSIS,'__', r.NAME, '__', s.SAMPLE_NAME)
, ps.C_CERTIFIED_VALUE, ps.C_CERTIFIED_SD, ps.C_CTR_X, ps.C_CTR_SD, ps.VERSION, b.C_DATE_BATCHRUN
from result r
inner join test t on t.TEST_NUMBER = r.TEST_NUMBER
inner join sample s on s.SAMPLE_NUMBER = t.SAMPLE_NUMBER
inner join qc_samples qcs on qcs.NAME = s.SAMPLE_NAME
inner join batch_objects bo on bo.OBJECT_ID = t.TEST_NUMBER
inner join batch b on b.NAME = bo.BATCH
inner join product_spec ps on ps.PRODUCT = s.PRODUCT and ps.VERSION = s.PRODUCT_VERSION and s.PRODUCT_GRADE = ps.GRADE and ps.ANALYSIS = t.ANALYSIS and ps.COMPONENT = r.NAME
and r.ENTRY is not null and s.SAMPLE_TYPE is not null and s.SAMPLE_TYPE <> 'DUP' ",
" and r.STATUS in ('E', 'M', 'A')",
" and ps.C_CTR_ADD = 'T'",
" and b.C_DATE_BATCHRUN > ", ts,
" and s.PRODUCT = '", qcproduct, "'",
" order by r.ANALYSIS, r.NAME, s.SAMPLE_NAME, bo.BATCH, bo.ORDER_NUMBER")

alldata <- dbGetQuery(conn, query) %>% mutate(ROW = 1:n(), period = as.numeric(substring(ENTERED_ON,1,4)) - (lastyear-3))

### selecteer enkel de eerste data in een batch

firstselect <- alldata %>%
  group_by(ANALYSIS, NAME, SAMPLE_NAME, BATCH) %>%
  summarise(KEPT_ROW = min(ROW))
firstdata <- alldata %>% semi_join(firstselect, by = c("ROW" = "KEPT_ROW"))


test1 <- firstdata %>%
  group_by(BATCH, ANALYSIS) %>%
  summarise(DATE = min(C_DATE_BATCHRUN))
ggplot(data = test1, aes(x = DATE)) + facet_wrap(~ANALYSIS) + geom_histogram(binwidth = 30 * 24 * 3600)


firstdata %>% group_by(COMBI, period) %>% summarise(aantal = n()) %>%
  pivot_wider(names_from = period, values_from = aantal, values_fill = 0 ) %>% view()

combis <- unique(firstdata$COMBI)


  comb <- combis[54]
  print(comb)
  fdata <- firstdata %>% filter(COMBI == comb) %>% arrange(C_DATE_BATCHRUN) %>% mutate(BATCHNR = 1:n(), VALUE = as.numeric(ENTRY))
  table(fdata$period)
  ggplot(fdata, aes(x = C_DATE_BATCHRUN, y = VALUE)) +
    geom_point() +
    geom_smooth() +
    geom_line(aes(y = C_CTR_X), color = "green4") +
    geom_line(aes(y = C_CTR_X + 3 * C_CTR_SD), color = "red") +
    geom_line(aes(y = C_CTR_X - 3 * C_CTR_SD), color = "red")

  periods <- fdata$period
  values <- fdata$VALUE
  values1 <- values[periods == 1]
  values2 <- values[periods == 2]
  values3 <- values[periods == 3]

  if (length(values2)>= 10 & length(values3)>10) {
    print("ok")
    tobj <- t.test(fdata$VALUE[fdata$period == 2], fdata$VALUE[fdata$period == 3])
    vobj <- var.test(fdata$VALUE[fdata$period == 2], fdata$VALUE[fdata$period == 3])
  } else if (length(values) > 20) {
    print("limited")
    len <- length(values)
    len1 <- floor(len/2)
    len2 <- len - len1
    v1 <- values[1:len1]
    v2 <- values[(len1+1):len]
    tobj <-t.test(v1, v2)
    vobj <- var.test(v1, v2)
  } else {
    tobj <- list(estimate = NA, p.value = NA)
    vobj <- list(estimate = NA, p.value = NA)
  }
  print(tobj)
  print(vobj)


