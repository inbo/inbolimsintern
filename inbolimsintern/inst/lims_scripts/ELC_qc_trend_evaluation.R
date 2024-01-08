#ELC - Evaluatie QC charts over the years

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "ELC_qc_eval")
writeLines(con = logfile, paste0("ELC_Evaluatie\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id = 6661 7107  7108
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

### inlezen gegevens

qcproduct <- params %>% filter(ARG_NAME == "PRODUCT") %>% pull(VALUE)
lastyear <- params %>% filter(ARG_NAME == "LAST_YEAR") %>% pull(VALUE) %>% as.numeric()
outputfile <- params %>% filter(ARG_NAME == "OUTPUT_FILE")  %>% pull(VALUE)
ts <- paste0("{ts '", lastyear - 2, "-01-01 00:00:00'}")

query <- paste0("
select s.PRODUCT, s.SAMPLE_NUMBER, s.TEXT_ID, s.SAMPLE_TYPE, s.SAMPLE_NAME , bo.BATCH, bo.ORDER_NUMBER
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
firstdata <- alldata %>%
  semi_join(firstselect, by = c("ROW" = "KEPT_ROW"))


#de laatste productversie in de data wordt gekozen als basislimieten
prodversion <- max(firstdata$VERSION)

qry2 <- paste0("
select PRODUCT, ps.VERSION, ps.ANALYSIS, ps.COMPONENT, GRADE",
      " , C_CERTIFIED_VALUE, C_CERTIFIED_SD",
      " , C_CTR_X, C_CTR_SD, C_CTR_ADD",
      " , DET_LIMIT = SUBSTRING(CLAMP_LOW, 2, LEN(CLAMP_LOW) - 1)",
      " , CASE WHEN CHARINDEX('PBL', grade) > 0 THEN 1 ELSE 0 END AS IS_PBL",
" from PRODUCT_SPEC ps",
" inner join COMPONENT c on ps.ANALYSIS = c.ANALYSIS and ps.COMPONENT = c.NAME",
" inner join VERSIONS v on c.VERSION = v.VERSION and v.NAME = c.ANALYSIS and v.TABLE_NAME = 'ANALYSIS'",
" where PRODUCT = '", qcproduct,  "' and ps.VERSION = ", prodversion)

productinfo <- dbGetQuery(conn, qry2)

# test1 <- firstdata %>%
#   group_by(BATCH, ANALYSIS) %>%
#   summarise(DATE = min(C_DATE_BATCHRUN))
# ggplot(data = test1, aes(x = DATE)) + facet_wrap(~ANALYSIS) + geom_histogram(binwidth = 30 * 24 * 3600)
# firstdata %>% group_by(COMBI, period) %>% summarise(aantal = n()) %>%
#   pivot_wider(names_from = period, values_from = aantal, values_fill = 0 ) %>% view()

combis <- unique(firstdata$COMBI)

#headerAna <- NA
#headerComp <- NA

overzicht <- NULL
for (i in combis) {
  comb <- i
  print(comb)
  comps <- unlist(str_split(comb,pattern =  "__"))
  analyse <- comps[1]
  component <- comps[2]
  qcsample <- comps[3]
  prd <- productinfo %>% filter(PRODUCT == qcproduct, ANALYSIS == analyse, COMPONENT == component, GRADE == qcsample)
  if (nrow(prd)) {
    x_huidig <- prd[1, "C_CTR_X"]
    sd_huidig <- prd[1, "C_CTR_SD"]
    ref_x_huidig <- prd[1, "C_CERTIFIED_VALUE"]
    ref_sd_huidig <- prd[1, "C_CERTIFIED_SD"]
    qc_chart <- prd[1, "C_CTR_ADD"]

    minlim <- x_huidig - 3 * sd_huidig
    maxlim <- x_huidig * 3 * sd_huidig
    detlim <- prd[1, "DET_LIMIT"]
    is_pbl <- prd[1, "IS_PBL"]
  } else {
    x_huidig <- sd_huidig <- ref_x_huidig <- ref_sd_huidig <- qc_chart <- minlim <- maxlim <- is_pbl <- detlim <- NA
  }


  fdata <- firstdata %>% filter(COMBI == comb) %>% arrange(C_DATE_BATCHRUN) %>% mutate(BATCHNR = 1:n(), VALUE = as.numeric(ENTRY))
  table(fdata$period)
  # ggplot(fdata, aes(x = C_DATE_BATCHRUN, y = VALUE)) +
  #   geom_point() +
  #   geom_smooth() +
  #   geom_line(aes(y = C_CTR_X), color = "green4") +
  #   geom_line(aes(y = C_CTR_X + 3 * C_CTR_SD), color = "red") +
  #   geom_line(aes(y = C_CTR_X - 3 * C_CTR_SD), color = "red")

  periods <- fdata$period
  values1 <- fdata$VALUE[fdata$period == 1]
  values2 <- fdata$VALUE[fdata$period == 2]
  values3 <- fdata$VALUE[fdata$period == 3]
  if(!is.na(minlim)){
    values1 <- na.omit(values1[values1 >= minlim & values1 <= maxlim])
    values2 <- na.omit(values2[values2 >= minlim & values2 <= maxlim])
    values3 <- na.omit(values3[values3 >= minlim & values3 <= maxlim])
  }
  len1 <- length(values1)
  len2 <- length(values2)
  len3 <- length(values3)

  do_tests <- TRUE
  if (len3 >= 30) {
    allvalues <- c(values2, values3)
  } else if (len3 < 30) {
    allvalues <- c(values2, values3)
    if (length(allvalues) < 30) {
      allvalues <- c(values1, values2, values3)
    }
    if (length(allvalues) < 30) {
      do_tests <- FALSE
    }
    lenall <- length(allvalues)
  }

  lenall <- length(allvalues)
  split <- lenall %/% 2
  valset1 <- allvalues[1:split]
  valset2 <- allvalues[(split+1):lenall]
  n1 <- length(valset1)
  n2 <- length(valset2)
  m1 <- mean(valset1)
  m2 <- mean(valset2)
  sd1 <- sd(valset1)
  sd2 <- sd(valset2)

  if (do_tests) {
    tobj <- t.test(valset1, valset2)
    vobj <- var.test(valset1, valset2)
  } else {
    tobj <- list(estimate = NA, p.value = NA)
    vobj <- list(estimate = NA, p.value = NA)
  }
  t_test_p <- tobj$p.value
  var_test_p <- vobj$p.value
  if (is.na(is_pbl)) {
    x_voorstel <- sd_voorstel <-  lcl_voorstel <-  ucl_voorstel <-  ucl_voorstel <-  NA
  }
  else if (is_pbl) {
    x_voorstel <- 0
    sd_voorstel <- as.numeric(detlim)/2
    lcl_voorstel <- x_voorstel - 2 * sd_voorstel
    ucl_voorstel <-  x_voorstel + 2 * sd_voorstel

  } else {
    x_voorstel <- ifelse(t_test_p < 0.01 & !is.na(t_test_p), m2, x_huidig)
    sd_voorstel <- ifelse(var_test_p < 0.01 & !is.na(var_test_p), sd2, sd_huidig)
    lcl_voorstel <- x_voorstel - 3 * sd_voorstel
    ucl_voorstel <-  x_voorstel + 3 * sd_voorstel
  }


  cs <- unlist(str_split(comb, pattern = "__"))

  rv <- data.frame(product = qcproduct,
                   referentie = cs[3],
                   analyse = cs[1],
                   component = cs[2],
                   ref_x_huidig = ref_x_huidig,
                   ref_sd_huidig = ref_sd_huidig,
                   x_huidig = x_huidig,
                   sd_huidig = sd_huidig,
                   n_laatst = len3,
                   x_laatst = mean(values3),
                   sd_laatst = sd(values3),
                   n_tot = n1 + n2,
                   n_set1 = n1,
                   n_set2 = n2,
                   x_set1 = m1,
                   x_set2 = m2,
                   sd_set1 = sd1,
                   sd_set2 = sd2,
                   t_test_p = t_test_p,
                   var_test_p = var_test_p,
                   significant = paste0(ifelse(t_test_p < 0.01, "*", "_"), ifelse(var_test_p < 0.01, "*", "_")),
                   is_pbl = is_pbl,
                   qc_chart = qc_chart,
                   x_voorstel = x_voorstel,
                   sd_voorstel = sd_voorstel,
                   lcl_voorstel = lcl_voorstel,
                   ucl_voorstel = ucl_voorstel
                   )

  overzicht <- bind_rows(overzicht, rv)
}

write_excel_csv2(overzicht, file = outputfile)



