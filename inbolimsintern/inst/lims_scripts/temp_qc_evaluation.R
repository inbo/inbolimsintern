sql_product_spec <- "
select ps.PRODUCT, ps.VERSION, ps.SAMPLING_POINT, ps.GRADE, ps.STAGE,
ps.ANALYSIS, ps.COMPONENT, ps.NOMINAL_VALUE, ps.C_SD,
c.C_QC, qc.C_QC_CTR_CHART
from PRODUCT_SPEC ps
inner join QC_SAMPLES qc on qc.NAME = ps.GRADE
inner join COMPONENT c on c.NAME = ps.COMPONENT and c.ANALYSIS = ps.ANALYSIS
inner join VERSIONS v on
	v.TABLE_NAME = 'ANALYSIS'
	and c.ANALYSIS = v.NAME
	and v.VERSION = c.VERSION
inner join VERSIONS v2 on
	v2.TABLE_NAME = 'PRODUCT'
	and ps.PRODUCT = v2.NAME
	and v2.VERSION = ps.VERSION
where PRODUCT in ('QC_WATER', 'QC_VAST')
"

min_date <- '2020-01-01'
max_date <- '2021-01-01'
sql_result <- paste0("
select  s.TEXT_ID, t.BATCH, bo.ORDER_NUMBER, s.PRODUCT, s.PRODUCT_GRADE,
r.ANALYSIS, r.NAME, r.ENTRY
from result r
inner join test t on t.TEST_NUMBER = r.TEST_NUMBER
inner join batch_objects bo on bo.OBJECT_ID = t.TEST_NUMBER
inner join sample s on t.SAMPLE_NUMBER = s.SAMPLE_NUMBER
where s.PRODUCT in ('QC_VAST', 'QC_WATER')
and r.ENTERED_ON > '",
min_date, "' and r.ENTERED_ON < '", max_date, "'",
"and r.STATUS = 'A' and s.PRODUCT_GRADE <> 'DUP'
")

call_id <- 500 #random
args <- inbolimsintern::prepare_session(call_id)
conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])

qc_specs <- DBI::dbGetQuery(conn, sql_product_spec)
qc_results <- DBI::dbGetQuery(conn, sql_result) %>%
  arrange(PRODUCT_GRADE, ANALYSIS, NAME, BATCH, ORDER_NUMBER) %>%
  mutate(NumENTRY = as.numeric(ENTRY))
qc_results$dup <- duplicated(qc_results[,c('PRODUCT_GRADE', 'ANALYSIS', 'NAME', 'BATCH')])

qc_results_red <- qc_results %>% filter(!dup)

qc_results_mean <- qc_results_red %>%
  group_by(PRODUCT, PRODUCT_GRADE, ANALYSIS, NAME) %>%
  summarize(gemiddelde = mean(NumENTRY, na.rm = TRUE),
            sd = sd(NumENTRY, na.rm = TRUE),
            aantal = sum(!is.na(NumENTRY))) %>%
  mutate(lcl3s = gemiddelde - 3 * sd,
         ucl3s = gemiddelde + 3 * sd)

qc_bind <- qc_specs %>%
  left_join(qc_results_mean,
            by = c("PRODUCT", GRADE = "PRODUCT_GRADE",
                   "ANALYSIS", COMPONENT='NAME')) %>%
  filter(aantal >= 10)

queries <- character(nrow(qc_bind))
for (i in 1:nrow(qc_bind)) {
  data <- slice(qc_bind, i)
  if (!is.na(data$gemiddelde)) {
    queries[i] <- paste0("
    update product_spec set ",
        "NOMINAL_VALUE = ", data$gemiddelde,
        ", C_SD = ", data$sd,
        ", LSL = ", data$lcl3s,
        ", USL = ", data$ucl3s,
    " where product = '", data$PRODUCT, "'",
    " and version = ", data$VERSION,
    " and grade = '", data$GRADE, "'",
    " and stage = '", data$STAGE, "'",
    " and sampling_point = '", data$SAMPLING_POINT, "'",
    " and analysis = '", data$ANALYSIS, "'",
    " and component = '", data$COMPONENT, "'")
  }
}

execute_queries <- FALSE #execute_queries <- TRUE
for (i in 1:length(queries)) {
  if (is.na(queries[i]) & execute_queries) next
  cat('.')
    DBI::dbGetQuery(conn, queries[i])
}


###Analyses op het einde van het jaar
  #Bereken de lineaire trend
  #splits data in 2 delen en doe een t-test voor gemiddelde en F test voor stdev

elc_t_and_var_test <- function(x){
  print(x)
  len <- length(x)
  split <- round(len/2)
  g1 <- x[1:split]
  g2 <- x[(split+1):len]

  result_t <- try(t.test(g1, g2, var.equal = FALSE), silent = TRUE)
  if (class(result_t)[1] == "try-error") {
    result_t_df <- data.frame(estimate = NA, statistic = NA, p.value = NA)
  } else {
    result_t_df <- broom::tidy(result_t)
  }

  result_var <- try(var.test(g1, g2), silent = TRUE)
  print(class(result_var))
  if (class(result_var)[1] == "try-error") {
    result_var_df <- data.frame(estimate = NA, statistic = NA, p.value = NA)
  } else {
    result_var_df <- broom::tidy(result_var)
  }

  print(cbind(result_t_df, result_var_df))
  rv <-
    tibble(t_diff = result_t_df$estimate,
           t_stat = result_t_df$statistic,
           t_pval = result_t_df$p.value,
           var_ratio = result_var_df$estimate,
           var_stat = result_var_df$statistic,
           var_pval = result_var_df$p.value)
  print(rv)
  rv
}

qc_tests <- qc_results_red %>%
  mutate(combi = paste(PRODUCT, PRODUCT_GRADE, ANALYSIS, NAME, sep= "---" )) %>%
  nest_by(combi) %>%
  filter(nrow(data) >= 30)

qc_tests_result <- qc_tests %>%
  mutate(nrw = nrow(data),
         tests = elc_t_and_var_test(data$NumENTRY))





