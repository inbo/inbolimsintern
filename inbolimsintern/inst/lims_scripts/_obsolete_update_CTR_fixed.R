testdata <- list_all_qc_data(conn, '2020-01-01', '2021-01-01') %>%
  group_by(PRODUCT, PRODUCT_VERSION, SAMPLE_NAME, BATCH, ORDER_NUMBER, ANALYSIS, NAME) %>%
  arrange(BATCH, ORDER_NUMBER, ANALYSIS, NAME) %>%
  mutate(NumEntry = as.numeric(ENTRY)) %>%
  summarize(FirstEntry = NumEntry[1]) %>%
  arrange(FirstEntry) %>%
  group_by(PRODUCT, PRODUCT_VERSION, SAMPLE_NAME, ANALYSIS, NAME) %>%
  summarize(N = length(FirstEntry),
            xbar = mean(FirstEntry),
            sd = sd(FirstEntry))

update_queries <- character(nrow(testdata))
for (i in 1:nrow(testdata)) {
  data <- testdata[i, ]
  print(data)
  q = paste0("update PRODUCT_SPEC set ",
             " C_CTR_X = ", data$xbar,
             " ,C_CTR_SD = ", data$sd,
             " ,MIN_VALUE = ", data$xbar - 2 * data$sd,
             " ,MAX_VALUE = ", data$xbar + 2 * data$sd, " WHERE ",
             " PRODUCT = '", data$PRODUCT, "'",
             " and VERSION = ", data$PRODUCT_VERSION, "",
             " and GRADE = '", data$SAMPLE_NAME, "'",
             " and ANALYSIS = '", data$ANALYSIS, "'",
             " and COMPONENT = '", data$NAME, "'")
  update_queries[i] <- q
}

for (i in 1:length(update_queries)) {
  DBI::dbGetQuery(conn, update_queries[i])
}
