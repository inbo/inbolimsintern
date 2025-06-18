library(tidyverse)
library(odbc)
library(inbolimsintern)

creds <- read_csv2("dbcredentials.txt", col_names = FALSE)
conn <- limsdb_connect(uid = creds[[1]][2], pwd = creds[[1]][3])

query <-
  readLines(
    con = file.path("inbolimsintern",
                    "inst",
                    "data_queries",
                    "LOQ overzicht analytisch labo.sql")
  )
query <- paste(query, collapse = "\n")

data <- dbGetQuery(conn, query)

write_csv2(data,
           file.path("output",
                     "LOQ_overzicht_analytisch_labo.csv")
)


data_agg <- data %>%
  group_by(ANALYSIS, NAME) %>%
  summarize(MIN_LOQ = min(CLAMP_LOW_CLEAN, na.rm = TRUE),
            MAX_LOQ = max(CLAMP_LOW_CLEAN,  na.rm = TRUE),
            DIFF = MAX_LOQ != MIN_LOQ)
data_agg_diff <- data_agg %>% filter(DIFF == 1)



ggplot(data %>% filter(ANALYSIS %in% data_agg_diff$ANALYSIS,
                       NAME %in% data_agg_diff$NAME),
       aes(x = VERSION, y = LOQ)) +
  geom_line() +
  ggforce::facet_wrap_paginate(~ANALYSIS + NAME, nrow = 6, ncol = 4)


