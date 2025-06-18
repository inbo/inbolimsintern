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
                    "C_N_ANAL_V_REFS",
                    "C_N_ANAL_V_2022-2024_REFS.sql")
  )
query <- paste(query, collapse = "\n")

data <- dbGetQuery(conn, query)

write_csv2(data,
           file.path("output",
                     "C_N_ANAL_V_2022-2024_REFs.csv")
)
