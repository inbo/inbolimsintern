
library(tidyverse)
library(googlesheets4)
library(inbolimsintern)

key <- "1K0OFf144hJHbhL2zlfAODaypNKzACr8AeUA4dpg1t1Y"

df <- read_sheet(key) %>% filter(!is.na(Sample_number))
connectlist <- read_db_credentials()
conn <- limsdb_connect(uid=connectlist$uid, pwd = connectlist$pwd)

q = paste0("select sample_number, sample_id from sample where sample_number in (", paste(df %>% pull(Sample_number), collapse = ",") , ")")
sids <- dbGetQuery(conn, q)

df_export <- df %>% left_join(sids, by = c("Sample_number" = "sample_number"))
write_excel_csv2(df_export, "export_including_sampleid.csv")
