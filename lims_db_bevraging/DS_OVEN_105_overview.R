library(inbolimsintern)
library(DBI)

qry <- "
select s.PROJECT, s.TEXT_ID, s.C_VISUAL_MATRIX, r.SAMPLE_NUMBER, TEST_NUMBER, RESULT_NUMBER
, ANALYSIS, NAME, ENTERED_ON, ENTRY, r.STATUS, r.REPLICATE_COUNT
,CASE
WHEN ANALYSIS = 'DS_OVEN_105' AND NAME LIKE 'gew. recip. vol 105°C M%'
THEN CONCAT('gew. recip. M', REPLICATE_COUNT)
ELSE NAME
END AS RESULT_NAME
from RESULT r inner join sample s on r.SAMPLE_NUMBER = s.SAMPLE_NUMBER
where (analysis = 'DS_OVEN_105' or analysis = 'C_N_ANAL_V')
and r.STATUS = 'A'
and r.ENTERED_ON > '2016-01-01'
and (NAME in ('gew. recip. vol', 'gew. recip. leeg') or NAME like 'gew. recip. vol 105°C%' or NAME = 'T.C')
order by r.SAMPLE_NUMBER, r.ANALYSIS, r.NAME
"
params <- read.delim(file = '../dbcredentials.txt', header = FALSE) %>% pull(1)
con <- limsdb_connect(uid = params[2], pwd = params[3])
data <- dbGetQuery(con, qry)

datawide <- data %>%
  filter(!is.na(ENTRY), !is.na(ENTERED_ON)) %>%
  mutate(DATE = as.character(format(ENTERED_ON, "%Y-%m-%d %H:%M:%S")),
         VALUE = as.numeric(str_replace(ENTRY, ",", "."))) %>%
  filter(!is.na(VALUE)) %>%
  group_by(PROJECT, C_VISUAL_MATRIX, TEXT_ID, RESULT_NAME) %>%
  summarise(VALUE = VALUE[1], DATE = DATE[1], .groups = "drop") %>%
  pivot_wider(
    id_cols = c(PROJECT, C_VISUAL_MATRIX, TEXT_ID),
    names_from = RESULT_NAME,
    values_from = c(VALUE, DATE),
    values_fill = NA
  ) %>%
  rename_with(~ gsub("VALUE_", "", .)) %>%
  select(TEXT_ID, PROJECT, C_VISUAL_MATRIX,
         `DATE_gew. recip. leeg`, `gew. recip. leeg`,
         `DATE_gew. recip. vol`, `gew. recip. vol`,
         `DATE_gew. recip. vol 105<b0>C`, `gew. recip. vol 105<b0>C`,
         `DATE_gew. recip. M1`,`gew. recip. M1`,
         `DATE_gew. recip. M2`,`gew. recip. M2`,
         `DATE_gew. recip. M3`,`gew. recip. M3`,
         `DATE_gew. recip. M4`,`gew. recip. M4`,
         `DATE_gew. recip. M5`,`gew. recip. M5`,
         T.C) %>%
  arrange(PROJECT, TEXT_ID)

write_excel_csv2(datawide, "Q:\\_PIETER\\DS105_overzicht.csv", na = "")
