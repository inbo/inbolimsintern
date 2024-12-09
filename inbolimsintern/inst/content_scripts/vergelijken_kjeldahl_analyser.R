library(tidyverse)
library(DBI)

sql <-
  paste(
  readLines(
    file.path("inbolimsintern",
              "inst",
              "sql_queries",
              "vergelijken_kjeldahl_analyser.sql")),
  collapse = "\n")

sql <- "
WITH paired_samples AS (
    SELECT s.ORIGINAL_SAMPLE
    FROM result r
    INNER JOIN sample s ON r.sample_number = s.sample_number
    WHERE r.status = 'A' AND s.status = 'A' AND r.ENTRY IS NOT NULL
    AND ((r.analysis IN ('C_N_ANAL_V', 'N_TOT_ANAL_V', 'N_TOT_TIT_V') AND r.NAME = 'T.N.DS')
        OR (r.analysis = 'N_KJEL_TIT' AND r.name = 'N.DS'))
    GROUP BY s.ORIGINAL_SAMPLE
    HAVING COUNT(DISTINCT CASE
        WHEN r.ANALYSIS IN ('C_N_ANAL_V', 'N_TOT_ANAL_V') THEN 'analyser'
        WHEN r.ANALYSIS IN ('N_KJEL_TIT', 'N_TOT_TIT_V') THEN 'Kjeldahl'
    END) = 2
)
SELECT DISTINCT
    s.PROJECT,
    s.ORIGINAL_SAMPLE,
    (SELECT TOP 1 TEXT_ID FROM sample WHERE ORIGINAL_SAMPLE = s.ORIGINAL_SAMPLE AND status = 'A') as SAMPLE_TEXT_ID,
    r.ANALYSIS,
    r.NAME,
    r.ENTRY,
    r.ENTERED_ON,
    CASE
        WHEN r.ANALYSIS IN ('C_N_ANAL_V', 'N_TOT_ANAL_V') THEN 'analyser'
        WHEN r.ANALYSIS IN ('N_KJEL_TIT', 'N_TOT_TIT_V') THEN 'Kjeldahl'
    END as METHOD_TYPE
FROM result r
INNER JOIN sample s ON r.sample_number = s.sample_number
WHERE r.status = 'A' AND s.status = 'A' AND r.ENTRY IS NOT NULL
AND (
    (s.ORIGINAL_SAMPLE IN (SELECT ORIGINAL_SAMPLE FROM paired_samples)
    AND ((r.analysis IN ('C_N_ANAL_V', 'N_TOT_ANAL_V', 'N_TOT_TIT_V') AND r.NAME = 'T.N.DS')
    OR (r.analysis = 'N_KJEL_TIT' AND r.name = 'N.DS')))
    OR
    (s.ORIGINAL_SAMPLE IN (SELECT ORIGINAL_SAMPLE FROM paired_samples)
    AND r.analysis = 'N_MIN_V'
    AND r.name IN ('NH4.N.DS', 'NO2.N.DS', 'NO3.N.DS', 'T.MIN.N.DS'))
)
ORDER BY s.ORIGINAL_SAMPLE, r.ANALYSIS, r.NAME"

connectlist <- inbolimsintern::read_db_credentials()
conn <- inbolimsintern::limsdb_connect(uid=connectlist$uid,
                                       pwd = connectlist$pwd)
dataN <- dbGetQuery(conn, sql)
dataN <- dataN |>
  mutate(analysis = paste(ANALYSIS, NAME, METHOD_TYPE, sep = "__"))

dataN_wide <- dataN |>
  select(PROJECT, ORIGINAL_SAMPLE, SAMPLE_TEXT_ID, analysis, ENTRY) |>
  pivot_wider(values_from = ENTRY, names_from = analysis, values_fn = ~.[1])


write_excel_csv2(dataN, 'vergelijken_kjeldahl_analyser_long_format.csv')
write_excel_csv2(dataN_wide, 'vergelijken_kjeldahl_analyser_xtab_format.csv')
