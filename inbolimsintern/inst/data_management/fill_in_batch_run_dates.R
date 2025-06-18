
library(inbolimsintern)
library(DBI)

qry <- "
with tmp as (
  select NAME, DATE_CREATED, C_DATE_BATCHRUN, BATCH_STATUS
  from batch b  inner join test t on t.BATCH = b.name
  where DATE_CREATED > '2019-01-01' and  DATE_CREATED < '2022-12-31'
  and BATCH_STATUS <> 'X'
  and C_DATE_BATCHRUN is null
  and b.GROUP_NAME <> 'GENETICA'
)
select tmp.NAME, tmp.BATCH_STATUS,  tmp.DATE_CREATED, tmp.C_DATE_BATCHRUN, maxd = max(r.ENTERED_ON)
from tmp
inner join test t on tmp.NAME = t.BATCH
inner join result r on r.TEST_NUMBER = t.TEST_NUMBER
group by tmp.NAME, tmp.BATCH_STATUS,  tmp.DATE_CREATED, tmp.C_DATE_BATCHRUN
"

params <- read.delim(file = '../dbcredentials.txt', header = FALSE) %>% pull(1)
con <- limsdb_connect(uid = params[2], pwd = params[3])

batches <- read.delim(file = 'D:/Labo_FS/_PIETER/BATCHRUNDATES.csv', na.strings = "NULL")
batches <- batches %>%
  mutate(C_DATE_BATCHRUN = maxd) %>%
  filter(!is.na(C_DATE_BATCHRUN))
updqry <- NULL
for(i in 1:nrow(batches)) {
  updqry[i] <- paste0("update batch set C_DATE_BATCHRUN = '", batches$C_DATE_BATCHRUN[i],
                      "' where  NAME = '", batches$NAME[i], "';")
}
updqries <- paste(updqry, collapse = "\n")
cat(updqries)
