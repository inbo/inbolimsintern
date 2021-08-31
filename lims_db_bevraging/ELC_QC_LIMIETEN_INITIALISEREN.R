library(googlesheets4)
library(magrittr)

#connecteren met databank
connectlist <- read_db_credentials()
conn <- limsdb_connect(uid = connectlist$uid, pwd = connectlist$pwd)

#Input GSHEET voor water
ss <- "https://docs.google.com/spreadsheets/d/1A4OeK672kG3Wh3ixjaZcaYMV1oGjg-14PqQEmAYUV2s/edit#gid=362776876"
dfLimW <- googlesheets4::read_sheet(ss,
                                    sheet = 'QC_Water',
                                    range = 'A4:H500',
                                    col_types = "cccnnnnn",
                                    col_names = FALSE) %>%
  filter(!is.na(.[[1]]))
colnames(dfLimW) <- c("ANALYSIS", "COMPONENT", "GRADE", "AVG", "SD", "CV", "MIN", "MAX")
str(dfLimW)

### Update C_CTR_X, C_CTR_SD, C_CERTIFIED_VALUE, MIN_VALUE, MAX_VALUE

upd_queries <- list()
for (i in 1:nrow(dfLimW)) {
  df <- dfLimW %>% filter(row_number() == i)
  analyse <- df %>% pull(ANALYSIS)
  comp <- df %>% pull(COMPONENT)
  grade <- df %>% pull(GRADE)
  avg <- df %>% pull(AVG)
  mnm <- df %>% pull(MIN)
  mxm <- df %>% pull(MAX)
  if(is.na(avg)) {
    avg <- sd <- cv <- "null"
  } else {
    sd <- df %>% pull(SD)
    cv <- df %>% pull(CV)
    if(is.na(cv)) cv <- "null"
  }
  if(is.na(mnm)) mnm <- "null"
  if(is.na(mxm)) mxm <- "null"

  qry <- paste0(
    " update PRODUCT_SPEC set",
    " C_CTR_X = ", avg, ",",
    " C_CTR_SD = ", sd, ",",
    " C_CERTIFIED_VALUE = ", cv, ",",
    " MIN_VALUE = ", mnm, ",",
    " MAX_VALUE = ", mxm,
    " \nwhere ",
    " ANALYSIS = '", analyse, "'",
    " AND COMPONENT = '", comp, "'",
    " AND GRADE = '", grade, "'")
  upd_queries[i] <- qry
}

for (i in 1:length(upd_queries)) {
  print(i)
  dbGetQuery(conn, upd_queries[[i]])
}




