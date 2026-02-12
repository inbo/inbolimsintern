library(inbolimsintern)
library(tidyverse)
library(DBI)
library(googlesheets4)
library(glue)


qry <- "select  r.analysis, r.name, r.UNITS, product = s.PRODUCT,
minDate = min(ENTERED_ON), maxDate = max(ENTERED_ON), aantal = count(entry),
minVersion = min(t.VERSION), maxVersion = max(t.VERSION)
from result r
inner join test t on t.test_number = r.test_number
inner join sample s on t.sample_number = s.sample_number
where s.product in ('VAST', 'WATER') and r.reportable = 'T' and r.status in ('A')
group by r.analysis, r.name, r.UNITS , s.PRODUCT
order by r.analysis, r.name, r.units
"

creds <- inbolimsintern::read_db_credentials()
conn <- limsdb_connect(uid = creds$uid, pwd = creds$pwd)


### Create component overview
#-------------------------------

component_overview <- dbGetQuery(conn, qry)

### Solve non-reportables
#-------------------------

listing <- read_sheet(ss = "1mmSeNNarfr1eRWSxVHusnS2bd3Xm3KfbDmJUn-JHsp4",
                      sheet = "overzicht_reportable_results")

listing_nonrp <- listing |> filter(`reportable New` == "F")
qry_rp <- list()
for (i in seq_len(nrow(listing_nonrp))) {
  analyse  <- listing_nonrp |> slice(i) |> pull(analysis)
  component <- listing_nonrp |> slice(i) |> pull(component)
  qry_rp[[i]] <- glue("update result set reportable = 'F'",
                      " where analysis = '{analyse}' and name = '{component}'",
                      " and reportable = 'T'")
}
#run query: sapply(qry_rp, function(q) dbGetQuery(conn, q))

### Fill in generic components

qry_gc <- list()
for (i in seq_len(nrow(listing))) {
  analyse  <- listing |> slice(i) |> pull(analysis)
  component <- listing |> slice(i) |> pull(component)
  genericname <- listing |> slice(i) |> pull(`Algemene component`)
  qry_gc[[i]] <-
    glue("update component set C_GENERAL_COMPONENT = '{genericname}'",
         " where analysis = '{analyse}' and name = '{component}'")
}
#run query: sapply(qry_gc, function(q) dbGetQuery(conn, q))
