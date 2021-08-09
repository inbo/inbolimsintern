#TLC - Toon Duplo Resultaten van het hele gekozen lopende jaar

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)
library(magrittr)

### Logfile
logfile <- logfile_start(prefix = "TLC_Duplo_Overview")
writeLines(con = logfile, paste0("TLC_Duplo_Overview\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 3066 call_id <- 3065
digits <- 5
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

try({
  lab  <- try(filter(params, ARG_NAME == "LAB") %>% pull(VALUE))
  csvpath <- try(filter(params, ARG_NAME == "CSV_FILE") %>% pull(VALUE))
  year <- try(filter(params, ARG_NAME == "YEAR") %>% pull(VALUE))
}, outFile = logfile)

## Data

prefix = paste0(as.numeric(year) - 2000, "-%")
dupprefix = paste0("D", as.numeric(year) - 2000, "-%")
labprefix = paste0("%", lab, "%")

qry = paste0("
select
project = s.PROJECT, samplenr = s.SAMPLE_NUMBER, matrix = s.C_SAMPLE_MATRIX
, textid = s.TEXT_ID, sampletype = s.SAMPLE_TYPE
, dupnr = s.C_ORIG_DUP_NUMBER, sstatus = s.STATUS
, analysis = r.ANALYSIS, name = r.NAME, r.ENTRY, rstatus = r.STATUS
from sample s
inner join result r on r.SAMPLE_NUMBER = s.SAMPLE_NUMBER
where 1 = 1
and r.REPORTABLE = 'T'
and r.STATUS in ('E', 'M', 'A')
and s.STATUS in ('C', 'A')
and C_ORIG_DUP_NUMBER > 0
and (text_id like '",
dupprefix, "' or text_id like '", prefix, "')",
" and (PRODUCT like '", labprefix, "')",
" order by C_ORIG_DUP_NUMBER")

dfAll <- dbGetQuery(conn, qry)
dfAll %<>%
  mutate(sample_type = ifelse(is.na(sampletype), "SAMP", sampletype),
         ENTRY = ifelse(ENTRY %in% ('Y'), 1, ENTRY),
         ENTRY = ifelse(ENTRY %in% ('N'), 0, ENTRY)) %>%
  mutate(waarde_ruw = as.numeric(ENTRY))

dfPivot <- dfAll %>%
  group_by(dupnr, sample_type, analysis, name) %>%
  summarise(waarde = mean(waarde_ruw, na.rm = TRUE),
            N = n()) %>%
  arrange(analysis, name, dupnr) %>%
  pivot_wider(id_cols = c(dupnr, analysis, name),
              names_from = sample_type,
              values_from = waarde) %>%
  mutate(RATIO = DUP / SAMP,
         DIFF = DUP - SAMP,
         MEAN = mean(c(DUP, SAMP)),
         PCT_DVG = abs(DIFF) / MEAN * 100)

dfOrigSamps <- dfAll %>%
  group_by(dupnr) %>%
  do({
    first_samp = min(.$samplenr)
    filter(., samplenr == first_samp) %>%
      mutate(aantal = nrow(.))
  })


dfReturn <- dfPivot %>%
  ungroup() %>%
  inner_join(dfOrigSamps %>% select( project, textid, aantal, dupnr),
             by = "dupnr") %>%
  transmute(project, textid, analysis, name,
            DUP = round(DUP, digits),
            SAMP = round(SAMP, digits),
            RATIO = round(RATIO, digits),
            DIFF = round(DIFF, digits),
            PCT_DVG = round(PCT_DVG, digits)) %>%
  arrange(analysis, name, project, textid) %>%
  filter(!duplicated(.))

write_excel_csv2(dfReturn, path = csvpath, col_names = TRUE)


