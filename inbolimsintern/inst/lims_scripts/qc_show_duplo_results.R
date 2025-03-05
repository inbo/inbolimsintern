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
call_id <- 0 #call_id <- 3066 call_id <- 3065 #call_id <- 5720 5721 6572 9733
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

#prefix = paste0(as.numeric(year) - 2000, "-%")
#dupprefix = paste0("D", as.numeric(year) - 2000, "-%")
#dupprefix = "D"
#labprefix = paste0("%", lab, "%")
firstdate <- params %>% filter(ARG_NAME == 'START') %>% pull(VALUE)
lastdate  <-  params %>% filter(ARG_NAME == 'END') %>% pull(VALUE)
lab       <- params %>% filter(ARG_NAME == 'LAB') %>% pull(VALUE)
outtype   <- params %>% filter(ARG_NAME == 'FILE_SEP') %>% pull(VALUE)

qry <- paste0(
"select project = s.PROJECT, matrix = s.C_SAMPLE_MATRIX, product_grade = s.PRODUCT_GRADE, dupnr = s.C_ORIG_DUP_NUMBER ", "\n",
", sampletype = s.SAMPLE_TYPE, textid = s.TEXT_ID, blindparent = s.C_BLIND_PARENT", "\n",
", analysis = r.ANALYSIS, name = r.NAME, value = r.NUMERIC_ENTRY, unit = r.UNITS", "\n",
", date = r.ENTERED_ON, repli = t.REPLICATE_COUNT ", "\n",
" from result r, test t, sample s ", "\n",
" where r.TEST_NUMBER = t.TEST_NUMBER and t.SAMPLE_NUMBER = s.SAMPLE_NUMBER", "\n",
" and s.C_ORIG_DUP_NUMBER in (SELECT C_ORIG_DUP_NUMBER FROM SAMPLE s ", "\n",
                             " where sample_type = 'DUP' and status in ('C', 'A') ", "\n",
                             " and DATE_COMPLETED < '", lastdate, "' and DATE_COMPLETED >= '", firstdate, " ')", "\n",
" and r.REPORTABLE = 'T' and r.NUMERIC_ENTRY is not null ", "\n",
" and r.ENTERED_ON >= '", firstdate, "' and r.ENTERED_ON < '", lastdate, "'",
" and r.STATUS in ('E', 'M', 'A') and s.PRODUCT like '%" , lab, "'", "\n",
" and r.ENTRY_QUALIFIER is NULL",
" order by C_ORIG_DUP_NUMBER, ANALYSIS, NAME"
)

cat(qry, file =  logfile, append = TRUE, sep = "\n")

df_all <- dbGetQuery(conn, qry)
df_all <- df_all %>%
  mutate(sampletype = ifelse(is.na(sampletype), "SAMP", sampletype),
         waarde_ruw = as.numeric(value))
cat("aantal rijen in data: ", nrow(df_all), '\n', file = logfile, sep = "\n")


df_pivot_orig <- df_all %>%
  group_by(dupnr, sampletype, analysis, name, repli ) %>%
  summarise(waarde = mean(waarde_ruw, na.rm = TRUE),
            N = n()) %>%
  arrange(analysis, name, dupnr, repli) %>%
  pivot_wider(id_cols = c(dupnr, analysis, name, repli),
              names_from = sampletype,
              values_from = waarde) %>%
  mutate(ratio = DUP / SAMP,
         afwijking = DUP - SAMP,
         gemiddelde = (DUP + SAMP) / 2,
         relatief = abs(afwijking) / gemiddelde * 100) %>%
  filter(!is.na(DUP) & !is.na(SAMP))

df_pivot_orig <- df_pivot_orig %>%
  inner_join(df_all %>%
               select(textid, unit, blindparent, dupnr, date, analysis, name, repli, sampletype) %>%
               filter(sampletype == "SAMP"),
             by = c('dupnr', 'analysis', 'name', 'repli')) %>%
  inner_join(df_all %>%
               select(textid_dup = textid, dupnr, date_dup = date, analysis, name, repli, sampletype_dup = sampletype) %>%
               filter(sampletype_dup == "DUP"),
             by = c('dupnr', 'analysis', 'name', 'repli'), multiple = "all") %>%
  transmute(dupnr, repli, analysis, name, unit,  textid, textid_dup, blindparent, date, date_dup,
         meting=round(SAMP,digits), duplometing=round(DUP, digits),
         gemiddelde=round(gemiddelde, digits), ratio = round(ratio, digits),
         afwijking = round(afwijking, digits), relatief = round(relatief, digits)
         )

#onderzoekn waarom hier rijen bijkomen
df_pivot <- df_pivot_orig %>%
  ungroup() %>%
  mutate(rownr = 1:n()) %>%
  inner_join(df_all %>%
               select(project, textid, product_grade, matrix) %>%
               group_by(textid) %>%
               summarise(project = paste(unique(project), collapse = ","),
                         product_grade = paste(unique(product_grade), collapse = ","),
                         matrix = paste(unique(matrix), collapse = ",")),
             join_by(textid == textid))

#bereken samenvattende statistieken en voeg die bij de dataset
df_cvsd <- df_pivot %>%
  group_by(analysis, name, unit) %>%
  summarise(N = n(),
            SD = round(sqrt((sum(afwijking^2)) / (2*n())), digits), #2n zie VITO, 2x aantal metingen
            CV = round(100 * sqrt((sum((afwijking / gemiddelde)^2) / (2*n()))),digits-2)) %>%
  transmute(dupnr = -1, repli = N, analysis, name, unit,
            textid = 'ZZZZZZ', textid_dup = 'D-ZZZZZZ-1', blindparent = NA, date_dup = NA,
            meting = NA, duplometing = NA, gemiddelde=NA, ratio=NA, afwijking=NA, relatief=NA,
            sd = SD, cv = CV)

df_pivot_incl_smry <- bind_rows(df_pivot, df_cvsd) %>%
  arrange(analysis, name, textid) %>%
  transmute(Analysis = analysis, Component = name,
            "Text-ID 1" = textid, "Datum 1" = date,
            "Text-ID 2" = textid_dup, "Datum 2" = date_dup,
            Project = project, Matrix = matrix, "Product grade" = product_grade,
            "Meting 1" = meting, "Meting 2" = duplometing,
            "Gemiddelde (x)" = gemiddelde, "Ratio" = ratio,
            "Absoluut verschil" = afwijking,
            "Relatief verschil" = relatief,
            SD = sd, CV = cv)

cat("aantal paarsgewijze testen: ", nrow(df_pivot_incl_smry), '\n', file = logfile, sep = "\n")

Sys.sleep(2)

if (outtype == "FULL") {
  cat("writing csv", sep = "\n", file = logfile, append = TRUE)
  a <- try(write_excel_csv2(df_pivot_incl_smry, file = csvpath, col_names = TRUE, na = ''))
  cat(class(a), sep = "\n", file = logfile, append = TRUE)
} else {
  df_pivot_incl_smry$COMBI <-  interaction(df_pivot_incl_smry$analysis, df_pivot_incl_smry$name, sep = "_")
  for (i in unique(df_pivot_incl_smry$COMBI)) {
    partpath <- substring(csvpath, 1, nchar(csvpath) - 4)
    partpath <- paste0(partpath, "_", i, ".csv")
    write_excel_csv2(df_pivot_incl_smry %>% filter(COMBI == i) %>% select(-COMBI), file = partpath, col_names = TRUE, na = '')
  }
}
Sys.sleep(2)



