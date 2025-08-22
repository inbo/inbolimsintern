###############################################################
#TLC - Toon Duplo Resultaten van het hele gekozen lopende jaar (of periode)
###############################################################

#// setup environment
##=====================

library(tidyverse)
library(DBI)
library(inbolimsintern)
digits <- 5
args <- commandArgs(trailingOnly = TRUE)
setup <- try(r_session_setup(args))
#args <- c("LWL8DEV", "10045", "TEST_INT"); setup <- try(r_session_setup(args, test_mode = TRUE))
invisible(list2env(setup, envir = .GlobalEnv))

if (inherits(setup, "try-error")) {
  write_db_log(paste("Problem setting up R script", setup), "E")
  stop("probleem bij setup script")
} else {
  write_db_log("R session setup finished", "P")
}

#// Import arguments
##======================

e <- try({
  lab  <- try(filter(params, ARG_NAME == "LAB") %>% pull(VALUE))
  csvpath <- try(filter(params, ARG_NAME == "CSV_FILE") %>% pull(VALUE))
  year <- try(filter(params, ARG_NAME == "YEAR") %>% pull(VALUE))
  firstdate <- params %>% filter(ARG_NAME == 'START') %>% pull(VALUE)
  lastdate  <-  params %>% filter(ARG_NAME == 'END') %>% pull(VALUE)
  lab       <- params %>% filter(ARG_NAME == 'LAB') %>% pull(VALUE)
})
if (inherits(e, "try-error")) {
  write_db_log(paste("error retrieving params:", e), "E")
  stop(e)
} else {
  write_db_log("parameters ingelezen, start laden data", "P", extra = " (kan eventjes duren)")
}


#// Import data
##=============

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

e <- try({
  df_all <- dbGetQuery(conn, qry)
  df_all <- df_all %>%
    mutate(sampletype = ifelse(is.na(sampletype), "SAMP", sampletype),
           waarde_ruw = as.numeric(value))
})
if (inherits(e, "try-error") || is.null(nrow(e)) || nrow(e) == 0) {
  if (nrow(e) == 0) {
    msg = "Data bevat 0 rijen"
  } else {
    msg = "Probleem laden data"
  }
  write_db_log(paste(msg, e), "E")
  stop(e)
} else {
  write_db_log(paste("data records:", nrow(df_all)), "P")
}


#// Process the data

e <- try({
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

  #onderzoeken waarom hier rijen bijkomen
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
})
if (inherits(e, "try-error")) {
  write_db_log(paste("Error processing data", e), "E")
  stop(e)
} else {
  write_db_log(paste("Data processed:", nrow(df_pivot), "records"), "P")
}


#// Calculate summary statistics
##===============================

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

write_db_log("Samenvattende statistiekeb berekend", "P")

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


e <- try(write_excel_csv2(df_pivot_incl_smry, file = csvpath, col_names = TRUE, na = ''))
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
} else {
 write_db_log("File weggeschreven", "C")
}




