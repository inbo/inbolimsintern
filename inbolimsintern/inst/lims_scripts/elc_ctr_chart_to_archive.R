
# QC chart to archive

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "CTR_SAVE")
writeLines(con = logfile, paste0("Bewaren archiefkaarten\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 4269 call_id <- 4275 call_id <- 5349
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

try({
  sqlfile <- filter(params, ARG_NAME == "SQL_FILE") %>% pull(VALUE)
  htmlfile <- filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE)
  chartlabel <- filter(params, ARG_NAME == "LABEL") %>% pull(VALUE)
  product <- filter(params, ARG_NAME == "PRODUCT") %>% pull(VALUE)
  productversie <- filter(params, ARG_NAME == "PRODUCT_VERSION") %>% pull(VALUE)
  datetime <- filter(params, ARG_NAME == "DATE_TIME") %>% pull(VALUE)
  user <- filter(params, ARG_NAME == "USER") %>% pull(VALUE)
  htmlrootshort <- substring(htmlfile, max(unlist(gregexpr("\\\\", htmlfile))) + 1, nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
  htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile)))) #including last backslash
}, outFile = logfile)


### data inlezen

try({
  #haal sqlcode op
  sqlcode <- readLines(sqlfile)
  sqlcode <- paste(sqlcode, collapse = "\n")
  cat("\n", sqlcode, file = logfile, append = TRUE)

  #haal data binnen (deze bevat reeds de limieten)
  alldata<- get_ELC_data(conn, sqlfile, keep = Inf) %>%
    filter(C_CTR_ADD == 'T')

}, outFile = logfile)

if (nrow(alldata) == 0) cat("\nGEEN DATA\n", file = logfile, append = TRUE)
combis <- unique(alldata$combi)

archive_data <- NULL
for (comb in combis) {
  print(comb)
  plotdata <- alldata %>% filter(comb == combi)
  htmldata <- elc_htmldata(plotdata)
  archive_data <- rbind(archive_data, htmldata$plot)
}
archive_data_export <- archive_data %>%
  transmute(LABEL, DATE = datetime, USER = user,
            PRODUCT, LIMIT_VERSION = VERSION, SAMPLING_POINT, SAMPLE_NAME,
            BATCH, BATCHNR, CHECK_RULES,  ANALYSIS, NAME, ENTERED_ON, ENTRY, UNITS,
            C_CTR_X, C_CTR_SD, C_CERTIFIED_VALUE, C_CERTIFIED_SD,
            OUT3S, WARN, OUT2S, DRIFT, BIAS, COLOR, SIZE,
            LCL3S, LCL2S, LCL1S, UCL1S, UCL2S, UCL3S, COMBI = combi) %>%
  arrange(PRODUCT, SAMPLING_POINT, ANALYSIS, NAME, SAMPLE_NAME, BATCHNR)

DBI::dbWriteTable(conn, name = "C_CTR_ARCHIVE", value = archive_data_export,
                  overwrite = FALSE, append = TRUE)

### Maak HTML

cat(paste0("<html>\n<head>", "call_id : ", args["call_id"], "</head>\n<body>\n"),
    file = htmlfile, append = FALSE)
csvfile <- paste0(htmlpath, htmlrootshort, ".csv")
cat("<a href=\"",csvfile, "\">download csv data:</a>", file = htmlfile, append = FALSE)
write_excel_csv2(alldata, path = csvfile)

teller <- 0
all_archive_data <- NULL
for (cmb in unique(alldata$COMBI)) {
  teller <- teller + 1
  fignr <- sprintf("%03d", teller)
  print(cmb)
  figbaseabs <- paste0(htmlpath, htmlrootshort, "_", fignr, "_")
  figbaserel <- paste0(htmlrootshort, "_", fignr, "_")
  cat(paste0("<h2>", cmb, "</h2>"), file = htmlfile, append = TRUE)
  dfcomb <- alldata %>%
    arrange(IN_STAT, ROWNR) %>%
    filter(COMBI == cmb)
  print(nrow(dfcomb))

  #controlekaart laatste jaar
  batchvolgorde <- dfcomb %>%
    group_by(BATCH) %>%
    summarize(FIRST_BATCH_ENTRY = min(ENTERED_ON)) %>%
    arrange(FIRST_BATCH_ENTRY) %>%
    mutate(BATCHNR = 1:nrow(.))

  print(nrow(batchvolgorde))

  dfcomb2 <- dfcomb %>%
    left_join(batchvolgorde, by = "BATCH") %>%
    arrange(BATCHNR)

  print("here")
  print(nrow(dfcomb2))

  ctrdata <- try(elc_fixlim_data(dfcomb2))

  print("here2")
  ctrtabel <- ctrdata %>%
    transmute(BATCH, TEXT_ID, ENTRY = round(ENTRY, 5), UNITS,
              EVAL = ifelse(is.na(EVAL), ".", EVAL))


  if (class(ctrdata) == "try-error") {
    cat("\nTE WEINIG DATA\n", file = htmlfile, append = TRUE)
    next

  } else {
    all_archive_data <- rbind(all_archive_data, ctrdata)
  }
  p2 <- ELC_shewhart_plot(subdata = ctrdata ) +
    labs(title = cmb,
         x = paste0("Batch"))

  ggsave(p2, filename = paste0(figbaseabs, ".png"))
  cat(paste0("\n<p>\n<IMG SRC = \"", paste0(figbaserel, ".png"), "\">\n"),
      file = htmlfile, append = TRUE)

  cat(knitr::kable(ctrtabel, format = "html", row.names = FALSE),
      file = htmlfile, append = TRUE)

}

write_archive_data <- function(conn, data) {
  colnames(data) <- toupper(colnames(data))

  cond_str <- paste0(" LABEL in ('",
                     paste(unique(data$LABEL), collapse = "','"), "')",
                     " and ANALYSIS in ('",
                     paste(unique(data$ANALYSIS), collapse = "','"), "')",
                     " and NAME in ('",
                     paste(unique(data$NAME), collapse = "','"), "')",
                     " and SAMPLE_NAME in ('",
  paste(unique(data$PRODUCT_GRADE), collapse = "','"), "')"
  )


  qry_check <- paste0("select LABEL, ANALYSIS, NAME, SAMPLE_NAME ",
                      " from C_CTR_ARCHIVE ",
                      " where ", cond_str)
  in_db <- dbGetQuery(conn, qry_check)
  if (nrow(in_db)) {
    qry_dis <- paste0(" update C_CTR_ARCHIVE set ACTIVE = 'F' where ", cond_str)
    dbSendQuery(conn, qry_dis)
  }
  print("here")
  data <- data %>%
    select(-which(duplicated(colnames(data)))) %>% #kolom waarde is dubbel
    mutate(ACTIVE = 'T')

  print(colnames(data))
  dbAppendTable(conn, "C_CTR_ARCHIVE",
                data %>% select(-ROWNR, -FIRST_BATCH_ENTRY))
}

write_archive_data(conn, all_archive_data)

cat(paste0("</body></html>"), file = htmlfile, append = TRUE)

shell.exec(htmlfile)