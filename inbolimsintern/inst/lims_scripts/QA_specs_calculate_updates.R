library(googlesheets4)
library(DBI)
library(inbolimsintern)

logfile <- logfile_start(prefix = "QC_SPECS")
writeLines(con = logfile, paste0("inbolimsintern versie: ", packageVersion("inbolimsintern")))

call_id <- 0 #mag 0 blijven bij gs, bij excel moet dit iets zijn die een argument PATH en SHEET heeft
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

#Dit zal wel uit excel ingelezen worden om authentificatieproblemen te voorkomen
read_from_gs <- TRUE
if (read_from_gs) {
  gs4_auth(email = "pieter.verschelde@inbo.be")
  dataOrig <- read_sheet("1wjG-tOCGUzfx4ky1cmPXHAfB3qqCVul3TE6FbeNlkPo",
                     sheet = "Sheet1") %>%
    mutate(AVG_EXPECTED = as.numeric(unlist(AVG_EXPECTED)),
           AVG_MEASURED = as.numeric(unlist(AVG_MEASURED)))
} else {
  dataOrig <- read_excel(path, sheet = sheet)
}

update_qc_limits(conn, dataOrig)

#######################################
### VOORBEELD BEREKENING            ###
#######################################

label = "test2020"
min_date = '2020-01-01'
max_date = '2021-01-01'

calc_avg_sd <- function(x) {
  x <- x[!is.na(x)]
  avg <- mean(x)
  sdev <- sd(x)
  n <- length(x)
  print(n)
  nkeep <- n
  low3s <- avg - 3 * sdev
  upp3s <- avg + 3 * sdev
  if (!is.na(sdev) & (any(x > upp3s) | any(x < low3s))) {
    xkeep <- x[x <= upp3s & x>= low3s] #alle buiten 3s worden allemaal direct eruit gezwierd
    nkeep <- length(xkeep)
    newvals <- calc_avg_sd(xkeep)
    n <- unname(newvals['n'])
    avg <- unname(newvals['avg'])
    sdev <- unname(newvals['sd'])
  }
  c(n = nkeep, avg = avg, sd = sdev)
}

data <- dataOrig
data$NEW_AVG <- NA
data$NEW_SD <- NA
data$N <- NA
for (i in 1:nrow(data)) {
  print(paste("rij ", i, round(100*i/nrow(data),1), "%"))
  datavec <- unlist(data[i, ])
  qry <- paste0(" select r.ENTRY from RESULT r inner join sample s on r.SAMPLE_NUMBER = s.SAMPLE_NUMBER ",
             " where r.ANALYSIS = '", datavec["ANALYSIS"], "'",
             " and r.NAME = '", datavec["COMPONENT"], "'",
             " and s.SAMPLE_NAME = '", datavec["QC_SAMPLE"], "'",
             " and s.PRODUCT = '", datavec["PRODUCT"], "'",
             " and r.STATUS in ('E', 'M', 'A') and ENTERED_ON >= '", min_date, "' and ENTERED_ON <= '", max_date, "'")
  waarden <- DBI::dbGetQuery(conn, qry) %>%
    mutate(NUM_ENTRY = as.numeric(ENTRY))
  calcs <- calc_avg_sd(waarden %>%  pull(NUM_ENTRY))
  data$NEW_AVG[i] <- calcs['avg']
  data$NEW_SD[i] <- calcs['sd']
  data$N[i] <- calcs['n']
}
data <- data %>%
  mutate(AVG_MEASURED = round(NEW_AVG, 6),
         SD_MEASURED = round(NEW_SD, 7),
         AVG_EXPECTED = unlist(AVG_EXPECTED),
         NEW_AVG = NULL,
         NEW_SD = NULL)
write_excel_csv2(data, path = "\\\\limsbgops.inbo.be\\LABO_FS\\LIMS\\Export_limieten_test2020.csv", na = "")
write_excel_csv(data, path = "\\\\limsbgops.inbo.be\\LABO_FS\\LIMS\\Export_limieten_test2020_en.csv", na = "")


