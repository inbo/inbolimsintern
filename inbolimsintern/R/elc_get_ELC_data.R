#' Verkrijg de ELC data
#'
#' @param dbcon dbconnection object (DBI)
#' @param sqlfile path naar de file die de sql code bevat
#' @param keep aantal batches te behouden voor de figuur
#' @param productversions data.frame me een kolom PRODUCT en VERSIE om te bepalen welke limieten gebruikt moeten worden. IF NULL dan worden de maximale versies gebruikt voor elk product
#'@param logfile indien niet NULL schrijf debugging info weg in logfile
#' @return dataset met alle te verwerken gegevens
#' @export
get_ELC_data <- function(dbcon, sqlfile, keep = 30, productversions = NULL, logfile = NULL) {

  cat("\nimporting results ...\n", file = logfile, append = TRUE)
  sqlcode <- readLines(sqlfile)
  sqlcode <- paste(sqlcode, collapse = "\n")
  sqlcode <- stringi::stri_encode(sqlcode, to = "UTF-8")
  #diff between greek mu and micro mu: (niet meer nodig)
  #oplossing: rechtstreeks in query:
  #SELECT REPLACE(r.NAME, NCHAR(0x00B5), N'u') AS NAME (ook in join gebruiken)
  #sqlcode <- gsub("/u03BCm", "/u00B5m", sqlcode)
  #sqlcode <- gsub("/u03BCS", "/u00B5S", sqlcode)
  #sqlcode <- gsub("µm", "/u00B5m", sqlcode)
  #sqlcode <- gsub("µS", "/u00B5S", sqlcode)


  try(cat("\nSQL code:\n", sqlcode, "\n", sep = "\n", file = logfile, append = TRUE))
  cat(sqlcode, file = logfile, append = TRUE)

  plotdata <- DBI::dbGetQuery(dbcon, sqlcode)
  cat("\nstructure of imported data (query):\n", file = logfile, append = TRUE)
  try(cat(str(plotdata), "\n", sep = "\n", file = logfile, append = TRUE))
  #Haal de batches op door enkel de eerste ENTRY te behouden per component

  dofun <- function(data, keep) {
    rv <- slice_tail(data, n = keep)
    rv <- mutate(rv, BATCHNR = 1:nrow(rv))
    rv
  }

  sink(file = logfile, append = TRUE, split = TRUE)
  cat(str(plotdata), "\n", sep = "\n", file = logfile, append = TRUE)

  firstentries <- plotdata %>%
    group_by(C_DATE_BATCHRUN, BATCH, ANALYSIS, NAME, SAMPLE_NAME) %>%
    arrange( C_DATE_BATCHRUN, BATCH, ANALYSIS, NAME, SAMPLE_NAME, ENTERED_ON) %>%
    slice(1) %>%
    group_by(ANALYSIS, NAME, SAMPLE_NAME) %>%
    do(dofun(., keep)) %>%
    select(BATCH, ANALYSIS, NAME, SAMPLE_NAME, BATCHNR, FIRST_ORDER_NUMBER = ORDER_NUMBER)

  plotdata2 <- plotdata %>%
    inner_join(firstentries, by = c("BATCH", "ANALYSIS", "NAME", "SAMPLE_NAME")) %>%
    mutate(combi = paste(ANALYSIS, SAMPLE_NAME, NAME, sep = "---"),
           FIRST_ENTRY = ifelse(FIRST_ORDER_NUMBER == ORDER_NUMBER, TRUE, FALSE)) %>%
    arrange(ANALYSIS, NAME, SAMPLE_NAME, BATCHNR)

  cat("\nstructure of processed data (only first entries):\n", file = logfile, append = TRUE)
  try(cat(str(plotdata2), "\n", sep = "\n", file = logfile, append = TRUE))
  attr(plotdata2, "sqlcode") <- sqlcode
  try(sink())
  plotdata2
}
