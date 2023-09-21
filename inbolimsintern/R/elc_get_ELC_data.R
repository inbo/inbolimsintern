#' Verkrijg de ELC data
#'
#' @param dbcon dbconnection object (DBI)
#' @param sqlfile path naar de file die de sql code bevat
#' @param keep aantal batches te behouden voor de figuur
#' @param productversions data.frame me een kolom PRODUCT en VERSIE om te bepalen welke limieten gebruikt moeten worden. IF NULL dan worden de maximale versies gebruikt voor elk product
#'
#' @return dataset met alle te verwerken gegevens
#' @export
get_ELC_data <- function(dbcon, sqlfile, keep = 30, productversions = NULL) {
  sqlcode <- readLines(sqlfile)
  sqlcode <- paste(sqlcode, collapse = "\n")

  #qry = "select NAME, MaxVersion = max(VERSION) from PRODUCT group by NAME"
  #productVersions = DBI::dbGetQuery(dbcon, qry)
  cat(sqlcode)

  plotdata <- DBI::dbGetQuery(dbcon, sqlcode)

  #Haal de batches op door enkel de eerste ENTRY te behouden per component

  dofun <- function(data, keep) {
    rv <- slice_tail(data, n = keep)
    rv <- mutate(rv, BATCHNR = 1:nrow(rv))
    rv
  }

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

  attr(plotdata2, "sqlcode") <- sqlcode
  plotdata2
}
