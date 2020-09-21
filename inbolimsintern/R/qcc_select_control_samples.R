#' Selecteer de waarden die als basis voor de controlekaart dienen
#'
#' @param conn db connection
#' @param num number of samples to keep or all samples of a year if coded as JD2017 or all samples from the last D days, coded starting with a D, so num can be like: 30 or JD2017 or D60
#' @param batch the name of the batch, which will be used to check which control samples are returned 
#' @param analysis the name of the analysis
#' @param components a vector of one or more components within the analysis
#' @param start the earliest startdate coded as "YYYY-mm-dd". Only if num is a numeric value
#' @param end the latest enddate coded as "YYYY-mm-dd". Only used if num is a numeric value
#'
#' @return a dataset with all lab information (SAMPLE_NAME, SAMPLE_NUMBER, TEXT_ID, ANALYSIS, NAME, BATCH, ENTRY, C_DATE_BATCHRUN, ENTERED_ON, Nr, CV)
#' @export
#' @importFrom dplyr bind_rows row_number left_join
#' @import dplyr
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' batch = "IC_AN-190430-1"
#' analysis = "IC_ANIONEN"
#' components = c("NO2", "NO3")
#' testdf <- select_control_samples(conn, num = 30, batch, analysis, components)
#' }
select_control_samples <- function(conn, num, batch, analysis, components, 
                                   start = "1900-01-01", end = "2200-01-01"){
  
  #Kies de datums afhankelijk van de laatste N of een opgegeven jaar
  today <- Sys.Date()  
  if (suppressWarnings(is.na(as.numeric(num)))) {
    #indien num geen getal is
    if (substring(num,1,2) == "JD") {
      year <- as.numeric(substring(num,3,6))
      start <- paste0(year, "-01-01")
      end <- paste0(year + 1, "-01-01")
      num <- 5000
    } else if (substring(num, 1, 1) == "D") {
      days_previous <- as.numeric(substring(num, 2))
      end <- as.character(format(today + 1, format = "%Y-%m-%d"))
      start <- as.character(format(today - days_previous, format = "%Y-%m-%d"))
      num <- 5000
    }
  } else {
    #indien num een getal is
    start <- start
    end <- end
  }
  
  #Zoek de verschillende controlestalen die in dit type batch kunnen voorkomen
  sql01 <- paste0("select distinct(s.sample_name) from test t",
                  " inner join sample s on t.sample_number = s.sample_number",
                  " where t.batch = '", batch ,"'",
                  " and s.sample_type in ('CONTROL', 'BLANK', 'PRBLANCO', 'STANDARD')")
  dfSampNames <- DBI::dbGetQuery(conn, sql01)
  
  #Maak een query om alle data op te halen
  alldata <- NULL
  for (samptyp in dfSampNames[[1]]) {
    for (comp in components) {
      print(c(samptyp, comp))
      sql02 <- paste0("SELECT top(", num, ") ",
                      "s.SAMPLE_NAME, s.SAMPLE_NUMBER, s.TEXT_ID, r.ANALYSIS, r.NAME, t.BATCH, ", 
                      "r.ENTRY, b.C_DATE_BATCHRUN, r.ENTERED_ON, u.DISPLAY_STRING",
                      " from sample s inner join test t on s.sample_number = t.sample_number",
                      " inner join result r on t.test_number = r.test_number",
                      " left join batch b on t.BATCH = b.NAME",
                      " left join units u on u.UNIT_CODE = r.UNITS",
                      " where r.ENTRY is not NULL and r.STATUS in ('E', 'M', 'A')",
                      " and s.SAMPLE_NAME = '", samptyp, "'", " AND r.NAME ='", comp, "'",
                      " and t.ANALYSIS = '", analysis, "'",
                      " and r.ENTERED_ON >= '",start, "'",
                      " and r.ENTERED_ON < '", end, "'",
                      " ORDER BY b.C_DATE_BATCHRUN desc, b.NAME, r.ENTERED_ON desc")
      tmp <- DBI::dbGetQuery(conn, sql02)
      alldata <- bind_rows(alldata, tmp)
    }
  }
  alldata <- 
    na.omit(alldata) %>% 
    arrange(desc(.data$C_DATE_BATCHRUN), desc(.data$ENTERED_ON)) %>%
    group_by(.data$ANALYSIS, .data$NAME, .data$SAMPLE_NAME) %>% 
    mutate(Nr = n() - row_number() + 1)
  
  print(alldata)
  
  ###link data with product specifications
  
  sql03 <-
    paste0("
           select ps0.GRADE, ps0.ANALYSIS, ps0.COMPONENT, CV = ps0.NOMINAL_VALUE
           from product_spec ps0
           inner join
           (select PRODUCT, GRADE, ANALYSIS, COMPONENT,MAX_VERSION = max(VERSION)
           from product_spec
           group by PRODUCT, GRADE, ANALYSIS, COMPONENT) ps1
           on ps0.GRADE = ps1.GRADE and ps0.ANALYSIS = ps1.ANALYSIS and ps0.COMPONENT = ps1.COMPONENT and ps0.VERSION = ps1.MAX_VERSION"
    )
  cvdata <- DBI::dbGetQuery(conn, sql03)
  alldata <- 
    left_join(alldata, cvdata,
              by = c("SAMPLE_NAME" = "GRADE", "ANALYSIS", "NAME" = "COMPONENT")) %>% 
    arrange(.data$SAMPLE_NAME, .data$Nr) %>%
    mutate(ENTRY = as.numeric(gsub(",", ".", .data$ENTRY)))
  
  ### RETURN
  
  write.csv2(alldata, file = "QC_CHARTS.csv")
  
  alldata
}
