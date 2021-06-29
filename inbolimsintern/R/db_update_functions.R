

#' Maak update queries zodat LIMS die kan verwerken
#'
#' @param conn dbi connectie naar db
#' @param data data ingelezen uit file die in bruikbaar formaat omgezet moet worden
#'
#' @return list met update queries
#' @export
db_update_worklist_table <- function(conn, data) {
  queries <- NULL
  date_request <- Sys.Date()
  for (i in 1:nrow(data)) {
    text_id <- data$TEXT_ID[i]
    sampNum <- data$SAMPLE_NUMBER[i]
    sampNum <- ifelse(is.na(sampNum), 'NULL', sampNum)
    testNum <- data$TEST_NUMBER[i]
    testNum <- ifelse(is.na(testNum), 'NULL', testNum)
    fieldName <- data$TABLE_KEY[i]
    fieldName <- ifelse(toupper(fieldName) == 'FIELDSAMPLEID', 'SAMPLE_ID', fieldName)
    queries[[i]] <-
      paste0(" insert into C_REQUESTED_DB_CHANGES(TEXT_ID, SAMPLE_NUMBER, TEST_NUMBER, [TABLE], TABLE_KEY, FIELD_NAME, ORIG_VALUE, NEW_VALUE, REQUESTED_BY, DATE_REQUEST,  REMARKS) VALUES (",
             "'", text_id, "', ",
             sampNum, ',',
             testNum, ',',
             "'", data$TABLE[i], "', ",
             "'", data$TABLE_KEY[i], "', ",
             "'", data$FIELD_NAME[i], "', ",
             "'", data$ORIG_VALUE[i], "', ",
             "'", data$NEW_VALUE[i], "', ",
             "'", data$REQUESTED_BY[i], "', ",
             "'", date_request, "', ",
             "'", data$REMARKS[i], "'",
             ")")
  }
  queries
}

#' Interpreteer data en zet om naar vorm van C_REQUESTED_DB_UPDATES
#'
#' @param data data ingelezen uit een csv die omgezet moet worden (1 rij per dataset want gebruikt via rowwise)
#' @param conn DBI connectie naar LIMS databank
#' @param remark_col de kolomnaam waar de update opmerkingen in staan
#'
#' @return data.frame omzetbaar naar queries voor C_REQUESTED_DB_UPDATES
#' @export
#'
db_update_get_data <- function(data, conn, remark_col = "opmerking_ZoetHab") {

  ### >>> Data ophalen

  #Bestaan all benodigde kolommen?
   if (! all(c('LABSAMPLEID', 'FIELDSAMPLEID', 'VELD_TE_WIJZIGEN',
              'ORIGINELE_WAARDE', 'NIEUWE_WAARDE',
              toupper(remark_col), 'LIMSANALYSISNAME') %in% names(data))) {
    stop("data heeft niet het correce formaat")
  }
  #Definieer de inhoud van de datarij
  text_id    <- as.character(data$LABSAMPLEID)
  extern_id  <- as.character(data$FIELDSAMPLEID)
  field_name <- as.character(data$VELD_TE_WIJZIGEN)
  orig_value <- as.character(data$ORIGINELE_WAARDE)
  new_value  <- as.character(data$NIEUWE_WAARDE)
  requested_by  <- as.character(substring(remark_col, 11))
  remarks       <- as.character(data[[toupper(remark_col)]])
  analysis  <- as.character(data$LIMSANALYSISNAME)
  analysis <- ifelse(is.null(analysis) | is.na(analysis) | analysis %in% c("NA", "-"),
                     NA,
                     analysis)
  table <- ifelse(is.na(analysis), 'SAMPLE', 'RESULT')

  #haal het staalnummer en testnummer op
  tableinfo <- get_sample_and_test_number(conn, text_id, extern_id,
                                          analysis = analysis)
  sample_number <- tableinfo$sample_number
  test_number <- tableinfo$test_number

  ### >>> corrigerende acties

  if (toupper(field_name) == "FIELDSAMPLEID") {
    field_name <- 'SAMPLE_ID'
  }
  if (toupper(field_name) == "FIELDOBSERVER") {
    field_name <- 'C_MONSTER_BY'
  }
  if (toupper(field_name) == "FIELDSAMPLINGDATE") {
    field_name <- 'C_MONSTER_DATE'
  }

  if (field_name %in% c('SAT.VELD', 'O2.VELD', 'O2_veld',
                        'EC25.VELD', 'WD_VELD', 'SD_VELD')){
    if (field_name == 'SD_VELD') field_name = 'SD.VELD'
    if (field_name == 'WD_VELD') field_name = 'WD.VELD'
  }
  data.frame(TEXT_ID = text_id,
             SAMPLE_NUMBER = sample_number,
             TEST_NUMBER = test_number,
             TABLE = table,
             TABLE_KEY = analysis,
             FIELD_NAME = field_name,
             ORIG_VALUE = orig_value,
             NEW_VALUE = new_value ,
             REQUESTED_BY = requested_by,
             REMARKS = remarks )
}


#' Verkrijg sample_number en test_number op basis van text_id en analysenaam
#'
#' @param conn DBI connectie naar LIMS DB
#' @param text_id text_id waarvoor sample nummer gezocht moet worden
#' @param extern_id optioneel extern_id waar ook op gecheckt moet worden voor extra veiligheid
#' @param analysis  optioneel enkel wanneer een specifieke test opgehaald moet worden
#'
#' @return data.frame met sample_number, sample_id en test_number als velden
#' @export
#'
get_sample_and_test_number <- function(conn, text_id, extern_id = NULL, analysis = NULL){
  if (is.null(analysis) | is.na(analysis)) {
    qry = paste0("select sample_number, sample_id, test_number = NULL from sample where text_id = '", text_id, "'")
  } else {
    qry = paste0("select sample.sample_number, sample_id, test_number from sample inner join test on sample.sample_number = test.sample_number where text_id = '", text_id, "' and test.ANALYSIS = '", analysis, "'")
  }

  print(qry)
  print(extern_id)

  if (!(is.null(extern_id) |is.na(extern_id) | (extern_id %in% c("-", "NA")))) {
    print("hereo")
    qry <- paste0(qry, " and sample_id = '", extern_id, "' order by test_number desc")
  }

  qrytest <<- qry
  rv <- dbGetQuery(conn, qry)
  print(rv)
  if (nrow(rv) == 0) rv <- data.frame(sample_number = NA, sample_id = NA, test_number = NA)
  print(rv)
  rv
}
