
#' Update de C_QC_LIMITS tabel
#'
#' @param conn connectie naar db
#' @param data dataset die alle records bevat die geupdated moeten worden
#'
#' @return invisible
#' @export
#'
update_qc_limits <- function(conn, data) {
  for (i in 1:nrow(data)) {
    datarow <- unlist(data[i, ])
    theo_value  <- datarow['AVG_EXPECTED']
    theo_sd     <- datarow["SD_EXPECTED"]
    meas_value  <- datarow["AVG_MEASURED"]
    meas_sd     <- datarow["SD_MEASURED"]

    label       <- datarow['LABEL']
    active      <- datarow["ACTIVE"]
    qc_sample   <- datarow["QC_SAMPLE"]
    qc_version  <- datarow["QC_SAMPLE_VERSION"]
    analysis    <- datarow['ANALYSIS']
    component   <- datarow["COMPONENT"]
    product     <- datarow["PRODUCT"]
    product_version   <- datarow["PRODUCT_VERSION"]
    sampling_point    <- datarow["SAMPLING_POINT"]

    queries <- make_spec_queries(label, active, qc_sample, qc_version,
                                 analysis, component,
                                 product, product_version, sampling_point,
                                 theo_value, theo_sd,
                                 meas_value, meas_sd)
    existing <- DBI::dbGetQuery(conn, queries[["select"]])
    if (nrow(existing) > 0) {
      status <- DBI::dbGetQuery(conn, queries[["update"]])
    } else {
      status <- DBI::dbGetQuery(conn, queries[["insert"]])
    }
  }
  return()
}


#' Maak insert, update, delete en select queries voor de C_QC_LIMITS tabel
#'
#' @param label label om de qc limieten op de selecteren
#' @param active "T" indien in gebruik, "F" dan kunnen die niet meer gebruikt worden
#' @param qc_sample naam van qc_sample in qc_samples tabel (= PRODUCT_GRADE)
#' @param qc_version versie van qc_sample
#' @param analysis analyse waarvoor de limieten gelden
#' @param component component waarvoor limieten gelden
#' @param product product waar QC_SAMPLE bij hort
#' @param product_version productversie die bij de limieten hoort
#' @param sampling_point sampling_point zoals in SAMPLING_POINT em PRODUCT_SPECS tabel
#' @param theo_value theoretische waarde
#' @param theo_sd theoretische sd (zal vaak 0 zijn)
#' @param meas_value gemeten gemiddelde waarde
#' @param meas_sd gemeten standaarddeviatie, waarop de 3s grenzen worden berekend
#'
#' @return lijst met 4 elementen (select, update, delete, insert) die elk de respectievelijke query bevatten
#' @export
#'
make_spec_queries <- function(label, active = "F", qc_sample, qc_version,
                              analysis, component,
                              product, product_version, sampling_point,
                              theo_value = NULL, theo_sd = NULL,
                              meas_value = NULL, meas_sd = NULL) {
  where_part <- paste0(
    " where LABEL = ", "'", label, "'", " and QC_SAMPLE = ", "'", qc_sample, "'",
    " and QC_SAMPLE_VERSION = ", "'", qc_version, "'",
    " and ANALYSIS = ", "'", analysis, "'",
    " and COMPONENT = ", "'", component, "'",
    " and PRODUCT = ", "'", product, "'",
    " and PRODUCT_VERSION = ", product_version,
    " and SAMPLING_POINT = ", "'", sampling_point, "'"
  )

  qry_sel <-  paste0(" select ID from C_QC_LIMITS ", where_part)
  qry_upd <- paste0(" update C_QC_LIMITS set ",
                    " AVG_EXPECTED = ", theo_value, ", ",
                    " SD_EXPECTED = ", theo_sd, ", ",
                    " AVG_MEASURED = ", meas_value, ", ",
                    " SD_MEASURED = ", meas_sd,
                    where_part)
  qry_del <- paste0(" delete from C_QC_LIMITS ", where_part)

  qry_ins = paste0(
    " insert into C_QC_LIMITS (LABEL, ACTIVE, QC_SAMPLE, QC_SAMPLE_VERSION, ANALYSIS, COMPONENT, ",
    " AVG_EXPECTED, SD_EXPECTED, AVG_MEASURED, SD_MEASURED, PRODUCT, PRODUCT_VERSION, SAMPLING_POINT)",
    " values (" ,
    "'", label , "'", ",",
    "'", active, "'", ",",
    "'", qc_sample, "'", ",",
    qc_version, ",",
    "'", analysis, "'", ",",
    "'", component, "'", ",",
    theo_value, ",",
    theo_sd, ",",
    meas_value, ",",
    meas_sd, ",",
    "'", product, "'", ",",
    product_version, ",",
    "'", sampling_point, "'",
    ")"
  )
  list(select = qry_sel, update = qry_upd, delete = qry_del, insert = qry_ins)
}
