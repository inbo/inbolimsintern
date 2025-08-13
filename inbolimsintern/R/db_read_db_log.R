
#' Haal de status op van het lopen van het R script
#'
#' @param conn db connection object
#' @param call_id integer specifying which call is run
#'
#' @returns dataset with current status of the table
#' @export
#'
read_db_log <-
function(conn, call_id){
  q = paste0("select ID, CALL_ID, STATUS, ",
             "TIMESTAMP, LOG_MESSAGE ",
             " from C_RSCRIPT_LOG ",
             " where CALL_ID = ", call_id,
             " order by ID desc")
  rv <- DBI::dbGetQuery(conn, q)
  rv
}
