#' Schrijf de logstatus weg in de databank
#'
#' @param conn db connectie object
#' @param call_id identifier van de opgeroepen routine
#'
#' @returns insertion of log information in C_RSCRIPT_LOG
#' @export
#'
write_db_log <- function(conn, call_id, status, message = "") {
  # The SQL statement with placeholders (?)
  sql <- "INSERT INTO C_RSCRIPT_LOG (call_id, status, timestamp, log_message)
          VALUES (?, ?, ?, ?);"

  # Execute the query, passing the values as parameters
  # The database driver will format the date correctly
  dbExecute(conn, sql,
            params = list(call_id,
                          status,
                          Sys.time(),
                          message))
}

#write_db_log(conn, 0, "U", "Initial test")
