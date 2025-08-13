#' Write a Log Entry to the Database
#'
#' Inserts a new record into the `C_RSCRIPT_LOG` table to log the status of a
#' script execution.
#'
#' @param conn A DBI database connection object.
#' @param call_id A unique identifier for the specific script execution.
#' @param status A character code representing the status. It's recommended to use
#'   a consistent set of codes, for example:
#'   \itemize{
#'     \item \code{START}: The process has started.
#'     \item \code{SUCCESS}: The process completed without errors.
#'     \item \code{FAIL}: The process terminated with an error.
#'     \item \code{INFO}: A generic informational message.
#'   }
#' @param message An optional character string providing additional details or an
#'   error message. Defaults to an empty string.
#'
#' @returns Invisibly returns the number of rows affected by the insert statement
#'   (typically 1 on success). The return value is invisible to prevent it
#'   from printing to the console.
#'
#' @importFrom DBI dbExecute
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'conn' is an active database connection
#' call_id <- 12345
#'
#' # Log the start of a script
#' write_db_log(conn, call_id, "START", "Processing started for monthly report.")
#'
#' # Log a successful completion
#' write_db_log(conn, call_id, "SUCCESS")
#'
#' # Log a failure
#' write_db_log(conn, call_id, "FAIL", "Error in data aggregation step.")
#' }
write_db_log <- function(conn, call_id, status, message = "") {
  sql <- "INSERT INTO C_RSCRIPT_LOG (call_id, status, timestamp, log_message) VALUES (?, ?, ?, ?);"

  # Execute the query, passing the values as parameters.
  # The function is wrapped in invisible() so it doesn't print the
  # number of affected rows (1) to the console upon success.
  invisible(
    DBI::dbExecute(
      conn,
      sql,
      params = list(call_id, status, Sys.time(), message)
    )
  )
}
