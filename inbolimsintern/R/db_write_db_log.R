#' Write a log message to the database.
#'
#' This function inserts a log entry into the C_RSCRIPT_LOG table.
#' The 'message' and 'status' are required. Other parameters like 'call_id',
#' 'user', and 'conn' are optional; if not provided, the function will attempt
#' to retrieve them from the parent environment.
#'
#' @param message The log message string to be written. (Required)
#' @param status The status of the log entry (e.g., "I", "P" (in progress), "E" (error), "C" (completed)). (Required)
#' @param call_id A unique identifier for the script execution. If NULL,
#'   the function will look for a 'call_id' variable in the parent frame.
#' @param user_name The username associated with the log entry. If NULL,
#'   the function will look for a 'user_name' variable in the parent frame.
#' @param conn The database connection object. If NULL, the function will
#'   look for a 'conn' variable in the parent frame.
#' @param extra An optional string to append to the message when printing to the
#'   console. Defaults to an empty string.
#' @param print_log A logical value indicating whether to print the message to the
#'   console. Defaults to TRUE.
#'
#' @return Invisibly returns the number of rows affected by the SQL execution and writes a message to the console
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_conn', 'current_user', and 'run_id' exist in the environment
#' conn <- my_conn
#' user_name <- current_user
#' call_id <- run_id
#'
#' # Now you can call the function with only the required arguments
#' write_db_log("Starting the process", "P")
#' # ... some processing ...
#' write_db_log("Process finished successfully", "C")
#' }
write_db_log <- function(message,
                         status,
                         call_id = NULL,
                         user_name = NULL,
                         conn = NULL,
                         extra = "",
                         print_log = TRUE) {

  # --- Argument Handling ---
  # If arguments are not provided, try to get them from the parent environment.
  # This makes the function call cleaner at the top level.

  # Get call_id if it's not provided
  if (is.null(call_id)) {
    if (exists("call_id", envir = parent.frame())) {
      call_id <- get("call_id", envir = parent.frame())
    } else {
      stop("Argument 'call_id' was not provided and could not be found in the parent environment.", call. = FALSE)
    }
  }

  # Get user if it's not provided
  if (is.null(user_name)) {
    if (exists("user_name", envir = parent.frame())) {
      user_name <- get("user_name", envir = parent.frame())
    } else {
      stop("Argument 'user_name' was not provided and could not be found in the parent environment.", call. = FALSE)
    }
  }

  # Get conn if it's not provided
  if (is.null(conn)) {
    if (exists("conn", envir = parent.frame())) {
      conn <- get("conn", envir = parent.frame())
    } else {
      stop("Argument 'conn' was not provided and could not be found in the parent environment.", call. = FALSE)
    }
  }

  # --- Database Execution ---
  sql <- "INSERT INTO C_RSCRIPT_LOG (call_id, status, timestamp, log_message, lims_user) VALUES (?, ?, ?, ?, ?);"

  # Print the message to the console if requested
  if (print_log) {
    # Using paste to handle the 'extra' argument cleanly
    message(paste(message, extra))
  }

  # Execute the query, passing the values as parameters.
  # The function is wrapped in invisible() so it doesn't print the
  # number of affected rows (1) to the console upon success.
  invisible(
    DBI::dbExecute(
      conn,
      sql,
      params = list(call_id, status, Sys.time(), message, user_name)
    )
  )
}




# write_db_log <- function(message,
#                          status,
#                          call_id,
#                          user,
#                          conn,
#                          extra = "",
#                          print = T) {
#   sql <- "INSERT INTO C_RSCRIPT_LOG (call_id, status, timestamp, log_message, lims_user) VALUES (?, ?, ?, ?, ?);"
#
#   # Execute the query, passing the values as parameters.
#   # The function is wrapped in invisible() so it doesn't print the
#   # number of affected rows (1) to the console upon success.
#   if (print) message(message, extra)
#   invisible(
#     DBI::dbExecute(
#       conn,
#       sql,
#       params = list(call_id, status, Sys.time(), message, user)
#     )
#   )
# }
