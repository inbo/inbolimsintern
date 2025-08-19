#' Create a connection to the LIMS database
#'
#' This function automatically detects the environment (DEV, UAT, PRD, LW7),
#' retrieves the appropriate credentials using Sys.getenv(), and establishes
#' a database connection.
#'
#'@param env which environment PRD, UAT, DEV or LW7
#'@param connectlist list containinf connection information, should at least have the elements host, dsn, uid and pwd
#' It requires a correctly configured .Renviron file to be present in the
#' user's home directory or the project's working directory.
#'
#' @return A DBI database connection object.
#' @export
limsdb_connect <- function(env = "PRD", connectlist = NULL) {
  if (!is.null(connectlist)) {
    if (!all(c("host", "dsn", "uid", "pwd") %in% names(connectlist))) {
      stop("connectlist not valid, should be a list at least having the elements host, dsn, uid and pwd")
    }
    cvars <- connectlist
 } else {
    # Detect which environment we are in
    message("Attempting to connect to environment: ", env)

    # Construct the names of the environment variables to look for
    host_var <- paste0("DB_HOST_", env)
    user_var <- paste0("DB_USER_", env)
    pass_var <- paste0("DB_PASS_", env)
    dsrc_var <- paste0("DB_DSRC_", env)

    # Retrieve the credentials using Sys.getenv()
    #    R automatically finds and loads the .Renviron file on startup.
    db_host <- Sys.getenv(host_var)
    db_user <- Sys.getenv(user_var)
    db_pass <- Sys.getenv(pass_var)
    db_dsrc <- Sys.getenv(dsrc_var)
    # Check that the variables were actually found
    if (db_host == "" || db_user == "" || db_pass == "" || db_dsrc == "") {
      stop(
        "Database credentials not found for environment '", env, "'.\n",
        "Ensure a .Renviron file is present and contains the variable: ", host_var,
        call. = FALSE
      )
    }
    cvars <- list(host = db_host, dsn = db_dsrc, uid = db_user, pwd = db_pass)
  }

  # Establish and return the connection (example using DBI and odbc)
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = cvars$host,
    Database = cvars$dsn,
    UID      = cvars$uid,
    PWD      = cvars$pwd,
    Port     = 1433
  )
  return(con)
}
