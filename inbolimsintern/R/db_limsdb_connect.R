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
limsdb_connect <- function(env = "PRD", connectlist = NULL, logfile = 'limsdbconnect.log') {
  #cat("entering limsdb_connect\nenv:",env,"\n", file = logfile, append = FALSE)
  #cat("environment: ", env, "\n", file = logfile, append = TRUE)
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
    
    db_envs <- Sys.getenv()[grepl("^db", names(Sys.getenv()), ignore.case = TRUE)]
    #cat("host env var: ", host_var, "\n", file = logfile, append = TRUE)
    ##cat("env vars :\n", 
    #    paste(db_envs, collapse = "\n"), '\n', file = logfile, append = TRUE)

    # Retrieve the credentials using Sys.getenv()
    #    R automatically finds and loads the .Renviron file on startup.
    #cat("start retrieve fom REnviron\n", file = logfile, append = TRUE)
    #cat("vars to retrieve: ", host_var, user_var, dsrc_var, "\n", file = logfile, append = TRUE)
    db_host <- Sys.getenv(host_var)
    #cat("value of db_host: ", db_host, file = logfile, append = TRUE)
    db_user <- Sys.getenv(user_var)
    db_pass <- Sys.getenv(pass_var)
    db_dsrc <- Sys.getenv(dsrc_var)
  
    # Check that the variables were actually found
    if (db_host == "" || db_user == "" || db_pass == "" || db_dsrc == "") {
      msg = paste("Database credentials not found for environment '", env, "'.\n",
      "Ensure a .Renviron file is present and contains the variable: ", host_var
      )
      #cat(msg, "\n", file = logfile, append = TRUE)
      cvars <- list(host = db_host, dsn = db_dsrc, uid = db_user, pwd = db_pass)
      #print(cvars)
      #cat(paste(unlist(cvars), collapse = "\n"), "\n", file = logfile, append = TRUE)
      stop(msg)
    }
    #cat("all enviornmental variables have a value\n", file = logfile, append = TRUE)
    cvars <- list(host = db_host, dsn = db_dsrc, uid = db_user, pwd = db_pass)
    #print(cvars)
    #cat("cvars: \n", paste(unlist(cvars), collapse = "\n"), "\n", file = logfile, append = TRUE)
 }
  
  #cat("Validating cvars before connection...\n", file = logfile, append = TRUE)
  
  # 1. Safety Check: Are any vars missing?
  if (any(sapply(cvars, is.null)) || any(sapply(cvars, is.na))) {
    #cat("ERROR: One or more connection variables are NULL or NA!\n", file = logfile, append = TRUE)
    return(NULL)
  }
  
  #cat("Attempting connection to:", cvars$host, "\n", file = logfile, append = TRUE)
  
  # 2. Use a more modern driver string if possible
  # Note: "SQL Server" is the old legacy driver. 
  # Try "ODBC Driver 17 for SQL Server" if installed.
  con <- try({
    DBI::dbConnect(
      odbc::odbc(),
      Driver   = "SQL Server", # Double check this name in ODBC Admin!
      Server   = cvars$host,
      Database = cvars$dsn,
      UID      = cvars$uid,
      PWD      = cvars$pwd,
      Port     = 1433,
      timeout  = 10 # Add a timeout so it doesn't hang forever
    )
  }, silent = FALSE)
  
  if (inherits(con, "try-error")) {
    #cat("R caught the error:", as.character(con), "\n", file = logfile, append = TRUE)
  } else {
    #cat("Finished establishment. Connection status:", DBI::dbIsValid(con), "\n", file = logfile, append = TRUE)
  }
    #cat("start with effective connecting\n", file = logfile, append = TRUE)
  # Establish and return the connection (example using DBI and odbc)
  # con <- try(DBI::dbConnect(
  #   odbc::odbc(),
  #   Driver   = "SQL Server",
  #   Server   = cvars$host,
  #   Database = cvars$dsn,
  #   UID      = cvars$uid,
  #   PWD      = cvars$pwd,
  #   Port     = 1433
  # ))
  #cat("Finished trying to establish connection\n", file = logfile, append = TRUE)
  ##cat(con,'\n------------\n' , file = logfile, append = TRUE)
  return(con)
}
