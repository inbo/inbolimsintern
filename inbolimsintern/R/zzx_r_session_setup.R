#' Prepare a remote script session
#'
#' This high-level function handles all boilerplate setup for scripts run via Rscript.
#' It prepares the session, connects to the LIMS database, reads parameters,
#' and sets up logging and pandoc paths.
#'
#' @param args defaults to commandArgs(trailingOnly = TRUE), but user can specify a custom character vector of arguments. The first 3 arguments must be dbodbc, call_id, and user
#' @param test_mode if TRUE the credentials are read from the cred_file
#' @param cred_file Path to the credentials file containing
#'   database connection information. Used only in interactive/test mode.
#'   The file should contain at least three lines containing
#'   and just one column containing: data source, username, and password
#'   in this exact order
#'   Default is "dbcredentials.txt".
#' @param logfile logfile to write to, because this routine is ran before the databae connection is established and the logs can be saved there

#' @return A list containing essential objects for the script:
#'   \itemize{
#'     \item \code{conn}: The DBI database connection object.
#'     \item \code{params}: A list of parameters read from the database.
#'   }
#' @importFrom DBI dbConnect
#' @export
r_session_setup <- function(args = commandArgs(trailingOnly = TRUE),
                            test_mode = FALSE,
                            cred_file = "dbcredentials.txt",
                            logfile = "lims_rsession.log") {
  message("starting r session setup")
  message("arguments: ", args)

  #// 0. validate arguments
  cat('\n\n', as.character(Sys.time()), "setup scripts with",
      args, "\n", file = logfile, append = TRUE)

  if (length(args) < 3) {
    stop("At least 3 arguments must be passed containing the odbc, call_id and user_name")
  }
  odbc <- args[1]

  user_name <- args[3]
  call_id <- try(as.numeric(args[2]))
  if(inherits(call_id, "try-error")) {
    cat("call_id cannot be converted to numeric\n", file = logfile, append = TRUE)
    stop("call_id must be numeric")
  }

  #validate cred_file type
  if (!is.character(cred_file) || length(cred_file) != 1) {
    stop("cred_file must be a single character string")
  }
  message("working dir: ", getwd())
  env <- try(get_current_environment(odbc))
  if (inherits(env, "try-error")) {
    cat(env, file = logfile, append = TRUE)
    stop(env)
  }

  message(env, " environment")
  e <- try(load_encrypted_renviron())
  if (inherits(e, "try-error")) {
    cat(e, file = logfile, append = TRUE)
    stop(e)
  }

  #// 1 get the arguments
  #validate if is test_mode
  if (test_mode) {
    creds <- try(inbolimsintern::read_db_credentials(cred_file), silent = TRUE)
    if (inherits(creds, "try-error")) {
      errmsg <- paste("Database info niet gevonden, zorg dat cred_file verwijst naar een bestaand bestand\n",
                      "(Database info not found, ensure cred_file points to an existing file)\n",
                      "Attempted file: ", cred_file, "\n")
      cat(errmsg, file = logfile, append = TRUE)
      stop(errmsg)
    }
    arglist <- list(
      host    = as.character(creds$host),
      dsn     = as.character(creds$dsn),
      uid     = as.character(creds$uid),
      pwd     = as.character(creds$pwd),
      call_id = as.character(call_id),
      user    = "TEST")
    conn <- limsdb_connect(connectlist = arglist)
  } else {
    conn <- limsdb_connect(env = env)
  }
  if (is.character(conn)) {
    cat("db connection failed: ", conn, file = logfile, append = TRUE)
    stop(paste("db connection failed: ", conn))
  } else {
    message("database connection established")
  }

  #// 2 Initiate logging
  #read en write logs
  lims_log_entry <- read_db_log(conn, call_id)
  if (!nrow(lims_log_entry)) {
    lims_log_entry <- data.frame(LOG_MESSAGE = "Undefined routine")
  }
  write_db_log(
    message = paste("Started", lims_log_entry[1, "LOG_MESSAGE"]),
    status = "P",
    conn = conn,
    call_id = call_id,
    user_name = user_name)

  #// 3. Read script-specific parameters
  params <- try(read_db_arguments(conn, call_id))
  if (inherits(params, "try-error")) {
    msg <- paste("parameters could not be loaded: ", params)
    write_db_log(msg, "E", conn = conn, call_id = call_id, user_name = user_name)
    stop(msg)
  }

  #// 4. Configure Pandoc path
  pandoc_dir <- get_lims_constant(conn, "PANDOC_DIR")
  Sys.setenv(PATH = paste(pandoc_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)

  #// 5. Return all the necessary objects in a list
  msg <- "r session startup successful"
  write_db_log(msg, "P", conn = conn, call_id = call_id, user_name = user_name)
  list(conn = conn, user_name = user_name, call_id = call_id, params = params, env = env)
}
