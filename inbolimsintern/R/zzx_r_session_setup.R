#' Prepare a remote script session
#'
#' This high-level function handles all boilerplate setup for scripts run via Rscript.
#' It prepares the session, connects to the LIMS database, reads parameters,
#' and sets up logging and pandoc paths.
#'
#' @param call_id The unique identifier for the script call, passed via command line as commandArgs()[2]
#' @param odbc The odbc that is given as commandArgs()[1] to define environment
#' @param args defaults to commandArgs(trailingOnly = TRUE), but user can specify a custom character vector of arguments. The first 4 arguments must be dbodbc, dbuid, dbpwd and call_id, optionally the fifth should be user
#' @param cred_file Path to the credentials file containing
#'   database connection information. Used only in interactive/test mode.
#'   The file should contain at least three lines containing
#'   and just one column containing: data source, username, and password
#'   in this exact order
#'   Default is "dbcredentials.txt".
#' @return A list containing essential objects for the script:
#'   \itemize{
#'     \item \code{conn}: The DBI database connection object.
#'     \item \code{params}: A list of parameters read from the database.
#'   }
#' @importFrom DBI dbConnect
#' @export
r_session_setup <- function(call_id,
                            odbc,
                            args = commandArgs(trailingOnly = TRUE),
                            cred_file = "dbcredentials.txt") {
  message("starting setup scripts")

  # 0. Validate input
  if (is.null(odbc) || is.na(odbc) || length(odbc) != 1) {
    stop("odbc must be defined")
  }
  if (is.null(call_id) || is.na(call_id) || length(call_id) != 1) {
    stop("call_id must have exactly 1 value, it is ", call_id)
  }

  if (inherits(call_id, "try-error") || is.na(call_id) || !length(call_id)) {
    stop("call_id must be convertible to a numeric value, it is ", call_id)
  }
  #validate cred_file type
  if (!is.character(cred_file) || length(cred_file) != 1) {
    stop("cred_file must be a single character string")
  }

  env <- get_current_environment(odbc)
  load_encrypted_renviron()
  call_id <- as.numeric(call_id)

  # 1 get the arguments
  #validate if there are command arguments (= call from LIMS, not interactive)
  test_mode <- ifelse(!length(args), TRUE, FALSE)

  if(test_mode) {
    creds <- try(inbolimsintern::read_db_credentials(cred_file), silent = TRUE)
    if (inherits(creds, "try-error")) {
      stop("Database info niet gevonden, zorg dat cred_file verwijst naar een bestaand bestand\n",
           "(Database info not found, ensure cred_file points to an existing file)\n",
           "Attempted file: ", cred_file)
    }
    arglist<- list(
      host    = as.character(creds$host),
      dsn     = as.character(creds$dsn),
      uid     = as.character(creds$uid),
      pwd     = as.character(creds$pwd),
      call_id = as.character(call_id),
      user    = "TEST")
    conn <- limsdb_connect(connectlist = arglist)
  } else {
    if (length(args) < 2) stop("there should be at least 2 arguments odbc and call_id")
    if (args[2] != as.character(call_id)) stop("conflicting call_id")
    conn <- limsdb_connect(env = env)
  }

  if (is.character(conn)) {
    stop(paste("db connection failed"))
  } else {
    message("database connection established")
  }

  # 3. Read script-specific parameters
  params <- read_db_arguments(conn, call_id)

  # 5. Configure Pandoc path
  pandoc_dir <- get_lims_constant(conn, "PANDOC_DIR")
  Sys.setenv(PATH = paste(pandoc_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)

  # 6. Return all the necessary objects in a list
  message("end setup script")

  list(conn = conn, params = params, env = env)
}


#If logging via text is wanted, add this and return the logfile
  # 4. Set up logging directory and file
  # log_dir <- get_lims_constant(conn, "DYNAMIC_DIR")
  # log_dir <- sub("\\DYNAMIC\\", "\\LOGS\\", log_dir, fixed = TRUE)
  # logfile <- logfile_start(path = log_dir, prefix = "ELC_Shewhart") # You might want the prefix to be dynamic
