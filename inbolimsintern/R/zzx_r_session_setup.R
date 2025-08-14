#' Prepare a remote script session
#'
#' This high-level function handles all boilerplate setup for scripts run via Rscript.
#' It prepares the session, connects to the LIMS database, reads parameters,
#' and sets up logging and pandoc paths.
#'
#' @param call_id The unique identifier for the script call, passed via command line.
#' @param args defaults to commandArgs(), but user can specify a custom character vector of arguments
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
#'     \item \code{logfile}: The full path to the initialized log file.
#'   }
#' @importFrom DBI dbConnect
#' @export
r_session_setup <- function(call_id,
                            args = commandArgs(trailingOnly = TRUE),
                            cred_file = "dbcredentials.txt") {

  # 0. Validate input

  if (!is.null(call_id) && (!is.numeric(call_id) || length(call_id) != 1)) {
    stop("call_id must be a single numeric value or NULL")
  }

  #validate cred_file type
  if (!is.character(cred_file) || length(cred_file) != 1) {
    stop("cred_file must be a single character string")
  }

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
    argvec <- c(
      dsn     = as.character(creds$dsn),
      uid     = as.character(creds$uid),
      pwd     = as.character(creds$pwd),
      call_id = as.character(call_id))
  } else {
    if (length(args) < 4) stop("there should be at least 4 arguments")
    if (args[4] != as.character(call_id)) stop("conflicting call_id")
    argvec <- c(
      dsn     = as.character(args[1]),
      uid     = as.character(args[2]),
      pwd     = as.character(args[3]),
      call_id = as.character(args[4])
    )
  }
  message("arguments loaded for: ", argvec["call_id"])

  # 2. Connect to the database
  conn <- limsdb_connect(env = args["dsn"],
                         uid = args["uid"],
                         pwd = args["pwd"])

  # 3. Read script-specific parameters
  params <- read_db_arguments(conn, argvec["call_id"])

  # 4. Set up logging directory and file
  log_dir <- get_lims_constant(conn, "DYNAMIC_DIR")
  log_dir <- sub("\\DYNAMIC\\", "\\LOGS\\", log_dir, fixed = TRUE)
  logfile <- logfile_start(path = log_dir, prefix = "ELC_Shewhart") # You might want the prefix to be dynamic

  # 5. Configure Pandoc path
  pandoc_dir <- get_lims_constant(conn, "PANDOC_DIR")
  Sys.setenv(PATH = paste(pandoc_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)

  # 6. Return all the necessary objects in a list
  return(
    list(
      conn = conn,
      params = params,
      logfile = logfile
    )
  )
}
