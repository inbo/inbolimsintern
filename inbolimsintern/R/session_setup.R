#' Prepare a remote script session
#'
#' This high-level function handles all boilerplate setup for scripts run via Rscript.
#' It prepares the session, connects to the LIMS database, reads parameters,
#' and sets up logging and pandoc paths.
#'
#' @param call_id The unique identifier for the script call, passed via command line.
#' @return A list containing essential objects for the script:
#'   \itemize{
#'     \item \code{conn}: The DBI database connection object.
#'     \item \code{params}: A list of parameters read from the database.
#'     \item \code{logfile}: The full path to the initialized log file.
#'   }
#' @importFrom DBI dbConnect
#' @importFrom utils Sys.setenv
#' @export
session_setup <- function(call_id) {
  # 1. Get session arguments (dsn, uid, pwd)
  args <- prepare_session(call_id)

  # 2. Connect to the database
  conn <- limsdb_connect(env = args["dsn"],
                         uid = args["uid"],
                         pwd = args["pwd"])

  # 3. Read script-specific parameters
  params <- read_db_arguments(conn, args["call_id"])

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
