#' Prepare a remote script session
#'
#' This high-level function handles all boilerplate setup for scripts run via Rscript.
#' It prepares the session, connects to the LIMS database, reads parameters,
#' and sets up logging and pandoc paths.
#'
#' @param args defaults to commandArgs(trailingOnly = TRUE), but user can specify a custom character vector of arguments. The first 3 arguments must be dbodbc, call_id, and user
#' @param test_mode (obsolete) if TRUE the credentials are read from the cred_file (obsolete)
#' @param cred_file (obsolete) Path to the credentials file containing (obsolete)
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
#' @export
r_session_setup <- function(args = commandArgs(trailingOnly = TRUE),
                            logfile = "lims_rsession.log") {
  
  # Minimal local logging
  #cat('\n', as.character(Sys.time()), " Setup started\n", file = logfile, append = FALSE)
  #cat("working dir: ", getwd(), "\n")
  
  #// 0. validate arguments
  #cat('\n\n', as.character(Sys.time()), "setup scripts with", args, "\n", file = logfile, append = TRUE)

  if (length(args) < 3) stop("At least 3 arguments required: odbc, call_id, user")
  
  odbc <- args[1]
  call_id   <- as.numeric(args[2])
  user_name <- args[3]
  
  # Assign based on partial matches
  env_name <- dplyr::case_when(
    grepl("PRD", odbc) ~ "PRD",
    grepl("UAT", odbc) ~ "UAT",
    grepl("DEV", odbc) ~ "DEV",
    TRUE                  ~ "UNKNOWN" # Fallback safety
  )
  #cat("env: ", env_name, "\n", file = logfile, append = TRUE)
  
  env <- try(inbolimsintern:::get_current_environment(env_name))
  if (inherits(env, "try-error")) {
    #cat(env, file = logfile, append = TRUE)
    stop(env)
  }
  
  #cat("Loading environment variables\n")
  e <- try(inbolimsintern:::load_encrypted_renviron())
  if (inherits(e, "try-error")) {
    ##cat(e, file = logfile, append = TRUE)
    stop(e)
  } else {
    ##cat(" environment variables loaded\n", file = logfile, append = TRUE)
  }
  ##cat("host: ", Sys.getenv()["DB_HOST_PRD"], "\n", file = logfile, append = TRUE)
  #cat("user: ", Sys.getenv()["DB_USER_PRD"], "\n", file = logfile, append = TRUE)
  

  
  #cat('try connecting\n', file = logfile, append = TRUE)
  
  # 2. Connect to DB 
  # Ensure limsdb_connect uses internal calls or :: for odbc/DBI
  conn <- try(limsdb_connect(env = env_name))
  #cat("Connection function finished\n", file = logfile, append = TRUE)
  if (inherits(conn, "try-error")) {
    #cat('connection failed\n', file = logfile, append = TRUE)
    #cat(conn, '\n', file = logflie, append = TRUE)
    stop("Database connection failed")    
  } else {
    message("Database Connection established")
    #cat("Database Connection established" , "\n", file = logfile, append = TRUE)
  }

  # 3. Ensure pandoc is found
  pandoc_dir <- get_lims_constant(conn, "PANDOC_DIR")
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
  Sys.setenv(PATH = paste(pandoc_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  
  #cat("pandoc set\n", file = logfile, append = TRUE)
  
  # 4. IMMEDIATE Milestone 
  # This makes the LIMS UI responsive immediately
  write_db_log(
    message = "R session setup successful",
    status = "P",
    conn = conn,
    call_id = call_id,
    user_name = user_name
  )
  
  #cat("start reading parameters\n")
  # 5. Read Parameters
  params <- try(read_db_arguments(conn, call_id))
  #cat("return values session setup:\n", unlist(params), file = logfile, append = TRUE)
  return(list(conn = conn, user_name = user_name, call_id = call_id, params = params, env = env_name))
}






# r_session_setup <- function(args = commandArgs(trailingOnly = TRUE),
#                             logfile = "lims_rsession.log",
#                             test_mode = FALSE,
#                             cred_file = "dbcredentials.txt") {
#   message("starting r session setup")
#   message("arguments: ", args)
# 
#   #// 0. validate arguments
#   #cat('\n\n', as.character(Sys.time()), "setup scripts with",
#       args, "\n", file = logfile, append = TRUE)
# 
#   if (length(args) < 3) {
#     stop("At least 3 arguments must be passed containing the odbc, call_id and user_name")
#   }
#   odbc <- args[1]
# 
#   user_name <- args[3]
#   call_id <- try(as.numeric(args[2]))
#   if(inherits(call_id, "try-error")) {
#     #cat("call_id cannot be converted to numeric\n", file = logfile, append = TRUE)
#     stop("call_id must be numeric")
#   }
# 
#   #validate cred_file type
#   if (!is.character(cred_file) || length(cred_file) != 1) {
#     stop("cred_file must be a single character string")
#   }
#   message("working dir: ", getwd())
#   env <- try(get_current_environment(odbc))
#   if (inherits(env, "try-error")) {
#     #cat(env, file = logfile, append = TRUE)
#     stop(env)
#   }
#   #cat(" environment: ", env, "\n", append = TRUE, file = logfile)
# 
#   message(env, " environment")
#   e <- try(load_encrypted_renviron())
#   if (inherits(e, "try-error")) {
#     #cat(e, file = logfile, append = TRUE)
#     stop(e)
#   } else {
#     #cat(" environment variables loaded\n", file = logfile, append = TRUE)
#   }
#   #cat(Sys.getenv()["DB_HOST_PRD"], "\n", file = logfile, append = TRUE)
#   #cat(Sys.getenv()["DB_USER_PRD"], "\n", file = logfile, append = TRUE)
#   #cat(Sys.getenv()["DB_PASS_PRD"], "\n", file = logfile, append = TRUE)
#   #cat(Sys.getenv()["DB_DSRC_PRD"], "\n", file = logfile, append = TRUE)
#   #cat("env:", env, "\n", file = logfile, append = TRUE)
# 
#   #// connect to db
#   a <- try(conn <- limsdb_connect(env = env))
#   if (inherits(a, "try-error")) #cat(a, "\n", file = logfile, append = TRUE)
# 
#   #// 2 Initiate logging
#   #read en write logs
#   lims_log_entry <- read_db_log(conn, call_id)
#   if (!nrow(lims_log_entry)) {
#     lims_log_entry <- data.frame(LOG_MESSAGE = "Undefined routine")
#   }
#   write_db_log(
#     message = paste("Started", lims_log_entry[1, "LOG_MESSAGE"]),
#     status = "P",
#     conn = conn,
#     call_id = call_id,
#     user_name = user_name)
# 
#   #// 3. Read script-specific parameters
#   params <- try(read_db_arguments(conn, call_id))
#   if (inherits(params, "try-error")) {
#     msg <- paste("parameters could not be loaded: ", params)
#     write_db_log(msg, "E", conn = conn, call_id = call_id, user_name = user_name)
#     stop(msg)
#   }
# 
#   #// 4. Configure Pandoc path
#   pandoc_dir <- get_lims_constant(conn, "PANDOC_DIR")
#   Sys.setenv(PATH = paste(pandoc_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
#   Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
# 
#   #// 5. Return all the necessary objects in a list
#   msg <- "r session startup successful"
#   write_db_log(msg, "P", conn = conn, call_id = call_id, user_name = user_name)
#   list(conn = conn, user_name = user_name, call_id = call_id, params = params, env = env)
# }
