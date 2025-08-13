#' Prepare Database Session Arguments
#'
#' Extracts and prepares database connection arguments for R scripts, supporting
#' both production mode (with command line arguments) and interactive testing mode
#' (with credential files). The function automatically detects the execution context
#' and retrieves the appropriate database credentials and script parameters.
#'
#' @param call_id Integer. The call identification number used to retrieve
#'   script-specific arguments from the LIMS database (table C_RSCRIPT_ARGS).
#'   Required when running in test/interactive mode.
#' @param args Character vector. Command line arguments passed to the script.
#'   Default is `commandArgs()`. In production, these typically include the
#'   R script path, data source, username, password, and call_id.
#' @param min_args Integer. Minimum number of arguments required to determine
#'   if this is a production call vs. interactive testing. Default is 5.
#' @param first_arg Integer. Position of the first meaningful argument in the
#'   args vector. This accounts for R's internal arguments. Default equals min_args.
#' @param cred_file Character string. Path to the credentials file containing
#'   database connection information. Used only in interactive/test mode.
#'   The file should contain three lines: data source, username, and password.
#'   Default is "dbcredentials.txt".
#'
#' @return Named character vector with the following elements:
#'   \describe{
#'     \item{dsn}{Data source name for database connection}
#'     \item{uid}{Username for database authentication}
#'     \item{pwd}{Password for database authentication}
#'     \item{call_id}{Call identification number for retrieving script arguments}
#'   }
#'
#' @details
#' The function operates in two modes:
#'
#' **Production Mode**: When `length(args) >= min_args`, arguments are extracted
#' from the command line. The expected argument structure is:
#' - args[1:min_args]: R internal arguments
#' - args[min_args + 1]: Data source name
#' - args[min_args + 2]: Username
#' - args[min_args + 3]: Password
#' - args[min_args + 4]: Call ID
#'
#' **Interactive/Test Mode**: When `length(args) < min_args`, credentials are
#' read from the specified credential file, and call_id must be provided as
#' a parameter.
#'
#' @section Error Handling:
#' - Stops execution if credential file cannot be read in test mode
#' - Stops execution if call_id is NULL in test mode
#' - Provides informative error messages in Dutch for LIMS integration
#'
#' @examples
#' \dontrun{
#' # Interactive/test mode - reading from credential file
#' cred_file <- system.file("extdata", "dbcredentials_example.txt", package = "inbolimsintern")
#' args_test <- prepare_session(call_id = 5, cred_file = cred_file)
#' print(args_test)
#' # Returns: c(dsn = "LIMS_DB", uid = "user123", pwd = "pass456", call_id = "5")
#'
#' # Production mode - using command line arguments
#' # Simulating command line args: c("Rscript", "script.R", "LIMS_DB", "user", "pass", "5")
#' mock_args <- c("R_EXEC_LOC", "--no-echo", "--no-restore", "--file=testje.r", "--args", "LIMS_DB", "user123", "pass456", "5")
#' args_prod <- prepare_session(args = mock_args)
#' print(args_prod)
#' # Returns: c(dsn = "LIMS_DB", uid = "username", pwd = "password", call_id = "5")
#'
#' # Custom minimum arguments
#' args_custom <- prepare_session(
#'   call_id = 15,
#'   min_args = 6,
#'   cred_file = "my_credentials.txt"
#' )
#'
#' # Using the returned arguments for database connection
#' session_args <- prepare_session(call_id = 5, cred_file = cred_file)
#' # Later in your script:
#' # con <- DBI::dbConnect(odbc::odbc(),
#' #                       dsn = session_args["dsn"],
#' #                       uid = session_args["uid"],
#' #                       pwd = session_args["pwd"])
#' }
#'
#' @seealso
#' \code{\link[inbolimsintern]{read_db_credentials}} for credential file reading
#' \code{\link[base]{commandArgs}} for command line argument handling
#'
#' @export
prepare_session <- function(call_id = NULL,
                            args = commandArgs(),
                            min_args = 5L,
                            first_arg = min_args,
                            cred_file = "dbcredentials.txt") {

  # Validate inputs
  if (!is.null(call_id) && (!is.numeric(call_id) || length(call_id) != 1)) {
    stop("call_id must be a single numeric value or NULL")
  }

  if (!is.numeric(min_args) || length(min_args) != 1 || min_args < 1) {
    stop("min_args must be a single positive integer")
  }

  if (!is.character(cred_file) || length(cred_file) != 1) {
    stop("cred_file must be a single character string")
  }

  # Determine execution mode
  is_test_mode <- length(args) < min_args

  if (is_test_mode) {
    # Interactive/Test mode: read from credentials file
    message("Running in test/interactive mode - reading credentials from file")

    if (is.null(call_id)) {
      stop("call_id moet een waarde hebben bij het testen (call_id must have a value when testing)")
    }

    # Attempt to read credentials
    creds <- try(inbolimsintern::read_db_credentials(cred_file), silent = TRUE)

    if (inherits(creds, "try-error")) {
      stop("Database info niet gevonden, zorg dat cred_file verwijst naar een bestaand bestand\n",
           "(Database info not found, ensure cred_file points to an existing file)\n",
           "Attempted file: ", cred_file)
    }

    # Validate credentials structure
    required_fields <- c("dsn", "uid", "pwd")
    if (!all(required_fields %in% names(creds))) {
      stop("Credential file must contain fields: ", paste(required_fields, collapse = ", "))
    }

    argvec <- c(
      dsn = as.character(creds$dsn),
      uid = as.character(creds$uid),
      pwd = as.character(creds$pwd),
      call_id = as.character(call_id)
    )

  } else {
    # Production mode: extract from command line arguments
    message("Running in production mode - extracting credentials from command line arguments")

    # Validate sufficient arguments
    required_positions <- (min_args + 1):(min_args + 4)
    if (length(args) < max(required_positions)) {
      stop("Insufficient command line arguments. Expected at least ",
           max(required_positions), " arguments, got ", length(args))
    }

    argvec <- c(
      dsn = as.character(args[min_args + 1]),
      uid = as.character(args[min_args + 2]),
      pwd = as.character(args[min_args + 3]),
      call_id = as.character(args[min_args + 4])
    )
  }

  # Validate final output
  if (any(is.na(argvec)) || any(nchar(argvec) == 0)) {
    warning("Some extracted arguments are empty or NA")
  }

  message("Session prepared with call_id: ", argvec["call_id"])

  return(argvec)
}
