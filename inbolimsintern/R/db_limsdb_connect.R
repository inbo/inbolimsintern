#' Connect to LIMS Database with Multi-Environment Fallback
#'
#' Attempts to establish a connection to the LIMS database by trying multiple
#' environments in sequence (LW7, Production, UAT, Development) until a
#' successful connection is made. This provides automatic failover capability
#' when working across different LIMS environments.
#'
#' @param env Character string. Name of the LIMS database odbc object.
#'   Default is "D0015_00_Lims" (main production database LW7).
#'   Other possibilities are LWL8DEV, LWL8UAT and LWL8PRD
#' @param uid Character string. Application username that has write permissions
#'   on the database. This should be an application service account, not a
#'   personal username.
#' @param pwd Character string. Password for the application username.
#' @param connectlist Named list containing connection parameters that override
#'   individual arguments. Must contain "dsn" (server name), "uid" (user ID),
#'   and "pwd" (password). Optionally can contain "db" (database name).
#'   When provided, this takes precedence over individual parameters.
#'
#' @return A DBI connection object if successful, or a character string
#'   "No database connection could be established" if all connection attempts fail.
#'
#' @details
#' The function attempts connections in the following order:
#' 1. **LW7 Environment**: inbo-sql07-prd.inbo.be / LWL8PRD
#' 2. **Production Environment**: inbo-sql07-prd.inbo.be / D0015_10_Lims
#' 3. **UAT Environment**: inbo-sql06-uat.inbouat.be / LWL8UAT
#' 4. **Development Environment**: inbo-sql05-dev.inbodev.be / LWL8DEV
#'
#' The function returns immediately upon the first successful connection.
#' If all environments fail, it returns an error message string.
#'
#' **Note**: The `server` and `database` parameters are only used when
#' `connectlist` is NULL. When `connectlist` is provided, its values
#' take precedence and the multi-environment fallback is bypassed.
#'
#' @section Security Considerations:
#' - Use application service accounts, not personal credentials
#' - Consider storing credentials securely rather than hardcoding
#' - Ensure proper database permissions for the application account
#'
#' @examples
#' \dontrun{
#' # Basic connection with fallback across environments
#' con <- limsdb_connect(env = "lims_env", uid = "lims_app_user", pwd = "secure_password")
#'
#' # Check if connection was successful
#' if (is.character(con)) {
#'   stop("Failed to connect to any LIMS environment: ", con)
#' }
#'
#' # Using connection list (typical with prepare_session function)
#' creds <- list(
#'   dsn = "inbo-sql07-prd.inbo.be",
#'   uid = "app_user",
#'   pwd = "password",
#'   db = "D0015_00_Lims"
#' )
#' con <- limsdb_connect(connectlist = creds)
#'
#' # Integration with prepare_session
#' session_args <- prepare_session(call_id = 5, cred_file = "credentials.txt")
#' con <- limsdb_connect(connectlist = session_args)
#'
#' # Always close connection when done
#' if (!is.character(con)) {
#'   DBI::dbDisconnect(con)
#' }
#'
#' # Example of invalid credentials (should fail gracefully)
#' con_fail <- limsdb_connect(env = "lims_env", uid = "invalid_user", pwd = "wrong_password")
#' print(con_fail)  # Should print error message
#' }
#'
#' @seealso
#' \code{\link[DBI]{dbConnect}} for database connection details
#' \code{\link[odbc]{odbc}} for ODBC driver information
#'
#' @export
limsdb_connect <- function(env,
                           uid,
                           pwd,
                           connectlist = NULL) {

  # Validate required parameters
  if ((missing(uid) || missing(pwd)) && is.null(connectlist)) {
    stop("Both uid and pwd parameters are required when not using connectlist")
  }

  # Environment configurations
  env_names <-
    list(D0015_10_Lims = c(server = "inbo-sql07-prd.inbo.be",
                           db = "D0015_10_Lims"),
         LWL8PRD =       c(server = "inbo-sql07-prd.inbo.be",
                           db = "LWL8PRD"),
         LWL8UAT =       c(server = "inbo-sql06-uat.inbouat.be",
                           db = "LWL8UAT"),
         LWL8DEV =       c(server = "inbo-sql05-dev.inbodev.be",
                           db = "LWL8DEV"))

  # Handle connectlist
  if (!is.null(connectlist)) {
    if (!is.list(connectlist)) {
      stop("connectlist must be a list")
    }

    required_fields <- c("uid", "pwd", "dsn")
    if (!all(required_fields %in% names(connectlist))) {
      missing_fields <- setdiff(required_fields, names(connectlist))
      stop("connectlist missing required fields: ", paste(missing_fields, collapse = ", "))
    }

    uid <- connectlist$uid
    pwd <- connectlist$pwd
    server <- connectlist$dsn
    if (!is.null(connectlist$db)) {
      database <- connectlist$db
    }

    # When connectlist is provided, use those specific parameters
    message("Using provided connection parameters: ", server, "/", database)
    con <- try(DBI::dbConnect(odbc::odbc(),
                              Driver = "SQL Server",
                              Server = server,
                              Database = database,
                              uid = unname(as.character(uid)),
                              pwd = unname(as.character(pwd)),
                              timeout = 10),
               silent = TRUE)

    if (inherits(con, "try-error")) {
      warning("Connection failed with provided parameters: ", attr(con, "condition")$message)
      return("No database connection could be established")
    }

    message("Successfully connected to: ", server, "/", database)
    return(con)
  }

  # Volgens gekend odbc object
  env_vars <- env_names[[env]]
  if (!length(env_vars)) {
    # All connection attempts failed
    warning("Environment not known")
    return("No database connection because of environment not known")
  }


  print(env_vars)
  con <- try(DBI::dbConnect(odbc::odbc(),
                            Driver = "SQL Server",
                            Server = env_vars["server"],
                            Database = env_vars["db"],
                            uid = unname(as.character(uid)),
                            pwd = unname(as.character(pwd)),
                            timeout = 10),
             silent = TRUE)

    if (!inherits(con, "try-error")) {
      message("Successfully connected to ", env, " environment")
      return(con)
    } else {
      message("Failed to connect to ", env, " environment: ",
              attr(con, "condition")$message)
      return(NULL)
    }

  # All connection attempts failed
  warning("Failed to connect to all LIMS environments")
  return("No database connection could be established")
}

####################################################################
#### Original coding
####################################################################
#'

#' #' Title
#' #'
#' #' @param server name of the LIMS server
#' #' @param database name of the LIMS database
#' #' @param uid application username (that as writing rights on the db, so this is not you username)
#' #' @param pwd applciation username password
#' #' @param connectlist list containing dsn (server name), uid (user id) and pwd (password). Optionally it can also contain db. It overrides the values of the other arguments
#' #'
#' #' @return db connection
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' lims_db_connect(uid = "me", pwd = "123456") #should not work
#' #' }
#' limsdb_connect <- function(server = "inbo-sql07-prd.inbo.be",
#'                            database = "D0015_00_Lims",
#'                            uid,
#'                            pwd,
#'                            connectlist = NULL) {
#'   env <-
#'     list(lw7_env = c(server = "inbo-sql07-prd.inbo.be", db = "LWL8PRD"),
#'          prd_env = c(server = "inbo-sql07-prd.inbo.be", db = "D0015_10_Lims"),
#'          uat_env = c(server = "inbo-sql06-uat.inbo.be", db = "LWL8UAT"),
#'          dev_env = c(server = "inbo-sql05-dev.inbo.be", db = "LWL8DEV"))
#'
#'
#'   #overrides all arguments
#'   if  (!is.null(connectlist)) {
#'     if (is.list(connectlist) &
#'         all(c("uid", "pwd", "dsn") %in% names(connectlist))) {
#'       uid <- connectlist$uid
#'       pwd <- connectlist$pwd
#'       server <- connectlist$dsn
#'       if (!is.null(connectlist$db)) database <- connectlist$db
#'     }
#'   }
#'
#'   #lw7
#'   con_lw7 <- try(DBI::dbConnect(odbc::odbc(),
#'                         Driver = "SQL Server",
#'                         Server = env$lw7_env["server"],
#'                         Database = env$lw7_env["db"],
#'                         uid = unname(uid),
#'                         pwd = unname(pwd)))
#'   if (!inherits(con_lw7, "try-error")) return(con_lw7)
#'
#'   #prd
#'   con_prd <- try(DBI::dbConnect(odbc::odbc(),
#'                                 Driver = "SQL Server",
#'                                 Server = env$prd_env["server"],
#'                                 Database = env$prd_env["db"],
#'                                 uid = unname(uid),
#'                                 pwd = unname(pwd)))
#'   if (!inherits(con_prd, "try-error")) return(con_prd)
#'
#'   #uat
#'   con_uat <- try(DBI::dbConnect(odbc::odbc(),
#'                                 Driver = "SQL Server",
#'                                 Server = env$uat_env["server"],
#'                                 Database = env$uat_env["db"],
#'                                 uid = unname(uid),
#'                                 pwd = unname(pwd)))
#'   if (!inherits(con_uat, "try-error")) return(con_uat)
#'
#'   #dev
#'   con_dev <- try(DBI::dbConnect(odbc::odbc(),
#'                                 Driver = "SQL Server",
#'                                 Server = env$dev_env["server"],
#'                                 Database = env$dev_env["db"],
#'                                 uid = unname(uid),
#'                                 pwd = unname(pwd)))
#'   if (!inherits(con_dev, "try-error")) return(con_dev)
#'
#'   return("No database connection could be established")
#' }
#'
