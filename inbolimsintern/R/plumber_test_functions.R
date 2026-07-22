
################################################################################

#' Check if service is alive 
#'
#' @returns list
#' @export
#'
check_health <- function() {
  require("inbolimsintern")
  status <- "alive"
  time <- Sys.time()
  list(status = status, time = time) 
}

################################################################################

#' Simple multiplication to test service
#'
#' @param a numeric-convertible
#' @param b numeric-convertible
#'
#' @returns numeric value
#' @export
#'
check_multiply <- function(a, b) {
  as.numeric(a) * as.numeric(b)
}

################################################################################

#' Check if db connection is alive
#'
#' @param env PRD, UAT of DEV
#'
#' @returns list with result of check
#' @export
check_db <- function(env = "PRD") {
    if (!exists("global_conn") || is.null(global_conn) || !DBI::dbIsValid(global_conn)) {
      global_conn <- try(inbolimsintern::limsdb_connect(env = "PRD"))
    }
    if (inherits(global_conn, "try-error")) {
      return(list(message = paste("Connection problem:", global_conn)))
    }
  
    is_valid <- DBI::dbIsValid(global_conn)
    if (is_valid) {
      list(
        message = "Database check completed successfully",
        environment = env,
        timestamp = Sys.time()
      )
    } else {
      list(message = "Database connection is not valid",
           environment = env,
           timestamp = Sys.time()
      )
    }
}

################################################################################