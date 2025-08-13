#' Get a value from system configuration tables
#'
#' Searches for a named value first in LIMS_CONSTANTS and then in CONFIG_SYS.
#'
#' @param conn odbc connection object
#' @param name The name of the constant or entry_code to retrieve.
#' @returns Character value of the first match found, or NULL.
#' @importFrom DBI dbGetQuery
#' @examples
#' \dontrun{
#' # This will search for 'DEFAULTDIR_LAB_GENERIC' in both tables
#' get_system_value(conn, "DEFAULTDIR_LAB_GENERIC")
#' }
#' @export
get_lims_constant <- function(conn, name) {
  # This SQL query combines results from two tables.
  # UNION ALL is faster than UNION because it doesn't check for duplicates.
  qry <- '
    SELECT CONSTANT_VALUE AS value FROM LIMS_CONSTANTS WHERE NAME = ?
    UNION ALL
    SELECT "VALUE" AS value FROM CONFIG_SYS WHERE ENTRY_CODE = ?
  '

  # Using params is safer and avoids SQL injection.
  # The 'name' variable is passed for each '?' placeholder.
  rv <- DBI::dbGetQuery(conn, qry, params = list(name, name))

  if (nrow(rv) > 0) {
    # Return the value from the first row found
    return(rv$value[1])
  } else {
    warning("Constant/config not found: '", name, "'")
    return(NULL)
  }
}
