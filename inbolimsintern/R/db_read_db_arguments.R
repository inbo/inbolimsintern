
#' Lees argumenten uit de database
#'
#' @param conn connection object to the LIMS database
#' @param call_id numeric identifier of the function you want to get the arguments from
#'
#' @return named list with arguments
#' @export
#'
#' @examples
#' \dontrun{
#' conn = lims_db_connect(uid = "ikke", pwd = "123456")
#' read_db_arguments(conn, call_id = 10)
#' }
read_db_arguments <- function(conn, call_id){
  q = paste0("select CALL_ID, CALL_FUN, ARG_NAME, VALUE from C_RSCRIPT_ARGS where CALL_ID = ", call_id)
  rv <- DBI::dbGetQuery(conn, q)
  
}
