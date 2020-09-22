#' Lees file in waarbij stalen met: text_id---testnummer worden gebruikt
#'
#' @param file file die ingelezen moet worden
#' @param sheet naam van de sheet (case sensitive)
#' @param cell eerste cel
#' @param maxcol maximaal aantal kolommen
#' @param user  LIMS gebruikers naam van de inlezer
#' @importFrom readxl read_excel
#' @importFrom tidyr gather
#' @return dataset
#' @export
#'
batch_parse_xls_tripledash <- function(
  file,
  sheet = "LIMSIMPORT",
  cell = "B1",
  maxcol = 0,
  user = "TEST")
{

}
