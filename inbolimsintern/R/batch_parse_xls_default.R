#' Parse default excel bestanden
#'
#' @param file file die geopend moet worden
#' @param sheet het tabblad dat ingelezen moet worden
#' @param cell de eerste cel die wordt ingelezen
#' @param maxcol het maximaal aantal kolommen die gebruikt wordt
#' @param user gebruikersnaam die de opdracht heeft gegeven
#' @importFrom readxl read_excel
#' @importFrom tidyr gather
#'
#' @return dataset
#' @export
#'
batch_parse_xls_default <- function(file,
                                    sheet = "LIMSIMPORT",
                                    cell = "B1",
                                    maxcol = 0,
                                    user = "")
{
  errormessage = " "
  data <- readxl::read_excel(path = file,
                             sheet = sheet,
                             range = paste(cell, "AA10000", sep = ":"))
  data <- data[which(!is.na(as.numeric(data[[1]]))), 1:maxcol, drop = FALSE]
  data <- data %>% arrange_at(1)
  gathered_data <- data %>%
    tidyr::gather(key = "CName", value = "Value", -(1:3)) %>%
    arrange_at(1)
  gathered_data
}



