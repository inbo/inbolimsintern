#' Parse default excel bestanden
#'
#' @param file file die geopend moet worden
#' @param sheet het tabblad dat ingelezen moet worden
#' @param cell de eerste cel die wordt ingelezen
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
                                    user = "")
{
  errormessage = " "
  data <- readxl::read_excel(path = file,
                             sheet = sheet,
                             range = paste(cell, "AZ10000", sep = ":")) %>%
    mutate(`...` = NA)
  data <- select(data, 1:(starts_with("..")[1]-1))
  data <- data[which(!is.na(as.numeric(data[[1]]))), drop = FALSE]
  data <- data %>% arrange_at(1)
  gathered_data <- data %>%
    tidyr::gather(key = "CName", value = "Value", -(1:3)) %>%
    arrange_at(1)
  gathered_data
}



