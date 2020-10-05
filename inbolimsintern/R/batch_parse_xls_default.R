#' Parse default excel bestanden
#'
#' @param file die geopend moet worden
#' @param sheet het tabblad dat ingelezen moet worden
#' @param user gebruikersnaam die de opdracht heeft gegeven
#' @importFrom readxl read_excel
#' @importFrom tidyr gather
#'
#' @return dataset
#' @export
#'
batch_parse_xls_default <- function(file,
                                    sheet = "LIMSIMPORT",
                                    user = "")
{
  print("in batch_parse_xls_default")
  errormessage = " "
  print(file)
  print(sheet)

  data <- readxl::read_excel(path = file,
                             sheet = sheet) %>%
    mutate(`...` = NA)
  print("after xlread")
  print(data)
  data <- select(data, 2:(starts_with("..")[1]-1)) #kolom A bevat ID die we niet gebruiken
  print(data)
  data <- data[which(!is.na(as.numeric(data[[1]]))), , drop = FALSE]
  data <- data %>% arrange_at(1)
  print("before gathering")
  gathered_data <- data %>%
    tidyr::gather(key = "CName", value = "Value", -(1:3)) %>%
    arrange_at(1)
  print("finished batch_parse_xls_default")
  gathered_data
}



