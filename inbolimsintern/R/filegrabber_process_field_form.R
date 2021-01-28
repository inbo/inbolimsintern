#' Process files omtrent inlezen veldformulier
#'
#' @param source_path pad waar het bronbestand te vinden is
#' @param source_file  naam van het bronbestand (xlsx)
#' @param target_path het pad waar de tsv naartoe geschreven moet worden
#' @param finish_path het pad waar de excel file heen noet na deze processing
#' @param sheet naam van het tabblad dat ingelezen moet worden
#' @param digits hoeveel digits behouden bij het bewaren van nummerieke waarden
#' @importFrom utils str
#'
#' @return gecreerde en verplaatse bestanden
#' @export
#'
process_field_form <- function(source_path, source_file, target_path, finish_path, sheet, digits = 12){
  try(data <- read_excel(file.path(source_path, source_file),
                         sheet = sheet, n_max = 500,
                         range = "A1:P500",
                         col_types = c(rep("text", 3), "date", rep("text",12)), col_names = FALSE))
  data <- as.data.frame(data[!is.na(data[,1]), ], stringsAsFactors = FALSE)

  for (col in 1:ncol(data)) {
    if (col == 4) {
      next #kolom 4 is datumtijd en moet (mag) niet omgezet worden naar numeriek
    }
    for (row in 1:nrow(data)) {
      waarde <- data[row, col]
      try(waarde <- as.numeric(data[row, col])) #waarde wordt niet geupdated bij fouten zoals bij datetime die een origin verwacht
      if (!is.na(waarde) & is.numeric(waarde)) {
        data[row, col] <- round(waarde, digits)
      }
    }
  }
  #schrijf data weg als tsv
  print(head(data, 10))
  target_file <- paste0(source_file, "_", datetime_text(),  ".tsv")
  print(target_file)
  readr::write_tsv(data, path = file.path(target_path, target_file), col_names = FALSE, na = '')

  #verplaats de originele file
  finish_file <- file.path(finish_path, source_file)
  print(finish_file)
  try(file.remove(finish_file), silent = TRUE)
  file.rename(from = file.path(source_path, source_file), to = finish_file)
  str(data)
}
