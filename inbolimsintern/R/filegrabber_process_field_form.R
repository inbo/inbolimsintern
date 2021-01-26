#' Process files omtrent inlezen veldformulier
#'
#' @param source_path pad waar het bronbestand te vinden is
#' @param source_file  naam van het bronbestand (xlsx)
#' @param target_path het pad waar de tsv naartoe geschreven moet worden
#' @param finish_path het pad waar de excel file heen noet na deze processing
#' @param sheet naam van het tabblad dat ingelezen moet worden
#' @importFrom utils str
#'
#' @return gecreerde en verplaatse bestanden
#' @export
#'
process_field_form <- function(source_path, source_file, target_path, finish_path, sheet){
  try(data <- read_excel(file.path(source_path, source_file),
                         sheet = sheet,
                         col_types = "text", col_names = FALSE))
  #schrijf data weg als tsv
  target_file <- paste0(source_file, "_", datetime_text(),  ".tsv")
  print(target_file)
  readr::write_tsv(data, path = target_file, col_names = FALSE, na = '')

  #verplaats de originele file
  finish_file <- file.path(finish_path, source_file)
  print(finish_file)
  try(file.remove(finish_file), silent = TRUE)
  file.rename(from = file.path(source_path, source_file), to = finish_file)
  str(data)
}
