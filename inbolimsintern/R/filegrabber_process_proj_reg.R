#' Process files ontrent projectregistratie
#'
#' @param source_path pad waar het bronbestand te vinden is
#' @param source_file  naam van het bronbestand (xlsx)
#' @param target_path het pad waar de tsv naartoe geschreven moet worden
#' @param finish_path het pad waar de excel file heen noet na deze processing
#' @param sheet naam van het tabblad dat ingelezen moet worden
#'
#' @return gecreerde en verplaatse bestanden
#' @export
#'
process_proj_reg <- function(source_path, source_file, target_path, finish_path, sheet){
  try(data <- read_excel(file.path(source_path, source_file),
                         sheet = sheet,
                         col_types = "text", col_names = FALSE))
  #schrijf data weg als tsv
  target_file <- paste0(source_file, "_", datetime_text(),  ".tsv")
  print(target_file)
  readr::write_tsv(data, path = target_file, col_names = FALSE, na = '')

  #verplaats de originele file
  move_file <- file.path(move_path, source_file)
  print(move_file)
  try(file.remove(move_file), silent = TRUE)
  file.rename(from = file.path(source_path, source_file), to = move_file)
  str(data)
}
