#' Process files ontrent staalregistratie
#'
#' @param source_path pad waar het bronbestand te vinden is
#' @param source_file  naam van het bronbestand (xlsx)
#' @param target_path het pad waar de tsv naartoe geschreven moet worden
#' @param finish_path het pad waar de excel file heen noet na deze processing
#' @param sheet_samp naam van het tabblad met de staalinformatie
#' @param sheet_ana naam van het tabblad met de te loggen analyses
#' @importFrom utils str
#'
#' @return gecreerde en verplaatse bestanden
#' @export
#'
process_samp_reg <- function(source_path, source_file, target_path, finish_path, sheet_samp, sheet_ana){
  print("start routine")
  data_samp <- read_excel(file.path(source_path, source_file),
                         sheet = sheet_samp,
                         range = "A1:X500",
                         col_types = c(rep("text",10), "date", rep("text", 5), "date", rep("text", 7)),
                         col_names = FALSE)
  data_ana <- read_excel(file.path(source_path, source_file),
                         sheet = sheet_ana,
                         col_types = "text", col_names = FALSE)
  print("in sampreg")
  print(data_samp)
  print(str(data_samp))
  whi_empty <- which((data_samp[,2] == "ProjectID?"))
  if (length(whi_empty)) {
    whi_empty <- whi_empty[1]
    data_samp <- data_samp[1:(whi_empty - 1), , drop = FALSE]
    data_ana <- data_ana[1:(whi_empty - 1), , drop = FALSE]
  }
  try(project_naam <- data_samp[5,2])
  print(project_naam)

  filedate <- datetime_text()
  #schrijf data weg als tsv
  target_file_samp <- paste0(project_naam, "_", filedate,  "---staalformulier.tsv")
  print(target_file_samp)
  readr::write_tsv(data_samp, path = file.path(target_path, target_file_samp), col_names = FALSE, na = '')

  target_file_ana <- paste0(project_naam, "_", filedate, "---analyseformulier.tsv")
  print(target_file_ana)
  readr::write_tsv(data_ana, path = file.path(target_path, target_file_ana), col_names = FALSE, na = '')

  #verplaats de originele file
  move_file <- file.path(finish_path, source_file)
  print(move_file)
  try(file.remove(move_file), silent = TRUE)
  file.rename(from = file.path(source_path, source_file), to = move_file)
  str(data_ana)
  str(data_samp)

}
