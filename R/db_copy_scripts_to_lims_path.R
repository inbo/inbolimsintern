
#' Kopieer R scripts naar LIMS directory
#'
#'De bestanden worden vanuit het inbolimsintern pakket gekopieerd naar het gekozen pad. De bestaande bestanden in dit pad, worden verplaatst naar een backup folder (met datumaanduiding) in het pad
#'
#' @param path de folder waar de R scripts terecht moeten komen
#'
#' @return vector met status per bestand of het succesvol gekopieerd is. Zou allemaal TRUE moeten zijn
#' @export
copy_scripts_to_lims_path <- function(path){
  if (substring(path, nchar(path),  nchar(path)) == "/") {
    path <- substring(path, 1, nchar(path) - 1)
  }
  existing <- list.files(path, pattern = "*.R")
  now <- gsub(pattern = c(":"),
              x = as.character(Sys.time()),
              replacement = "")
  backuppath <- paste0(path, "/_backup", now)
  if (!dir.exists(backuppath)) dir.create(backuppath)

  for (i in seq_len(length(existing))) {
    check_success <- file.copy(from = paste0(path, "/", existing[i]),
                               to = backuppath)
    if (check_success == TRUE) {
      file.remove(paste0(path, "/", existing[i]))
    }
  }

  packagepath <- system.file("lims_scripts", package = "inbolimsintern")
  packagefiles <- list.files(packagepath, pattern = "*.R")

  successvec <- NULL
  for (i in seq_len(length(packagefiles))) {
    successvec[i] <- file.copy(from = paste0(packagepath, "/", packagefiles[i]),
                               to = path)
  }
  successvec

}
