
#' Kopieer R scripts naar LIMS directory
#'
#'De bestanden worden vanuit het inbolimsintern pakket gekopieerd naar het gekozen pad. De bestaande bestanden in dit pad, worden verplaatst naar een backup folder (met datumaanduiding) in het pad
#'
#' @param path de folder waar de R scripts terecht moeten komen
#' @importFrom tools md5sum
#' @return vector met status per bestand of het succesvol gekopieerd is. Zou allemaal TRUE moeten zijn
#' @export
copy_scripts_to_lims_path <- function(path){

  #check dat het path niet met / of \ eindigt
  if (substring(path, nchar(path),  nchar(path)) %in% c("/", "\\")) {
    path <- substring(path, 1, nchar(path) - 1)
  }

  #md5 sums van R bestanden
  existing <- file.path(path, list.files(path, pattern = "*.R"))
  packagefiles <- paste(system.file("lims_scripts", package = "inbolimsintern"),
                         list.files(system.file("lims_scripts", package = "inbolimsintern"), pattern = "*.R"),
                        sep = "/")
  mdex <- tools::md5sum(existing)
  mdpkg <- tools::md5sum(packagefiles)

  if (length(mdex) != length(mdpkg) && !all(mdex == mdpkg)) {
    #maak backup directory
    now <- gsub(pattern = c(":"),
                x = as.character(Sys.time()),
                replacement = "")
    backuppath <- paste0(path, "/_backup", now)
    if (!dir.exists(backuppath)) dir.create(backuppath)

    #kopieer de files naar een backup folder
    for (i in seq_len(length(existing))) {
      check_success <- file.copy(from = existing[i],
                                 to = backuppath)
      if (check_success == TRUE) {
        file.remove(existing[i])
      }
    }
  } else {
    print("all scripts already up to date")
  }

  successvec <- NULL
  for (i in seq_len(length(packagefiles))) {
    successvec[i] <- file.copy(from = packagefiles[i],
                               to = path)
  }
  successvec

}
