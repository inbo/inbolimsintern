
#' Creert logbestand (opent connectie)
#'
#' @param path directory waar de logfile wordt aangemaakt . <auto> gebruikt de default
#' @param prefix vorvoegsel in de file, bijvoorbeeld welk script gebruikt werd
#' @param suffix suffix dat bijvoorbeeld een db id kan bevatten
#'
#' @return file connectie
#' @export
#'
logfile_start <- function(path = "<auto>", prefix = "", suffix = "") {
  defaultdir <- "\\\\LimsBGOPS.inbo.be\\Labware7\\LabWare-7\\Data\\logs"
  if (path == "<auto>") {
    check <- dir.exists(defaultdir)
    if (check) {
      path <- defaultdir
    } else {
      path <- getwd()
    }
  }
  time <- substring(Sys.time(),1,19)
  time <- gsub("-", "", time)
  time <- gsub(":", "", time)
  time <- gsub(" ", "_", time)
  filename <- paste0(path, "/", prefix, "_", time, "_", suffix, ".log")
  cat("log file:", filename, "\n")
  file(filename, open = "a")
}


