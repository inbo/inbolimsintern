
#' Make name of log file based on current datetime, directory, prefix and suffix
#'
#' @param path directory to write log file
#' @param prefix prefix to the file to make it recognisable
#' @param suffix suffix tot the file to make it recognisable
#'
#' @return file connection
#' @export
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

##############################################################################

#' Write to logfile (sink)
#'
#' @param logfile file to sink the output to
#' @param new set to TRUE if a new file is to be created
#' @param close close the output diversion?
#'
#' @return NULL, output redirected
logfile_write <- function(logfile, new = FALSE, close = FALSE) {
  try(sink())
  if (!is.null(logfile)) {
    sink(file = logfile, append = !new)
  }
  if (close) {
    sink()
  }
}
