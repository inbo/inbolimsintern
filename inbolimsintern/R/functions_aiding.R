#' Get current datetime as character variable
#'
#' @return character string yyyymmdd_hhmmss
#' @export
datetime_text <- function(){
  tm <- Sys.time()
  gsub(' ', '_', gsub(':', '', gsub('-', '', tm)))
}

#' Verkrijg de bestandsextensie
#'
#' @param file de naam van de file waarvoor de extensie gezocht wordt. De extensie is alles wat na het laatste '.' komt
#' @return character string met de extensie
get_file_extension <- function(file) {
  occ <- max(gregexpr(pattern = "\\.", file)[[1]])
  if (occ > 0) {
    return(substring(file, occ+1))
  } else {
    return('')
  }
}
