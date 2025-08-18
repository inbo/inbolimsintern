#' Lees de data credentials van de LIMS hoofddatabank
#'
#' Lees de credentials in voor de database vanuit een bestand. De eerste regel bevat de data source, de tweede regel de username, en de derde regel het passwoord
#' @param file bestandsnaam waaruit de credentials geschreven worden
#'
#' @return list with at least 4 elements, the host, the data source, user-id and paswoord of the LIMS Main DB
#' @export
#'
#' @examples
#' read_db_credentials(system.file("extdata", "dbcredentials.txt", package = "inbolimsintern"))
read_db_credentials <- function(file = "dbcredentials.txt")
{
  creds <- readLines(file)
  list(host = creds[1],
       dsn  = creds[2],
       uid  = creds[3],
       pwd  = creds[4])
}
