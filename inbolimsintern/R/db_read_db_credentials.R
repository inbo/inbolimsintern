#' Lees de data credentials van de LIMS hoofddatabank
#'
#' Lees de credentials in voor de database vanuit een bestand. De eerste regel bevat de data source, de tweede regel de username, en de derde regel het passwoord
#' @param file bestandsnaam waaruit de credentials geschreven worden
#'
#' @return list with 3 elements, the data source, user-id and paswoord of the LIMS Main DB
#' @export
#'
#' @examples
#' read_db_credentials(system.file("extdata", "dbcredentials.txt", package = "inbolimsintern"))
read_db_credentials <- function(file = "dbcredentials.txt")
{
  creds <- readLines(file)
  list(dsn = creds[1],
       uid = creds[2],
       pwd = creds[3])
}
