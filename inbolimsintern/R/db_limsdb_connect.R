
#' Title
#'
#' @param server name of the LIMS server
#' @param database name of the LIMS database
#' @param uid application username (that as writing rights on the db, so this is not you username)
#' @param pwd applciation username password
#' @param connectlist list containing dsn (server name), uid (user id) and pwd (password). Optionally it can also contain db. It overrides the values of the other arguments
#'
#' @return db connection
#' @export
#'
#' @examples
#' \dontrun{
#' lims_db_connect(uid = "me", pwd = "123456") #should not work
#' }
limsdb_connect <- function(server = "inbo-sql07-prd.inbo.be", 
                           database = "D0015_00_Lims", uid, pwd, connectlist = NULL){
  if  (!is.null(connectlist)) {
    if (is.list(connectlist) & all(c("uid", "pwd", "dsn") %in% names(connectlist))) {
      uid <- connectlist$uid
      pwd <- connectlist$pwd
      server <- connectlist$dsn
      if (!is.null(connectlist$db)) database <- connectlist$db
    }
  }
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver = "SQL Server", 
                        Server = server, 
                        Database = database, 
                        uid = unname(uid),
                        pwd = unname(pwd))   
  con
}

