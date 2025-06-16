#' Connect to lims datawarehouse
#'
#' Connect to the lims datawarehouse using a trusted connection as INBO staff member
#'
#' @param deployment 'uat' for test server 'prd' for production server
#' @param use_rodbc use the rodbc library (or the DBI when FALSE)
#' @param uidpwd not used for the moment
#'
#' @return
#' @export
limsdwh_connect <- function(deployment = "prd", use_rodbc = FALSE, uidpwd = "") {
  if (deployment != "uat") {
    if (use_rodbc) {
      con <- try(RODBC::odbcDriverConnect(
        connection = "Driver={ODBC Driver 13 for SQL Server};Server=inbo-sql08-prd.inbo.be;Database=W0003_00_Lims;Trusted_Connection=yes;Port=1433;" # nolint
      ), silent = TRUE)
      if (inherits(con, "try-error") || inherits(con, "integer")) {
        cstr <- paste0(
          "Driver={ODBC Driver 13 for SQL Server};Server=inbo-sql08-prd.inbouat.be;Database=W0003_00_Lims;Port=1435;", # nolint
          uidpwd
        )
        con <- RODBC::odbcDriverConnect(
          connection = cstr
        )
      }
      if (!inherits(con, "RODBC")) print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator") # nolint
    } else {
      con <- DBI::dbConnect(odbc::odbc(),
                            Driver = "SQL Server",
                            Server = "inbo-sql08-prd.inbo.be",
                            port = 1433, # toegevoegd voor vpn, weghalen indien dit problemen geeft
                            Database = "W0003_00_Lims",
                            Trusted_Connection = "True"
      )
      if (!inherits(con, "Microsoft SQL Server")) {
        print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator") # nolint
      }
    }
  } else {
    # DBI lijkt niet graag te werken met referentiebeheer
    # Referentiebeheer zoeken in windows
    # toevoegen inbo-sql06-uat.inbouat.be  gebruiker INBOUAT\pieter_verschelde

    con <- try(
      RODBC::odbcDriverConnect(
        connection = "Driver={ODBC Driver 13 for SQL Server};Server=inbo-sql06-uat.inbouat.be,1435;Database=W0003_00_Lims;Trusted_Connection=yes;" # nolint
      ),
      silent = TRUE
    )

    if (inherits(con, "try-error") || inherits(con, "integer")) {
      cstr <- paste0(
        "Driver={ODBC Driver 13 for SQL Server};Server=inbo-sql06-uat.inbouat.be,1435;Database=W0003_00_Lims;", # nolint
        uidpwd
      )
      con <- RODBC::odbcDriverConnect(
        connection = cstr
      )
    }
    if (!inherits(con, "RODBC")) {
      print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator") # nolint
    } else {
      print("Connectie succesvol")
    }
  }
  con
}
