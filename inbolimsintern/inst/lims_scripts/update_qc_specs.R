library(googlesheets4)
library(DBI)

logfile <- logfile_start(prefix = "QC_SPECS")
writeLines(con = logfile, paste0("inbolimsintern versie: ", packageVersion("inbolimsintern")))

call_id <- 0 #mag 0 blijven bij gs, bij excel moet dit iets zijn die een argument PATH en SHEET heeft
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

#Dit zal wel uit excel ingelezen worden om authentificatieproblemen te voorkomen
read_from_gs <- TRUE
if (read_from_gs) {
  gs4_auth(email = "pieter.verschelde@inbo.be")
  data <- read_sheet("1wjG-tOCGUzfx4ky1cmPXHAfB3qqCVul3TE6FbeNlkPo",
                     sheet = "Sheet1")
} else {
  data <- read_excel(path, sheet = sheet)
  update_qc_limits(conn, data)
}

