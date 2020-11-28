library(tidyverse)
library(readxl)

logfile <- logfile_start(prefix = "BATCH_IMPORTFILE_GRABBER")
call_id <- 0 #

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

grabloc <- "\\\\limsbgops.inbo.be\\labo_fs\\_SCHEDULER\\PARSER"
files <- tibble(path = list.files(path = grabloc, full.names = FALSE)) %>%
  filter(path != "_FINISHED") %>%
  mutate(sha1 = digest::sha1(path))


for (i in files) {
  batchname = get_batchname_from_file(files[i])
  if (is.null(batchname)) {
    cat(paste0("batchnaam niet gevonden in filenaam ", files[i], "\n"), file = logfile, append = TRUE)
    next
  }
  batch_info <- get_batchinfo(conn, batchname)
  if (nrow(batch_info) == 0) {
    cat(paste0("batch ", batchname, " niet gevonden in de DB", "\n"), file = logfile, append = TRUE)
    next
  }
  if (is.null(batch_info[1,'c_import_routine'])) {
    cat(paste0("batch ", batchname, " heeft geen gedefinieerde importroutine in de batch template", "\n"), file = logfile, append = TRUE)
    next
  }

  data <- batch_importfile_parse(grabloc, files[i,1], batch_info)
  db_write_import_table(data)
  move_file(file.path(grabloc, files[i,1]), to = attr(data, 'target'))
}

print(parsed)





