
library(tidyverse)
library(readxl)
library(inbolimsintern)

### >>> INIT

logfile <- logfile_start(prefix = "BATCH_IMPORTFILE_GRABBER")
writeLines(con = logfile, paste0("inbolimsintern versie: ", packageVersion("inbolimsintern")))
call_id <- 0 #689

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

### >>> FIND FILES

grabloc <- filter(params, ARG_NAME == "LOCATION") %>% pull(VALUE)
scheduler_base_dir <- filter(params, ARG_NAME == "SCHEDULER_BASE_DIR") %>% pull(VALUE)
files <- tibble(path = list.files(path = grabloc, full.names = FALSE)) %>%
  filter(path != "_FINISHED") %>%
  mutate(sha1 = digest::sha1(path))

### >>> PROCESS

for (i in 1:length(files)) {
  current_file <- batch_name <- batch_info <- data <- NULL
  print(paste0("processing file; ", files[i,1]))
  current_file <-  files[i, 1] %>% pull(path)
  if (class(current_file)[1] == 'try-error' | is.na(current_file)) {
    next
  }
  try(batch_name <- get_batchname_from_file(current_file), outFile = logfile)
  try(batch_info <- get_batch_info(conn, batch_name), outFile = logfile)
  try(data <- get_data_from_importfile(file.path(grabloc, current_file), batch_info), outFile = logfile)
  try(move_batch_importfile(data, batch_info, source_file = current_file, source_path = grabloc, scheduler_base_dir = scheduler_base_dir), outFile = logfile)
}

################


