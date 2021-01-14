library(tidyverse)
library(readxl)
library(inbolimsintern)

### >>> INIT

logfile <- logfile_start(prefix = "PROJECTAANVRAAG_FILE_GRABBER")
call_id <- 0 #

try({
  args <- inbolimsintern::prepare_session(call_id) #675
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

### >>> FIND FILES

grabloc <- filter(params, ARG_NAME == "LOCATION") %>% pull(VALUE)
files <- tibble(path = list.files(path = grabloc, full.names = FALSE)) %>%
  filter(path != "_FINISHED") %>%
  mutate(sha1 = digest::sha1(path))

for (i in files) {
  try(current_file <-  files[i, 1] %>% pull(path), outFile = logfile)
  try(data <- read_excel(current_file,
                         sheet = filter(params, ARG_NAME == "SHEET") %>% pull(VALUE),
                         guess_max = 5000), outFile = logfile)
  try(process_file_generic(data,
                           source_file = current_file,
                           target_location = ".",
                           move_location = "_FINISHED"), outFile = logfile)
}
