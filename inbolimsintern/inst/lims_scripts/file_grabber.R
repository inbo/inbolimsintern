library(tidyverse)
library(readxl)
library(inbolimsintern)

### >>> INIT

logfile <- logfile_start(prefix = "IMPORTFILE_GRABBER")
writeLines(con = logfile, paste0("inbolimsintern versie: ", packageVersion("inbolimsintern")))
call_id <- 0
#call_id <- 747 #ter test

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

### >>> BASE DIRECTORIES

scheduler_base_dir <- filter(params, ARG_NAME == "SCHEDULER_BASE_DIR") %>% pull(VALUE)
grabloc_batchimport <- paste0(scheduler_base_dir, "\\BATCH_IMPORT")
grabloc_projaanvraag <- paste0(scheduler_base_dir, "\\PROJECTAANVRAAG")
grabloc_sampreg <- paste0(scheduler_base_dir, "\\STAALONTVANGST")
grabloc_veldformulier <- paste0(scheduler_base_dir, "\\VELDFORMULIER")

### >>> BATCH

current_file <- NULL
files <- tibble(path = list.files(path = grabloc_batchimport, full.names = FALSE)) %>%
  filter(path != "_FINISHED") %>%
  mutate(sha1 = digest::sha1(path))
files_ic_an <- filter(files, substring(path, 1, 5) == "IC_AN")
if (nrow(files_ic_an)) {
  for (i in 1:nrow(files_ic_an)) {
    katfile = gsub("IC_AN", "IC_KAT", files_ic_an[i, "path"])
    try(file.copy(file.path(grabloc_batchimport, files_ic_an[i]),
              file.path(grabloc_batchimport, katfile)))
    files <- bind_rows(files, data.frame(path = katfile))
  }
}
writeLines(con = logfile, paste0("BATCH: ", as.character(files[,1])))

if (nrow(files) > 0) {
  for (i in 1:nrow(files)) {
    current_file <- batch_name <- batch_info <- data <- NULL
    print(paste0("processing file; ", files[i,1]))
    current_file <-  files[i, 1] %>% pull(path)
    if (class(current_file)[1] == 'try-error' | !length(current_file) | is.na(current_file)) {
      next
    }
    try(batch_name <- get_batchname_from_file(current_file), outFile = logfile)
    try(batch_info <- get_batch_info(conn, batch_name), outFile = logfile)
    try(data <- get_data_from_importfile(file.path(grabloc_batchimport, current_file), batch_info, interpret_types = TRUE), outFile = logfile)
    try(move_batch_importfile(data, batch_info, source_file = current_file, source_path = grabloc_batchimport, scheduler_base_dir = scheduler_base_dir), outFile = logfile)
  }
}

### >>> PROJECTAANVRAAG

current_file <- NULL
files <- tibble(path = list.files(path = grabloc_projaanvraag, full.names = FALSE)) %>%
  filter(path != "_FINISHED",
         substring(path, 1, 1) != "~",
         substring(path, nchar(path) - 4) == '.xlsx'
  ) %>%
  mutate(sha1 = digest::sha1(path))
writeLines(con = logfile, paste0("PRJAANVRAAG: ", as.character(files[,1])))

if (nrow(files) > 0) {
  for (i in 1:nrow(files)) {
    try(current_file <-  files[i, 1] %>% pull(path), outFile = logfile)
    try(process_proj_reg(source_path = grabloc_projaanvraag,
                         source_file = current_file,
                         target_path = grabloc_projaanvraag,
                         finish_path = file.path(grabloc_projaanvraag, "_FINISHED"),
                         sheet = "Projectformulier"), outFile = logfile)
    print(str(data))
  }
}


### >>> STAALONTVANGST

current_file <- NULL
files <- tibble(path = list.files(path = grabloc_sampreg, full.names = FALSE)) %>%
  filter(path != "_FINISHED",
         substring(path, 1, 1) != "~",
         substring(path, nchar(path) - 4) == '.xlsx'
         ) %>%
  mutate(sha1 = digest::sha1(path))
writeLines(con = logfile, paste0("ONTVANGST: ", as.character(files[,1])))

if (nrow(files) > 0) {
  for (i in 1:nrow(files)) {
    try(current_file <-  files[i, 1] %>% pull(path), outFile = logfile)
    try(process_samp_reg(source_path = grabloc_sampreg,
                         source_file = current_file,
                         target_path = grabloc_sampreg,
                         finish_path = file.path(grabloc_sampreg, "_FINISHED"),
                         sheet_samp = "STAALFORMULIER",
                         sheet_ana = "ANALYSEFORMULIER"))
  }
}


#### >>> VELDFORMULIER

current_file <- NULL
files <- tibble(path = list.files(path = grabloc_veldformulier, full.names = FALSE)) %>%
  filter(path != "_FINISHED",
         substring(path, 1, 1) != "~",
         substring(path, nchar(path) - 4) == '.xlsx'
  ) %>%
  mutate(sha1 = digest::sha1(path))
writeLines(con = logfile, paste0("VELD: ", as.character(files[,1])))

if (nrow(files) > 0) {
  for (i in 1:length(files)) {
    try(current_file <-  files[i, 1] %>% pull(path), outFile = logfile)
    try(process_field_form(source_path = grabloc_veldformulier,
                           source_file = current_file,
                           target_path = grabloc_veldformulier,
                           finish_path = file.path(grabloc_veldformulier, "_FINISHED"),
                           sheet = "Veldformulier"), outFile = logfile)
  }
}


