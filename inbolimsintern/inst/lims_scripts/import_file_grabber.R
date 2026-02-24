#################################
### Import file grabber
#################################


#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
library(readxl)
args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#library(tidyverse);library(readxl);args <- c("LWLPRD", "11390", "TEST_INT"); setup <- try(r_session_setup(args))
invisible(list2env(setup, envir = .GlobalEnv))

if (inherits(setup, "try-error")) {
  write_db_log(paste("Problem setting up R script", setup), "E")
  stop("probleem bij setup script:", e)
} else {
  write_db_log("R session setup finished", "P")
}

### >>> BASE DIRECTORIES
e <- try({
  scheduler_base_dir <- filter(params, ARG_NAME == "SCHEDULER_BASE_DIR") %>% pull(VALUE)
  grabloc_batchimport <- paste0(scheduler_base_dir, "\\BATCH_IMPORT")
  grabloc_projaanvraag <- paste0(scheduler_base_dir, "\\PROJECTAANVRAAG")
  grabloc_sampreg <- paste0(scheduler_base_dir, "\\STAALONTVANGST")
  grabloc_veldformulier <- paste0(scheduler_base_dir, "\\VELDFORMULIER")
})
if (inherits(e, "try-error")) {
  write_db_log(paste("Probleem bij inlezen parameters", e ), "E")
  stop(e)
} else {
  write_db_log("parameters gelezen", "P")
}


### >>> BATCH

current_file <- NULL
files <- tibble(path = list.files(path = grabloc_batchimport, full.names = FALSE)) %>%
  filter(path != "_FINISHED") %>%
  mutate(sha1 = digest::sha1(path))
files_ic_an <- filter(files, substring(path, 1, 5) == "IC_AN")
if (nrow(files_ic_an)) {
  for (i in 1:nrow(files_ic_an)) {
    katfile = gsub("IC_AN", "IC_KAT", files_ic_an[i, "path"])
    e <- try(file.copy(file.path(grabloc_batchimport, files_ic_an[i]),
              file.path(grabloc_batchimport, katfile)))
    if (inherits(e, "try-error")) write_db_log(e, "E")
    files <- bind_rows(files, data.frame(path = katfile))
  }
}

if (nrow(files) > 0) {
  for (i in 1:nrow(files)) {
    current_file <- batch_name <- batch_info <- data <- NULL
    message(paste0("processing file; ", files[i,1]))
    current_file <-  files[i, 1] %>% pull(path)
    if (class(current_file)[1] == 'try-error' | !length(current_file) | is.na(current_file)) {
      write_db_log(current_file, "E")
      next
    }
    e <- try(batch_name <- get_batchname_from_file(current_file))
    if (inherits(e, "try-error")) write_db_log(e, "E")
    e <- try(batch_info <- get_batch_info(conn, batch_name))
    if (inherits(e, "try-error")) write_db_log(e, "E")
    if (!nrow(batch_info)) {
      write_db_log(paste("geen geldige batch records voor ", batch_name), "E")
      next
    }
    e <- try(data <- get_data_from_importfile(file.path(grabloc_batchimport, current_file), batch_info, interpret_types = TRUE))
    if (inherits(e, "try-error")) write_db_log(e, "E")
    e <- try(move_batch_importfile(data = data,
                                   batch_info = batch_info,
                                   source_file = current_file,
                                   source_path = grabloc_batchimport,
                                   scheduler_base_dir = scheduler_base_dir))
    if (inherits(e, "try-error")) write_db_log(e, "E")
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

if (nrow(files) > 0) {
  for (i in 1:nrow(files)) {
    e <- try(current_file <-  files[i, 1] %>% pull(path))
    if (inherits(e, "try-error")) write_db_log(e, "E")
    e <- try(process_proj_reg(source_path = grabloc_projaanvraag,
                         source_file = current_file,
                         target_path = grabloc_projaanvraag,
                         finish_path = file.path(grabloc_projaanvraag, "_FINISHED"),
                         sheet = "Projectformulier"))
    if (inherits(e, "try-error")) write_db_log(e, "E")
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

if (nrow(files) > 0) {
  for (i in 1:nrow(files)) {
    e <- try(current_file <-  files[i, 1] %>% pull(path))
    if (inherits(e, "try-error")) write_db_log(e, "E")
    e <- try(process_samp_reg(source_path = grabloc_sampreg,
                         source_file = current_file,
                         target_path = grabloc_sampreg,
                         finish_path = file.path(grabloc_sampreg, "_FINISHED"),
                         sheet_samp = "STAALFORMULIER",
                         sheet_ana = "ANALYSEFORMULIER"))
    if (inherits(e, "try-error")) write_db_log(e, "E")
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

if (nrow(files) > 0) {
  for (i in 1:length(files)) {
    e <- try(current_file <-  files[i, 1] %>% pull(path))
    if (inherits(e, "try-error")) write_db_log(e, "E")
    e <- try(process_field_form(source_path = grabloc_veldformulier,
                           source_file = current_file,
                           target_path = grabloc_veldformulier,
                           finish_path = file.path(grabloc_veldformulier, "_FINISHED"),
                           sheet = "Veldformulier"))
    if (inherits(e, "try-error")) write_db_log(e, "E")
  }
}
write_db_log("R script afgerond", "C")

