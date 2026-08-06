

#' Batch file grabber
#'
#' @param env  PRD, UAT of DEV
#' @param scheduler_path path waar scheduler de files zoekt
#'
#' @returns aantal geconverteerde files
#' @export
#'
plumber_batch_file_grabber <- function(env = "PRD",
                                       call_id = 0,
                                       user_name = "",
                                       scheduler_path = "\\\\Inbo-limsbg-prd-labware8.inbo.be\\LABO_FS_PRD\\ANA\\_SCHEDULER") {

  logfile = "D:\\LOGS\\FILE_GRABBER.txt"
  cat("START\n", append = FALSE, file = logfile)
  library(tidyverse)
  library(inbolimsintern)
  library(readxl)
  lgf <- function(...) {cat(paste(..., "\n"), append = TRUE, file = logfile)}
  
  rv <- list(env = env, scheduler_path = scheduler_path)
  lgf(paste(rv, collapse = "\n"))

  
  if (exists("global_conn")) {
    conn <- global_conn #needed for write_db_log'    
  } else {
    global_conn <<- inbolimsintern::limsdb_connect(env = env)    
    conn <- global_conn
  }
  lgf(paste("global conn checked, no entering first db entry", "\n"))
  write_db_log("Entered QC_CHART_SIMPLE", "P")
  lgf("eerste log geschreven\n")
  
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = env)
  } 
  conn <- global_conn #global_conn is establlished with launcher script or the line above
  
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = env)
    conn <- global_conn
  }
  
  if (scheduler_path == "") {
    stop("Missing required scheduler path")
  }
  lgf("just before pasting paths together\n")
  e <- try({
    scheduler_base_dir <- scheduler_path #_scheduler/ANA
    grabloc_batchimport <- paste0(scheduler_base_dir, "\\BATCH_IMPORT")
    #grabloc_projaanvraag <- paste0(scheduler_base_dir, "\\PROJECTAANVRAAG")
    #grabloc_sampreg <- paste0(scheduler_base_dir, "\\STAALONTVANGST")
    #grabloc_veldformulier <- paste0(scheduler_base_dir, "\\VELDFORMULIER")
  })
  
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
  nrows <- nrow(files)
  lgf("start loop\n")
  if (nrows > 0) {
    for (i in 1:nrow(files)) {
      current_file <- batch_name <- batch_info <- data <- NULL
      message(paste0("processing file; ", files[i,1]))
      current_file <-  files[i, 1] %>% pull(path)
      lgf(paste("processing", current_file, "\n"))
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
        lgf(paste("geen geldige batch records voor ", batch_name), "\n")
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
    lgf("files moved\n")
  }
  lgf("just before last log\n")
  write_db_log("R script afgerond", "C")
  lgf("last log written\n")
  print('routine ended')
  lgf("just before return\n")
  return(paste("file grabber finished:", nrows, "files"))
  }

testit <- FALSE
if (testit){
  
}
