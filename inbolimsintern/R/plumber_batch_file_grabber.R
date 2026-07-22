

batch_file_grabber <- function(env, scheduler_path) {
  library(tidyverse)
  library(inbolimsintern)
  library(readxl)
  
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = env)
  } 
  conn <- global_conn #global_conn is establlished with launcher script or the line above
  
  if (scheduler_path == "") {
    stop("Missing required scheduler path")
  }
  
  e <- try({
    scheduler_base_dir <- scheduler_path #_scheduler/ANA
    grabloc_batchimport <- paste0(scheduler_base_dir, "\\BATCH_IMPORT")
    grabloc_projaanvraag <- paste0(scheduler_base_dir, "\\PROJECTAANVRAAG")
    grabloc_sampreg <- paste0(scheduler_base_dir, "\\STAALONTVANGST")
    grabloc_veldformulier <- paste0(scheduler_base_dir, "\\VELDFORMULIER")
  })
}
