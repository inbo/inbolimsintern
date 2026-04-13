#// SESSION SETUP
##=====================
library(inbolimsintern)
#log_message("Setting up R session...")
args <- commandArgs(trailingOnly = TRUE)
setup <- try(r_session_setup(args), silent = TRUE)
#library(inbolimsintern);args <- c("LWL8PRD", "11737", "PIETERVS");setup <- try(r_session_setup(args))

#log_message(paste("Arguments:", paste(args, collapse = ", ")))
if (inherits(setup, "try-error")) {
  log_message(paste("ERROR in session setup:", as.character(setup)))
  write_db_log("Session setup failed", "E")
  stop("Problem with session setup")
}

invisible(list2env(setup, envir = .GlobalEnv))
#log_message("Session setup complete")
write_db_log("R session initialized", "P")


#// RETRIEVE PARAMETERS
##=====================

qry = paste("select [PROJECT], [TEXT_ID], [SAMPLE_ID] , [USER],",
            "[CALL_ID], [PRINTER], [LABELFORMAAT], [SMP_ORDER], [REP]", 
            " from C_TMP_LABEL_PROCESSI ",
            " where CALL_ID = ", call_id, 
            " order by ID, SMP_ORDER, REP, TEXT_ID")
data <- DBI::dbGetQuery(conn, qry)
if (nrow(data) == 0){
  write_db_log("Geen labels gevonden", "E")
  stop("Geen labels gevonden")
} else {
  write_db_log(paste("Data read:", nrow(data), "rijen"), "P")
}


#// CONFIG
##============
printer <- data[1, "PRINTER"] 
layout <- data[1, "LABELFORMAAT"]

printer_config <- get_printer_config(conn, printer)
format_lines <- get_label_format(conn, layout)

#print_lims_labels(data, printer_config, format_lines, mode = "api", format = "pdf")
debugmode <- FALSE
batch_mode <- FALSE
batch_size <- 8

e <- try({
  print_lims_labels(
    data,
    printer_config,
    format_lines,
    mode = "real",
    format = "pdf",
    abort_real_show_payload = debugmode,
    batch_mode = batch_mode,
    batch_size = batch_size)
})
if (inherits(e, "try-error")) {
  write_db_log(paste("ERROR printing to printer:", e), "E")
  stop(e)
} else {
  write_db_log(paste("Sample labels sent to printer:"), "C") 
}
