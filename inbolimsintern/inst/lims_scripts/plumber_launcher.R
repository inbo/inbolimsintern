
# plumber_launcher2.R

# Create a text file to capture the exact crash reason
log_file <- file("D:/LWL8PRD/Data/R_SCRIPTS/api_crash_log.txt", open = "wt")
sink(log_file, type = "output")
sink(log_file, type = "message")

cat("--- Starting API Session at", as.character(Sys.time()), "---\n")

tryCatch({
  library(plumber)
  library(inbolimsintern)
  
  readRenviron("D:/LWL8PRD/Data/WORKDIR/.Renviron")
  global_conn <<- limsdb_connect(env = "PRD") # Note the <<- for global scope
  
  
  # Point this exactly to your plumber script path
  pr <- pr("D:/LWL8PRD/Data/R_SCRIPTS/plumber.R") 
  
  # Run the API
  pr_run(pr, port = 8000, host = "0.0.0.0")
  
}, error = function(e) {
  cat("FATAL ERROR OCCURRED:\n")
  print(e)
})

# Close the logs if it stops
sink(type = "message")
sink(type = "output")