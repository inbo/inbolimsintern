library(plumber)
library(inbolimsintern)

# Force load the logic into the Global Environment
source("D:\LWL8PRD\Data\R_SCRIPTS\plumber_logic_functions.R", local = FALSE)

#* @apiTitle Production API

### >>> Test functions
### ====================

#* @get /health
function() { list(status = "alive", time = Sys.time()) }

#* @get /multiply
function(a = 1, b = 1) {
  multiply_logic(a, b)
}


### >>> QC charts
### ====================

#* @get /generate-qc-report
#* @serializer text
function(env = "LWL8PRD", call_id = 0, user_name = "", 
         sqlfile = "", htmlfile = "", maxpoints = 30) {
  
  # Check connection (which was established in plumber_launcher.R)
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = "PRD")
  }
  
  # Call the named function from plumber_logic_functions.R
  generate_qc_report_logic(env, call_id, user_name, sqlfile, htmlfile, maxpoints)
}


### >>> File grabber
### ====================

#* @get /file-grabber
#* @serializer text
function( scheduler_path = "") {
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = "PRD")
  }
  # Call the named function from plumber_logic_functions.R
  batch_file_grabber_logic(env, scheduler_path)
}


### >>> Result stats
#=====================

#* @get /generate-results-stats
#* @serializer text
function(env = "LWL8PRD", call_id = 0, user_name = "", 
         workdir = "", timing = "") {
  
  # Check connection (which was established in plumber_launcher.R)
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = "PRD")
  }
  results_statistics(workdir, timing)
}
