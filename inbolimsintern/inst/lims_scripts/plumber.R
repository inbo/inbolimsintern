library(plumber)

# Force load the logic into the Global Environment
source("D:\LWL8PRD\Data\R_SCRIPTS\logic_functions.R", local = FALSE)

#* @apiTitle Production API

#* @get /health
function() { list(status = "alive", time = Sys.time()) }

#* @get /multiply
function(a = 1, b = 1) {
  multiply_logic(a, b)
}

#* @get /generate-qc-report
#* @serializer text
function(env = "LWL8PRD", call_id = 0, user_name = "", 
         sqlfile = "", htmlfile = "", maxpoints = 30) {
  
  # Check connection (which was established in launcher.R)
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = "PRD")
  }
  
  # Call the named function from logic_functions.R
  generate_qc_report_logic(env, call_id, user_name, sqlfile, htmlfile, maxpoints)
}