library(plumber)

# Force load the logic into the Global Environment
source("D:\\LWL8PRD\\Data\\R_SCRIPTS\\plumber_logic_functions.R", local = FALSE)

#* @apiTitle Production API

### >>> Test functions
### ====================

#* @get /health
function() { list(status = "alive", time = Sys.time()) }

#* @get /multiply
function(a = 1, b = 1) {
  multiply_logic(a, b)
}

#* @get /testdb
function(a = 1, b = 1) {
  # 1. Run your logic
  multiply_logic(as.numeric(a), as.numeric(b))
  
  # 2. Check/Establish the connection securely
  if (!exists("global_conn") || is.null(global_conn) || !DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = "PRD")
  }
  
  # 3. FIX: Return a clean list, NOT the raw connection object
  list(
    message = "Database check completed successfully",
    connection_is_valid = DBI::dbIsValid(global_conn),
    environment = "PRD",
    timestamp = Sys.time()
  )
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
  
  # 1. Ensure connection is alive
  if (!exists("global_conn") || is.null(global_conn) || !DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = "PRD")
  }
  
  # 2. Safety Check: Ensure workdir was actually provided
  if (workdir == "") {
    return("Error: 'workdir' parameter cannot be empty.")
  }
  
  # 3. FIX: Pass the global connection directly into your logic function
  # (Modify your underlying results_statistics function to accept it if necessary)
  results_statistics(path = workdir, basename = timing, conn = global_conn)
}