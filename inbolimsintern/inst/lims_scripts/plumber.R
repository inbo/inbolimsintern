library(plumber)

# Force load the logic into the Global Environment
#sourcepath <- "D:\\LWL8PRD\\Data\\R_SCRIPTS"
#source("D:\\LWL8PRD\\Data\\R_SCRIPTS\\plumber_logic_functions.R", local = FALSE)

#* @apiTitle Production API

### >>> Test functions
### ====================

#* @get /check_health
#* @serializer text
function() {check_health()}

#* @get /check_multiply
function(a = 1, b = 1) {
  check_multiply(a = a, b = b)
}

#* @get /check_db
function(env = "PRD") {
  check_db(env = env)
}

### >>> QC charts functions
### ====================

#* @get /qc_chart
#* @serializer text
function(env = "PRD", call_id = 0, user_name = "", 
         sqlfile = "", htmlfile = "", maxpoints = 30) {
  
  # Call the named function from plumber_logic_functions.R
  plumber_qc_chart(env, call_id, user_name, sqlfile, htmlfile, maxpoints)
}

#* @get /qc_chart_simple
#* @serializer text
function(env = "PRD", call_id = 0, user_name = "",  
         sqlfile = "", htmlfile = "", maxpoints = 30) {
  
  rv <- plumber_qc_chart_simple(env, call_id, user_name, sqlfile, htmlfile, maxpoints)
  
  # Return a clear message back to the LIMS rest client
  return(paste(rv, "\n\nSUCCESS: HTML generated at", htmlfile))
}


### >>> File grabber
### ====================

#* @get /file_grabber
#* @serializer text
function(env = "PRD", call_id = 0, user_name = "", scheduler_path = NULL) {
  tryCatch({
    if (is.null(scheduler_path) || scheduler_path == "") {
      scheduler_path <- "\\\\Inbo-limsbg-prd-labware8.inbo.be\\LABO_FS_PRD\\ANA\\_SCHEDULER"
    }
    plumber_batch_file_grabber(env = env, call_id = call_id, user_name = user_name, scheduler_path = scheduler_path)
  }, error = function(e) {
    paste("Plumber Endpoint Error:", e$message)
  })
}


### >>> Result stats
#=====================

#* @get /generate_results_stats
#* @serializer text
function(env = "LWL8PRD", call_id = 0, user_name = "", 
         workdir = "", timing = "") {
  results_statistics(path = workdir, basename = timing, conn = global_conn)
}