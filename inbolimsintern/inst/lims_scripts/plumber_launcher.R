library(plumber)
library(inbolimsintern)
#library(future)

#plan(multisession, workers = parallel::detectCores() - 2)

readRenviron("D:/LWL8PRD/Data/WORKDIR/.Renviron")
global_conn <<- limsdb_connect(env = "PRD") # Note the <<- for global scope

pr <- plumb("D:/LWL8PRD/Data/R_SCRIPTS/plumber.R")
pr$run(host = "0.0.0.0", port = 8000) 
