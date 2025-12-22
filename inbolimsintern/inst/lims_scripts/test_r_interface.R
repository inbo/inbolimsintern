
Sys.sleep(3)
f = "test.log"
cat("TEST R CONNECT\n", as.character(Sys.time()), "\n------------------\n\n", file = f)
cat("entering R script\n", file = f, append = TRUE)

library(tidyverse)
library(inbolimsintern)
library(htmltools)
fig_height <- 600

cat("loading libraries completed\n", file = f, append = TRUE)


args <- commandArgs(trailingOnly = TRUE)

cat("argumenten:\n", file = f, append = TRUE)
cat(paste(args, collapse = "\n"), file = f, append = TRUE)
cat("\n", file = f, append = TRUE)
call_id <- args[2]

cat("trying setup session:", file = f, append = TRUE)
setup <- try(r_session_setup(args,test_mode = FALSE))
try(invisible(list2env(setup, envir = .GlobalEnv)))
try(write_db_log("R session setup", "P"))

if (inherits(setup, "try-error")) {
  cat(".....ERROR\n", file = f, append = TRUE)
  stop("probleem bij setup script", file = f, append = TRUE)
} else {
  cat("....OK\n", file = f, append = TRUE)
}
invisible(list2env(setup, envir = .GlobalEnv))
cat("FINISHED\n", file = f, append = TRUE)
try(write_db_log("Test R completed", "C"))
Sys.sleep(10)
