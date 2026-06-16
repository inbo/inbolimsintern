#######################################
### RUN CHARTS
#######################################

#// setup environment
##=====================

library(tidyverse)
library(inbolimsintern)
library(readxl)
args <- commandArgs(trailingOnly = TRUE); setup <- try(r_session_setup(args))
#args <- c("LWL8PRD", "11782", "RUN_CHART"); setup <- try(r_session_setup(args))
invisible(list2env(setup, envir = .GlobalEnv))

if (inherits(setup, "try-error")) {
  write_db_log(paste("Problem setting up R script", setup), "E")
  stop("probleem bij setup script:", e)
} else {
  write_db_log("R session setup finished", "P")
}

e <- try({
  project <- (params %>% filter(ARG_NAME == "PROJECT") %>% pull(VALUE))[1]
  message(project)
  filepath <- paste0(params %>% filter(ARG_NAME == "PATH") %>%  pull(VALUE))[1]
  charts <- params %>% filter(ARG_NAME == "CHART") %>% pull(VALUE)
  htmlfile <- (params %>% filter(ARG_NAME == "HTMLPATH") %>% pull(.data$VALUE))[1]
  filebase <- gsub("\\.html", "", htmlfile)
  if (!length(charts)) {
    charts = " "
  }
  datetxt <- datetime_text()
})
if (inherits(e, "try-error")){
  write_db_log(e, "E")
} else {
  write_db_log("Parameters ingelezen", "P")
}

#write html contents

html <- "<HTML><HEAD></HEAD><BODY>"
for (i in 1:length(charts)) {
  e <- try(qry <- run_chart_query(params, index = i))
  if (inherits(e, "try-error")) write_db_log(e, "E")
  e <- try(plotdata <- get_run_chart_data(conn, qry))
  if (inherits(e, "try-error")) write_db_log(e, "E")
  e <-try(htmlstring <- html_run_chart_embedded(plotdata, project = project, path = filebase,
                                   split_fig = TRUE, chart_header = charts[i]))
  if (inherits(e, "try-error")) write_db_log(e, "E")
  e <- try(html <- paste0(html, htmlstring))
  if (inherits(e, "try-error")) write_db_log(e, "E")
}
html <- paste0(html, "</BODY></HTML>")

e <- try(writeLines(html, con = htmlfile))
if (inherits(e, "try-error")) {
  write_db_log(e, "E")
  stop(e)
}
write_db_log("Script afgerond", "C")


try(shell.exec(htmlfile))


#pl <- ggplotly(p)
#pl
#htmlwidgets::saveWidget(pl, file = plotpath, selfcontained = TRUE)

#calculate_textid zodat die altijd overeenkomt met de laagste sample_number in c_orig_dup_number


