library(tidyverse)
library(readxl)
library(inbolimsintern)
#library(plotly) pandoc nodig

logfile <- logfile_start(prefix = "RUN_CHART")
writeLines(con = logfile, paste0("inbolimsintern versie: ", packageVersion("inbolimsintern")))

call_id <- 0
#call_id <- 825 #ter test

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

project <- params %>% filter(ARG_NAME == "PROJECT") %>% pull(VALUE)
xaxis <- params %>% filter(ARG_NAME == "X") %>% pull(VALUE)
figpath <- params %>% filter(ARG_NAME == "PATH") %>% pull(VALUE)

try(qry <- run_chart_query(params), outFile = logfile)
try(plotdata <- get_run_chart_data(conn, qry), outFile = logfile)
try(htmlfile <- html_run_chart(plotdata, project = project, path = figpath,
                               split_fig = TRUE, show_pagesource = TRUE), outFile = logfile)
try(shell.exec(htmlfile), outFile = logfile)


#pl <- ggplotly(p)
#pl
#htmlwidgets::saveWidget(pl, file = plotpath, selfcontained = TRUE)

#calculate_textid zodat die altijd overeenkomt met de laagste sample_number in c_orig_dup_number


