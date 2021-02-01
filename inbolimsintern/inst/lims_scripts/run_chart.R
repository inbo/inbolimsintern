library(tidyverse)
library(readxl)
library(inbolimsintern)
#library(plotly) pandoc nodig

logfile <- logfile_start(prefix = "RUN_CHART")
writeLines(con = logfile, paste0("inbolimsintern versie: ", packageVersion("inbolimsintern")))

call_id <- 0
#call_id <- 857

try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

charts <- params %>% filter(ARG_NAME == "CHART") %>% pull(VALUE)
if (!length(charts)) {
  charts = " "
}
project <- (params %>% filter(ARG_NAME == "PROJECT") %>% pull(VALUE))[1]
print(project)
datetxt <- datetime_text()
filebase <- paste0((params %>% filter(ARG_NAME == "PATH") %>%  pull(VALUE))[1],
                   "\\run_chart_", project, "_", datetxt)
htmlfile <- paste0(filebase, ".html")

html <- "<HTML><HEAD></HEAD><BODY>"
for (i in 1:length(charts)) {
  try(qry <- run_chart_query(params, index = i), outFile = logfile)
  try(plotdata <- get_run_chart_data(conn, qry), outFile = logfile)
  try(htmlstring <- html_run_chart(plotdata, project = project, path = filebase,
                                   split_fig = TRUE, chart_header = charts[i]), outFile = logfile)
  try(html <- paste0(html, htmlstring), outFile = logfile)
}
html <- paste0(html, "</BODY></HTML>")
writeLines(html, con = htmlfile)
try(shell.exec(htmlfile), outFile = logfile)


#pl <- ggplotly(p)
#pl
#htmlwidgets::saveWidget(pl, file = plotpath, selfcontained = TRUE)

#calculate_textid zodat die altijd overeenkomt met de laagste sample_number in c_orig_dup_number


