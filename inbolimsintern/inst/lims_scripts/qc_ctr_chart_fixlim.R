
library(inbolimsintern)
library(DBI)
library(tidyverse)


logfile <- logfile_start(prefix = "QC_SPECS")
writeLines(con = logfile, paste0("QC_CTR_FIXLIM\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

call_id <- 0 #call_id <- 883
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)
writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)


try({
  sqlfile <- filter(params, ARG_NAME == "SQLFILE") %>% pull(VALUE)
  htmlfile <- filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE)
  htmlrootshort <- substring(htmlfile, max(unlist(gregexpr("\\\\", htmlfile))) + 1, nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
  htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile)))) #including last backslash

}, outFile = logfile)

### data inlezen
try({
  label <- filter(params, ARG_NAME == "LABELS") %>% pull(VALUE)
  sqlcode <- readLines(sqlfile)
  sqlcode <- paste(sqlcode, collapse = "\n")
  cat("\n", sqlcode, file = logfile, append = TRUE)

  plotdata <- DBI::dbGetQuery(conn, sqlcode) %>%
    arrange(SAMPLE_NAME, ANALYSIS, NAME, ENTERED_ON, SAMPLE_NUMBER) %>%
    mutate(combi = paste(ANALYSIS, SAMPLE_NAME, NAME, sep = "---"),
           nominal_value = AVG_EXPECTED)
  if (is.null(plotdata$UNITS)) plotdata$UNITS <- ""
  cat('\n', plotdata[[2]], '\n', class(plotdata), file = logfile, append = TRUE)
}, outFile = logfile)

cat("\ndim plotdata\n", dim(plotdata), file = logfile, append = TRUE, sep ="\n")

try(write_csv(plotdata, path = "\\\\limsbgops.inbo.be\\LABO_FS\\LIMS\\qc_ctr_fixlim.csv"))

### figuren maken

htmlstart <- '<HTML><HEAD></HEAD><BODY>'
cat(htmlstart, file = htmlfile, append = FALSE)

cat('\n\nstart loopen over figuren\n', file = logfile, append = TRUE)
for (comb in unique(plotdata$combi)) {
  cat("\n", comb, file = logfile, append = TRUE)
  try(rm(subdata))
  figpathshort <- paste0(htmlrootshort, "_", make.names(comb), ".png")
  figpath <- paste0(htmlpath, "\\", figpathshort)
  subdata <- plotdata %>% filter(combi == comb)
  subdata$Nr <- 1:nrow(subdata)
  rules <-
    lims_shewhart_rules(x = subdata %>% pull(ENTRY) %>% as.numeric(),
                        center = mean(subdata %>% pull(AVG_MEASURED)),
                        sd = mean(subdata %>% pull(SD_MEASURED)))
  subdata <- bind_cols(subdata, as.data.frame(rules))
<<<<<<< HEAD
  htmldata <- subdata %>% transmute(Nr, TEXT_ID, BATCH, value = round(value, 5), UNITS,
=======
  htmldata <- subdata %>% transmute(Nr, TEXT_ID, BATCH, value = round(value, 5),
>>>>>>> 8ca6b631dc4df9a454d14417d831562678df01e0
                                    rules = paste0(rule01, rule02, rule03, rule04, rule05, rule06, rule07, rule08))

  print(comb)
  print(dim(subdata))

  p <- gg_lims_shewhart(subdata, add_nominal = FALSE) + ggtitle(comb)
  ggsave(plot = p, filename = figpath, height = 5, width = 7, dpi = 300)

  cat(paste0("\n<H2>", comb, "</H2>\n"), file = htmlfile, append = TRUE)
  cat(paste0("\n<IMG SRC = \"", figpathshort, "\">\n"), file = htmlfile, append = TRUE)

  htmltabel <- knitr::kable(htmldata, format = "html")
  cat(htmltabel, file = htmlfile, append = TRUE)
  cat(paste0("\neinde van ", comb), file = logfile, append = TRUE)
}
cat('\n</BODY></HTML>', file = htmlfile, append = TRUE)

shell.exec(htmlfile)

