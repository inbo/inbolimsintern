#Maak een runchart van alle meetpunten de laatste 4 jaar, gegroepeerd per batch en gescheiden per jaar (OK)
#Maak een controlekaart van alle waarnemingen het laatste jaar volgens de standaard controlekaart regels (OK)
#Maak een controlekaart van de laatste 30 waarnemingen ongeacht het jaar (OK)
#zorg voor een csv download link (een over alles heen) (OK)

#Voer een F-test en t-test uit voor het laatste jaar (OK)
#Voer een F-test en t-test uit voor de laatste 30 punten
#Voer een F-test en t-test uit voor het laatste jaar tov 1, 1+2, 1+2+3 jaar geleden (OK)
#Probeer deze te plotten



library(inbolimsintern)
library(DBI)
library(tidyverse)


logfile <- logfile_start(prefix = "QC_SPECS")
writeLines(con = logfile, paste0("QC_CTR_FIXLIM\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

call_id <- 0 #call_id <- 2280 #1795 1965
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
    sqlcode <- readLines(sqlfile)
    sqlcode <- paste(sqlcode, collapse = "\n")
    cat("\n", sqlcode, file = logfile, append = TRUE)
    alldataorig <- DBI::dbGetQuery(conn, sqlcode)
}, outFile = logfile)

#alle limieten corresponderend met de puntdata
qry = paste0("select ANALYSIS, NAME = COMPONENT, PRODUCT_GRADE = GRADE, VERSION, C_CTR_X, C_CTR_SD, C_CERTIFIED_VALUE, C_CERTIFIED_SD
from PRODUCT_SPEC ps where ANALYSIS = '", unique(alldataorig$ANALYSIS), "'")
dfLimits <- dbGetQuery(conn, qry) %>%
  filter(VERSION == max(.$VERSION)) %>%
  filter(!duplicated(.))
jaren <- unique(lubridate::year(alldataorig$ENTERED_ON))
lastyear <- max(jaren)

#alle puntdata
alldata <- alldataorig %>%
  left_join(dfLimits) %>%
  arrange(PRODUCT_GRADE, ANALYSIS, NAME, BATCH, ORDER_NUMBER) %>%
  mutate(rownr = 1:nrow(.),
         WAARDE = as.numeric(ENTRY),
         rownr = 1:length(WAARDE),
         combi = make.names(paste(ANALYSIS, NAME, PRODUCT_GRADE, sep = "_")))

first_batch_qc <- alldata %>%
  group_by(BATCH, PRODUCT_GRADE, ANALYSIS, NAME) %>%
  summarize(rownr = min(rownr)) %>%
  mutate(in_stat = TRUE)

alldata <- alldata %>%
  left_join(first_batch_qc) %>%
  mutate(in_stat = ifelse(is.na(in_stat), FALSE, in_stat))

### Maak HTML

cat(paste0("<html>\n<head>", "call_id : ", args["call_id"], "</head>\n<body>\n"),
    file = htmlfile, append = FALSE)
csvfile <- paste0(htmlpath, htmlrootshort, ".csv")
cat("<a href=\"",csvfile, "\">download csv data:</a>", file = htmlfile, append = FALSE)
write_excel_csv2(alldata, path = csvfile)

for (cmb in unique(alldata$combi)) {
  print(cmb)
  figbaseabs <- paste0(htmlpath, htmlrootshort, "_", cmb, "_")
  figbaserel <- paste0(htmlrootshort, "_", cmb, "_")
  cat(paste0("<h2>", cmb, "</h2>"), file = htmlfile, append = TRUE)
  dfcomb <- alldata %>%
    arrange(in_stat, rownr) %>%
    filter(combi == cmb)

  #overzicht van alle punten
  p1 <-
    ggplot(dfcomb, aes(x = ENTERED_ON, y = WAARDE)) +
    geom_point(aes(color = in_stat)) +
    geom_smooth() +
    geom_vline(xintercept = as.POSIXct(paste0(unique(paste0(lubridate::year(dfcomb$ENTERED_ON))), "-01-01"))) +
    labs(y = paste("meting (", dfcomb$UNITS[1], ")"),
         x = "Invoerdatum",
         color = "berekeningen",
         subtitle = paste("Overzicht alle metingen van",
                          lubridate::date( min(dfcomb$ENTERED_ON)),
                          "tot",
                          lubridate::date( max(dfcomb$ENTERED_ON))),
         title = cmb)

  ggsave(p1, filename = paste0(figbaseabs, "_001.png"))
  cat(paste0("\n<p>\n<IMG SRC = \"", paste0(figbaserel, "_001.png"), "\">\n"),
      file = htmlfile, append = TRUE)

  #controlekaart laatste jaar
  batchvolgorde <- dfcomb %>%
    group_by(BATCH) %>%
    summarize(minEntered = min(ENTERED_ON)) %>%
    arrange(minEntered) %>%
    mutate(batchnr = 1:nrow(.))

  dfcomb2 <- dfcomb %>%
    left_join(batchvolgorde, by = "BATCH") %>%
    arrange(batchnr)

  htmldata <- try(elc_htmldata(dfcomb2 %>% filter(lubridate::year(minEntered) == lastyear)))
  if (class(htmldata) == "try-error") {
    cat("\nTE WEINIG DATA\n", file = htmlfile, append = TRUE)
    next
  }
  p2 <- ELC_shewhart_plot(subdata = htmldata[["plot"]], htmldata[['borders']] ) +
    labs(title = cmb,
         x = paste0("Batches in laatste kalenderjaar (", lastyear, ")"))

  #controlekaart laatste 30 meegetelde punten
  dfcomb3 <- dfcomb2 %>% filter(batchnr > max(batchnr - 30))
  htmldata3 <- elc_htmldata(dfcomb3)
  p3 <- ELC_shewhart_plot(subdata = htmldata3[["plot"]], htmldata3[['borders']] ) +
    labs(title = cmb,
         x = "Laatste 30 batches")

  #t-test en F-test binnen het laatste jaar
  dfly <- dfcomb2 %>%
    filter(year == lastyear, in_stat == TRUE)
  aantal_per_groep <- round(nrow(dfly)/2)
  dfly$grp <- c(rep(1, aantal_per_groep), rep(2, nrow(dfly) - aantal_per_groep))

  #t-test en F-testlaatste 30 punten
  dfl30 <- dfcomb3 %>%
    filter(in_stat == TRUE)
  aantal_per_groep <- round(nrow(dfl30)/2)
  dfl30$grp <- c(rep(1, aantal_per_groep), rep(2, nrow(dfl30) - aantal_per_groep))

  # ggsave(p1, filename = paste0(figbaseabs, "_001.png"))
  # cat(paste0("<IMG SRC =\"", paste0(figbaserel, "_001.png"), "\">"),
  #     file = htmlfile, append = TRUE)

  #binnen het laatste jaar

  htest_ly  <- elc_htest(dfly, label = paste0("binnen ", lastyear))
  htest_l30 <- elc_htest(dfl30, label = paste0("laatste 30 batches"))
  htest_py  <- elc_htest(dfcomb %>%
                          mutate(grp = ifelse(year == lastyear,
                                              1,
                                              ifelse(year == lastyear-1,
                                                     2, NA))) %>%
                          filter(!is.na(grp)),
                        label = paste0(lastyear, " vs vorig jaar"))
  htest_p2y <- elc_htest(dfcomb %>%
                          mutate(grp = ifelse(year == lastyear,
                                              1,
                                              ifelse(year %in% (lastyear - (1:2)),
                                                     2, NA))) %>%
                          filter(!is.na(grp)),
                        label = paste0(lastyear, " vs vorige 2 jaar"))

  htest_p3y <- elc_htest(dfcomb %>%
                           mutate(grp = ifelse(year == lastyear,
                                               1,
                                               ifelse(year %in% (lastyear - (1:3)),
                                                      2, NA))) %>%
                           filter(!is.na(grp)),
                         label = paste0(lastyear, " vs vorige 3 jaar"))

  testresults <- bind_rows(htest_ly, htest_l30, htest_py, htest_p2y, htest_p3y)

  p4 <- ggplot(testresults %>%
                 mutate(label = factor(label,
                                       levels = rev(unique(testresults$label)))) %>%
                 filter(test %in% c("t.test", "var.test")),
               aes(x = label)) +
    geom_errorbar(aes(ymin = lcl, ymax = ucl))  +
    facet_wrap(~test, scales = "free_x") +
    geom_hline(data = data.frame(test = c("t.test", "var.test"),
                                 y = c(0,1)),
               aes(yintercept = y), color = "green", linetype = 2) +
    labs(x = "Vergelijking",
         y = "Verschil (indien lijn niet in interval: significant)",
         title = cmb) +
    coord_flip()

  ### grenzen

  aanwezige_grenzen <-
    dfcomb %>%
    mutate(type = "aanwezig") %>%
    group_by(type, C_CTR_X, C_CTR_SD) %>%
    summarise(aantal = n(),
              minDate = min(ENTERED_ON),
              maxDate = max(ENTERED_ON)) %>%
    arrange(minDate)

  berekende_grenzen <- dfly %>%
    summarise(type = "berekend",
              Avg = mean(WAARDE, na.rm = TRUE), #toekomst: werken met iteratief proces
              Sd = sd(WAARDE, na.rm = TRUE),
              aantal = n(),
              minDate = min(ENTERED_ON),
              maxDate = max(ENTERED_ON))
  cat("<p>\n", file = htmlfile, append = TRUE)

  ggsave(p2, filename = paste0(figbaseabs, "_002.png"))
  cat(paste0("\n<p>\n<IMG SRC = \"", paste0(figbaserel, "_002.png"), "\">\n"),
      file = htmlfile, append = TRUE)

  ggsave(p3, filename = paste0(figbaseabs, "_003.png"))
  cat(paste0("\n<p>\n<IMG SRC = \"", paste0(figbaserel, "_003.png"), "\">\n"),
      file = htmlfile, append = TRUE)

  ggsave(p4, filename = paste0(figbaseabs, "_005.png"))
  cat(paste0("\n<p>\n<IMG SRC = \"", paste0(figbaserel, "_005.png"), "\">\n"),
      file = htmlfile, append = TRUE)

  cat(knitr::kable(testresults %>%
                     filter(test == "stats") %>%
                     select(label, test, n1, n2, avg1, avg2, sdv1, sdv2),
                   format = "html", row.names = FALSE),
      file = htmlfile, append = TRUE)
  cat(knitr::kable(testresults %>%
                     filter(test != "stats") %>%
                     select(label, test, stat, pval, lcl, ucl),
                   format = "html", row.names = FALSE),
      file = htmlfile, append = TRUE)

  berekende_grenzen <- berekende_grenzen %>%
    select(type, C_CTR_X = Avg, C_CTR_SD = Sd, aantal, minDate, maxDate)
  cat(knitr::kable(bind_rows(aanwezige_grenzen, berekende_grenzen), format = "html"),
      file = htmlfile, append = TRUE)

}

cat(paste0("</body></html>"), file = htmlfile, append = TRUE)

shell.exec(htmlfile)


# ### data inlezen
# try({
#   label <- filter(params, ARG_NAME == "LABELS") %>% pull(VALUE)
#   sqlcode <- readLines(sqlfile)
#   sqlcode <- paste(sqlcode, collapse = "\n")
#   cat("\n", sqlcode, file = logfile, append = TRUE)
#
#   plotdata <- DBI::dbGetQuery(conn, sqlcode) %>%
#     arrange(SAMPLE_NAME, ANALYSIS, NAME, ENTERED_ON, SAMPLE_NUMBER) %>%
#     mutate(combi = paste(ANALYSIS, SAMPLE_NAME, NAME, sep = "---"),
#            nominal_value = AVG_EXPECTED)
#   if (is.null(plotdata$UNITS)) plotdata$UNITS <- ""
#   cat('\n', plotdata[[2]], '\n', class(plotdata), file = logfile, append = TRUE)
# }, outFile = logfile)
#
# cat("\ndim plotdata\n", dim(plotdata), file = logfile, append = TRUE, sep ="\n")
#
# try(write_csv(plotdata, path = "\\\\limsbgops.inbo.be\\LABO_FS\\LIMS\\qc_ctr_fixlim.csv"))
#
# ### figuren maken
#
# htmlstart <- '<HTML><HEAD></HEAD><BODY>'
# cat(htmlstart, file = htmlfile, append = FALSE)
#
# cat('\n\nstart loopen over figuren\n', file = logfile, append = TRUE)
# for (comb in unique(plotdata$combi)) {
#   cat("\n", comb, file = logfile, append = TRUE)
#   try(rm(subdata))
#   figpathshort <- paste0(htmlrootshort, "_", make.names(comb), ".png")
#   figpath <- paste0(htmlpath, "\\", figpathshort)
#   subdata <- plotdata %>% filter(combi == comb)
#   subdata$Nr <- 1:nrow(subdata)
#   rules <-
#     lims_shewhart_rules(x = subdata %>% pull(ENTRY) %>% as.numeric(),
#                         center = mean(subdata %>% pull(AVG_MEASURED)),
#                         sd = mean(subdata %>% pull(SD_MEASURED)))
#   subdata <- bind_cols(subdata, as.data.frame(rules))
# <<<<<<< HEAD
#   htmldata <- subdata %>% transmute(Nr, TEXT_ID, BATCH, value = round(value, 5), UNITS,
# =======
#   htmldata <- subdata %>% transmute(Nr, TEXT_ID, BATCH, value = round(value, 5),
# >>>>>>> 8ca6b631dc4df9a454d14417d831562678df01e0
#                                     rules = paste0(rule01, rule02, rule03, rule04, rule05, rule06, rule07, rule08))
#
#   print(comb)
#   print(dim(subdata))
#
#   p <- gg_lims_shewhart(subdata, add_nominal = FALSE) + ggtitle(comb)
#   ggsave(plot = p, filename = figpath, height = 5, width = 7, dpi = 300)
#
#   cat(paste0("\n<H2>", comb, "</H2>\n"), file = htmlfile, append = TRUE)
#   cat(paste0("\n<IMG SRC = \"", figpathshort, "\">\n"), file = htmlfile, append = TRUE)
#
#   htmltabel <- knitr::kable(htmldata, format = "html")
#   cat(htmltabel, file = htmlfile, append = TRUE)
#   cat(paste0("\neinde van ", comb), file = logfile, append = TRUE)
# }
# cat('\n</BODY></HTML>', file = htmlfile, append = TRUE)
#
# shell.exec(htmlfile)
#
