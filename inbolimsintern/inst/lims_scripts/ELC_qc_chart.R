#ELC - Make QC chart html report

### R libraries
library(inbolimsintern)
library(DBI)
library(tidyverse)

### Logfile
logfile <- logfile_start(prefix = "ELC_Shewhart")
writeLines(con = logfile, paste0("ELC_Shewhart\n-------------\ninbolimsintern versie: ", packageVersion("inbolimsintern")))

### LIMS argumenten
call_id <- 0 #call_id <- 1740 #call_id <- 3134 #call_id <- 3814
try({
  args <- inbolimsintern::prepare_session(call_id)
  conn <- inbolimsintern::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
  params <- inbolimsintern::read_db_arguments(conn, args["call_id"])
}, outFile = logfile)

writeLines(con = logfile, "params\n------\n")
cat(params$VALUE, sep = "\n", file = logfile, append = TRUE)

try({
  sqlfile  <- try(filter(params, ARG_NAME == "SQL_FILE") %>% pull(VALUE))
  htmlfile <- try(filter(params, ARG_NAME == "HTML_FILE") %>% pull(VALUE))
}, outFile = logfile)

## Data

htmlrootshort <- substring(htmlfile,
                           max(unlist(gregexpr("\\\\", htmlfile))) + 1,
                           nchar(htmlfile) - 5) #+1 - 5 (zonder extensie)
htmlpath <-  substring(htmlfile, 1, max(unlist(gregexpr("\\\\", htmlfile))))
alldata <- get_ELC_data(conn, sqlfile, keep = 30)
if (nrow(alldata) == 0) cat("\nGEEN DATA\n", file = logfile, append = TRUE)
combis <- unique(alldata$combi)

## INIT html

htmlstart <- paste0('<HTML>\n<HEAD> call_id: ', args["call_id"], '</HEAD>\n<BODY>\n')
cat(htmlstart, file = htmlfile, append = FALSE)
cat("<H1>Leeswijzer</H1>",
"De blauwe punten worden niet gebruikt bij de berekeningen",
"Overtredingen van regels worden in de figuur en tabel aangeduid.",
"<ul>",
"  <li>Punt telt niet mee: blauwe bol, eval = . </li>",
"  <li>Correct punt: groene bol, eval = ------ </li>",
"  <li>R1: Buiten 3 sigma: rode bol, eval = R1</li>",
"  <li>R2: 2/3 opeenvolgend buiten 2 sigma, zelfde kant: rode bol, eval = R2</li>",
"  <li>R3: 9 opeenvolgende buiten aan zelfde kant gemiddelde: gele bol, eval = R3</li>",
"  <li>R4: 6 opeenvolgende met toenemede of dalende trend: gele bol, eval = R4</li>",
"</ul>",
"<H1>Controlekaarten</H1>",
"\n", sep = "\n", file = htmlfile, append = TRUE)

## Loop through each sample_name, component combination

for (comb in combis) {
  print(comb)
  figpathshort <- paste0(htmlrootshort, "_", make.names(comb), ".png")
  figpath <- paste0(htmlpath, "\\", figpathshort)
  cat("\n", comb, file = logfile, append = TRUE)

  plotdata <- alldata %>% filter(comb == combi)
  htmldata <- elc_htmldata(plotdata)
  p <- ELC_shewhart_plot(subdata = htmldata[["plot"]], htmldata[['borders']] )
  ggsave(plot = p, filename = figpath, height = 4.5, width = 6, dpi = 300)

  cat(paste0("\n<H2>", comb, "</H2>\n"), file = htmlfile, append = TRUE)
  cat(paste0("\n<IMG SRC = \"", figpathshort, "\">\n"),
      file = htmlfile, append = TRUE)
  cat(knitr::kable(htmldata[['summary']], format = "html"),
      file = htmlfile, append = TRUE)
  cat(knitr::kable(htmldata[['tabel']] %>% filter(eval != "."),
                   format = "html"),
      file = htmlfile, append = TRUE)
  fxavg <- htmldata[['summary']] %>% filter(param == "gem") %>% pull(ctr_fix)
  fxsd <- htmldata[['summary']] %>% filter(param == "sd") %>% pull(ctr_fix)
  mx <- fxavg + 3 * fxsd
  mn <- fxavg - 3 * fxsd
  noncalcout3s <- htmldata[['tabel']] %>%
    filter(eval == ".",
           waarde > mx | waarde < mn)
  if (nrow(noncalcout3s)> 0) {
    cat("<h3>Niet weergegeven waarden buiten 3s</h3><p>",
        file = htmlfile, append = TRUE)
    cat(knitr::kable(noncalcout3s,
                     format = "html"),
        file = htmlfile, append = TRUE)
  }


  cat(paste0("\neinde van ", comb), file = logfile, append = TRUE)
}

#Afronden file en html tonen
cat('\n</BODY></HTML>', file = htmlfile, append = TRUE)
shell.exec(htmlfile)







