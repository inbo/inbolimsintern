

#' Basisquery voor run chart
#'
#' @param params data.frame met kolommen ARG_NAME (PROJECT, Y1 + optioneel Y2,Y3,Y4,Y5) en VALUE
#' @import dplyr
#' @return query string
#' @export
#'

run_chart_query <- function(params) {
  project <- params %>% filter(.data$ARG_NAME == "PROJECT") %>% pull(.data$VALUE)
  yaxis1 <- params %>% filter(.data$ARG_NAME == "Y1") %>% pull(.data$VALUE)
  yaxis2 <- params %>% filter(.data$ARG_NAME == "Y2") %>% pull(.data$VALUE)
  yaxis3 <- params %>% filter(.data$ARG_NAME == "Y3") %>% pull(.data$VALUE)
  yaxis4 <- params %>% filter(.data$ARG_NAME == "Y4") %>% pull(.data$VALUE)
  yaxis5 <- params %>% filter(.data$ARG_NAME == "Y5") %>% pull(.data$VALUE)

  yaxis1 <- paste0("(r.ANALYSIS = '",
                   paste(unlist(str_split(yaxis1, pattern = "---")),
                         collapse = "' AND r.NAME = '"),
                   "')")
  yaxis2 <- paste0("(r.ANALYSIS = '",
                   paste(unlist(str_split(yaxis2, pattern = "---")),
                         collapse = "' AND r.NAME = '"),
                   "')")
  yaxis3 <- paste0("(r.ANALYSIS = '",
                   paste(unlist(str_split(yaxis3, pattern = "---")),
                         collapse = "' AND r.NAME = '"),
                   "')")
  yaxis4 <- paste0("(r.ANALYSIS = '",
                   paste(unlist(str_split(yaxis4, pattern = "---")),
                         collapse = "' AND r.NAME = '"),
                   "')")
  yaxis5 <- paste0("(r.ANALYSIS = '",
                   paste(unlist(str_split(yaxis5, pattern = "---")),
                         collapse = "' AND r.NAME = '"),
                   "')")
  errstring <- "(r.ANALYSIS = '')"

  sqland <- paste0(yaxis1,
                   " OR ", ifelse(yaxis2 == errstring, "(2 = 0)", yaxis2),
                   " OR ", ifelse(yaxis3 == errstring, "(3 = 0)", yaxis3),
                   " OR ", ifelse(yaxis4 == errstring, "(4 = 0)", yaxis4),
                   " OR ", ifelse(yaxis5 == errstring, "(5 = 0)", yaxis5))
  qry = paste0("select s.PROJECT, s.SAMPLE_NUMBER, s.C_ORIG_DUP_NUMBER, s.PARENT_ALIQUOT, s.TEXT_ID, r.ANALYSIS, r.NAME, r.ENTRY ",
               "from sample s inner join result r on s.SAMPLE_NUMBER = r.SAMPLE_NUMBER ",
               "where s.PROJECT = '", project, "' and r.STATUS in ('E', 'M', 'A') ",
               "AND (", sqland, ")")
  qry
}

#################################################################

#' Get the data voor runchart
#'
#' @param conn db connectie
#' @param qry uit te voeren query
#'
#'@importFrom DBI dbGetQuery
#'@import dplyr
#' @return dataset klaar om een run chart van te plotten
#' @export
#'
get_run_chart_data <- function(conn, qry) {
  plotdata <- DBI::dbGetQuery(conn, qry)

  #zet C_ORIG_DUP_NUMBER op waarde indien 0
  plotdata <- plotdata %>%
    mutate(ENTRY = as.numeric(ENTRY),
           C_ORIG_DUP_NUMBER = ifelse(C_ORIG_DUP_NUMBER == 0, SAMPLE_NUMBER, C_ORIG_DUP_NUMBER),
           TEXT_ID_SOURCE = TEXT_ID,
           TEXT_ID = NULL)

  #als het een aliquot is, neem de parent aliquot over als C_ORIG_DUP_NUMBER
  plotdata <- plotdata %>%
    mutate(C_ORIG_DUP_NUMBER = ifelse(PARENT_ALIQUOT > 0, PARENT_ALIQUOT, C_ORIG_DUP_NUMBER))

  #maak een join om het mininum text id te gebruiken
  plotdata <- plotdata %>%
    inner_join(plotdata %>%
                 group_by(C_ORIG_DUP_NUMBER) %>%
                 summarize(TEXT_ID = TEXT_ID_SOURCE[which.min(C_ORIG_DUP_NUMBER)]),
               by = "C_ORIG_DUP_NUMBER") %>%
    arrange(C_ORIG_DUP_NUMBER, SAMPLE_NUMBER)

  #maak een factor en numerieke waarde van de factor om de plot te voeden
  plotdata <- plotdata %>%
    mutate(TEXT_ID = factor(TEXT_ID, levels = unique(TEXT_ID)), #via unique voor de huidige en niet de alfabetische volgorde
           TEXT_ID_NUM = as.numeric(TEXT_ID))
  plotdata
}

#####################################################################

#' Maak een html met de run chart
#'
#' @param plotdata data om te plotten
#' @param project projectnaam
#' @param path basispad waarin geschreven wordt
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_x_continuous theme ggsave
#' @param split_fig logische waarde die bepaalt of de plots ook uitgesplitst per variabele getoond worden
#' @param ... andere argumenten voor subfuncties zoals show_pagesource voor generate_html_source
#'
#' @return naam van de gerenderde html file
#' @export
#'
html_run_chart <- function(plotdata, project, path, split_fig = TRUE, ...) {
  datetxt <- datetime_text()
  fileprefix <- paste0(project, "_run_chart_", datetxt)
  fileprefix1 <- paste0(project, "_run_chart_split_", datetxt)
  figfilename <- paste0(fileprefix, ".png")
  figfilename1 <- paste0(fileprefix1, ".png")
  htmlfullpath <- paste0(path, "/", fileprefix, ".html")
  figfullpath <- paste0(path, "/", figfilename)
  figfullpath1 <- paste0(path, "/", figfilename1)

  plotdata$anacomp <- interaction(plotdata$ANALYSIS, plotdata$NAME)
  breaks <- run_chart_breaks(n = 40, numeric_labels = plotdata$TEXT_ID_NUM, label_values = plotdata$TEXT_ID)
  p <- ggplot(plotdata,
              aes(x = TEXT_ID_NUM,
                  y = ENTRY,
                  color = anacomp)) +
    geom_point(size = 1, shape = 1) + geom_path() +
    scale_x_continuous(breaks = breaks[['value']],labels = breaks[["label"]], name = "") +
    scale_color_discrete(name = '') +
    ggtitle(project) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  p0 <- p + theme(legend.position = "bottom")
  print(p0)
  ggsave(p0, filename = figfullpath,
         width =7, height = 5, dpi = 300)

  print("here")
  p1 <- p + facet_wrap(~anacomp, ncol = 1, scales = "free_y") +
    theme(legend.position = 'none')
  print(p1)
  ggsave(p1, filename = figfullpath1,
         width =7, height = 5, dpi = 300)

  htmlfile <- htmlfullpath
  print(htmlfile)
  pagesource <-
    generate_html_source(htmlfile,
                         list(paste0("<h1></h1>\n<IMG SRC=\"",figfilename,"\"><p>\n"),
                              paste0("<h1></h1>\n<IMG SRC=\"",figfilename1,"\"><p>\n")))
  htmlfile
}

##########################################

#' Labels voor x as run chart
#'
#' @param numeric_labels numerieke labels (zoals ze gezien worden door de x as)
#' @param label_values vector met labels die horen bij de numeric_labels
#' @param n_max  maximum aantal breaks
#'
#' @return lijst met 2 componten met gelijke lengte: value en label
#' @export
#'
run_chart_breaks <- function(numeric_labels, label_values, n_max = 40) {
  len <- length(numeric_labels)
  orde <- order(numeric_labels)
  numeric_labels <- numeric_labels[orde]
  label_values <- as.character(label_values[orde])
  overflowfactor <- ceiling(len / n_max)

  if (len != length(label_values)) stop("numerieke waarden en labels moeten even lang zijn")
  if (overflowfactor > 1) {
    values <- c(numeric_labels[seq(1, len, by = overflowfactor)], numeric_labels[len])
    labels <- c(label_values  [seq(1, len, by = overflowfactor)],   label_values[len])
    if (values[length(values)] == values[length(values)-1]) {
      values <- values[-length(values)]
      labels <- labels[-length(labels)]
    }
    breaks <- list(value = values, label = labels)
  } else {
    breaks = list(value = numeric_labels, label = label_values)
  }
  breaks
}


#######################################################################

#' Genereer basis html file
#'
#' @param entries list met textstrings die de body van de html uitmaken
#' @param path de volledige padnaam naar de html file
#' @param show_pagesource logische waarde om te bepalen of de html source op het scherm getoond moet worden
#' @param ... andere parameters (nog niet in gebruik)
#'
#' @return html source
#' @export
#'
generate_html_source <- function(path, entries, show_pagesource = FALSE,  ...) {

  pagesource <- "<HTML><HEAD></HEAD><BODY>\n"
  for (i in 1:length(entries)) {
    pagesource <- paste0(pagesource, entries[[i]])
  }
  pagesource <- paste0(pagesource, "\n</BODY></HTML>")
  if (show_pagesource) print(pagesource)
  writeLines(pagesource, con = path)
}
