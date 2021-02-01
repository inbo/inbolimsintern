

#' Basisquery voor run chart
#'
#' @param params data.frame met kolommen ARG_NAME (PROJECT, Y1 + optioneel Y2,Y3,Y4,Y5) en VALUE
#' @param index wanneer verschillende figuren gemaakt worden, wordt hier de juiste info geselecteerd
#' @import dplyr
#' @return query string
#' @export
#'

run_chart_query <- function(params, index = 1) {
  chartname <- (params %>% filter(.data$ARG_NAME == "CHART") %>% pull(.data$VALUE))[index]
  if (is.na(chartname)) chartname = " "
  project <- (params %>% filter(.data$ARG_NAME == "PROJECT") %>% pull(.data$VALUE))[index]
  yaxis1 <- (params %>% filter(.data$ARG_NAME == "Y1") %>% pull(.data$VALUE))[index]
  yaxis2 <- (params %>% filter(.data$ARG_NAME == "Y2") %>% pull(.data$VALUE))[index]
  yaxis3 <- (params %>% filter(.data$ARG_NAME == "Y3") %>% pull(.data$VALUE))[index]
  yaxis4 <- (params %>% filter(.data$ARG_NAME == "Y4") %>% pull(.data$VALUE))[index]
  yaxis5 <- (params %>% filter(.data$ARG_NAME == "Y5") %>% pull(.data$VALUE))[index]

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
    arrange(TEXT_ID)

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
#' @param chart_header De titel voor de chart, niveau H1
#' @param ... andere argumenten voor subfuncties zoals show_pagesource voor generate_html_source
#'
#' @return naam van de gerenderde html file
#' @export
#'
html_run_chart <- function(plotdata, project, path, split_fig = TRUE, chart_header = "", ...) {
  datetxt <- datetime_text()
  file0 <- paste0(path, "_", chart_header, "_fig_combined.png")
  file1 <- paste0(path, "_", chart_header, "_fig_split.png")
  fileshort0 <- substring(file0, max(gregexpr("\\\\", file0)[[1]]) + 1)
  fileshort1 <- substring(file1, max(gregexpr("\\\\", file1)[[1]]) + 1)

  plotdata <- plotdata %>%
    mutate(anacomp = interaction(ANALYSIS, NAME)) %>%
    arrange(TEXT_ID)

  ulabs <- distinct(plotdata[c('TEXT_ID_NUM', 'TEXT_ID')])
  breaks <- run_chart_breaks(n = 40, numeric_labels = ulabs$TEXT_ID_NUM, label_values = ulabs$TEXT_ID)
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
  ggsave(p0, filename = file0,
         width =7, height = 5, dpi = 300)

  p1 <- p + facet_wrap(~anacomp, ncol = 1, scales = "free_y") +
    theme(legend.position = 'none')
  print(p1)
  ggsave(p1, filename = file1,
         width =7, height = 5, dpi = 300)

  paste0("<H1>", chart_header, "<H1>\n",
         "<h2>Gecombineerd</h2>\n<IMG SRC=\"",fileshort0,"\"><p>\n",
         "<h2>Gesplitst</h2>\n<IMG SRC=\"",fileshort1,"\"><p>\n")
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

  print(data.frame(numeric_labels, label_values))
  print(overflowfactor)

  if (len != length(label_values)) stop("numerieke waarden en labels moeten even lang zijn")
  if (overflowfactor > 1) {
    values <- c(numeric_labels[seq(1, len, by = overflowfactor)], numeric_labels[len])
    labels <- c(label_values  [seq(1, len, by = overflowfactor)],   label_values[len])
    print(values)
    print(labels)
    if (values[length(values)] == values[length(values)-1]) {
      values <- values[-length(values)]
      labels <- labels[-length(labels)]
    }
    breaks <- list(value = values, label = labels)
  } else {
    breaks = list(value = numeric_labels, label = label_values)
  }
  print(breaks)
  breaks
}


#######################################################################

