#' Bereken de ELC basisstatistieken van een numerieke kolom
#'
#' @param x numerieke vector met waarden
#'
#' @return data.frame met basisstatistieken
#' @export
#'
calc_elc_stats <- function(x){
  Norig   <- length(x)
  avgorig <- mean(x, na.rm = TRUE)
  sdorig  <- sd(x, na.rm = TRUE)
  not_all_in_3s <- TRUE
  n <- Norig
  avg = avgorig
  sd = sdorig
  x1 <- x
  #print(x1)

  while(not_all_in_3s & n > 3) {
    n = length(x1)
    avg <- mean(x1, na.rm = TRUE)
    sd  <- sd(x1, na.rm = TRUE)
    min3s <- avg - 3*sd
    max3s <- avg + 3*sd
    if (all(na.omit(x1) <= max3s) & all(na.omit(x1) >= min3s)) {
      not_all_in_3s <- FALSE
    } else {
      del <- which.max(abs(x1 - avg))
      x1 <- x1[-del]
    }
  }
  data.frame(N_all = Norig,
             Avg_all = avgorig,
             Sd_all = sdorig,
             N_used = n,
             Avg = avg,
             Sd = sd,
             min3s = avg - 3*sd,
             max3s = avg + 3*sd)
}

#############################################################
compare_with_tf <- function(x, years){
  x0 <- x$value[x$jaar == years[4]]
  xmin1 <- x$value[x$jaar == years[3]]
  xmin2 <- x$value[x$jaar == years[2]]
  xmin3 <- x$value[x$jaar == years[1]]
  print(c(length(x0), length(xmin1), length(xmin2), length(xmin3)))
  #de vorige jaren worden verder gecombineerd zodat de vergelijkingen zijn:
  #x0 vs xmin1
  #x0 vs c(xmin1, xmin2)
  #x0 vs c(xmin1, xmin2, xmin3)
  #op voorwaarde dat beide kanten minstens 10 observaties hebben

  if (length(x0) >= 10 & length(xmin1) >= 10 ) {
    print(x0)
    print(xmin1)
    ttest1 <- try(t.test(x0, xmin1))
    ftest1 <- try(var.test(x0, xmin1))
    t1 <- ifelse(class(ttest1) == "try-error", NA, ttest1$p.value)
    f1 <- ifelse(class(ftest1) == "try-error", NA, ftest1$p.value)
  } else {
    t1 <- f1 <- NA
  }
  if (length(x0) >= 10 & length(c(xmin1,xmin2)) >= 10 ) {
    ttest2 <- try(t.test(x0, c(xmin1,xmin2)))
    ftest2 <- try(var.test(x0, c(xmin1,xmin2)))
    t2 <- ifelse(class(ttest2) == "try-error", NA, ttest2$p.value)
    f2 <- ifelse(class(ftest2) == "try-error", NA, ftest2$p.value)
  } else {
    t2 <- f2 <- NA
  }
  if (length(x0) >= 10 & length(c(xmin1, xmin2, xmin3)) >= 10 ) {
    ttest3 <- try(t.test(x0, c(xmin1, xmin2, xmin3)))
    ftest3 <- try(var.test(x0, c(xmin1, xmin2, xmin3)))
    t3 <- ifelse(class(ttest3) == "try-error", NA, ttest3$p.value)
    f3 <- ifelse(class(ftest3) == "try-error", NA, ftest3$p.value)
  } else {
    t3 <- f3 <- NA
  }

  rv <- data.frame(t1, f1, t2, f2, t3, f3)
  colnames(rv) <- c(paste0(c("pval_t_", "pval_F_"), paste0(years[4], "-", years[3])),
                    paste0(c("pval_t_", "pval_F_"), paste0(years[4], "-", years[2])),
                    paste0(c("pval_t_", "pval_F_"), paste0(years[4], "-", years[1])))
  rv
}





#' Lijst alle controlestalen waarvoor limieten bestaan op binnen de gekozen periode
#'
#' @param conn connectie naar db object
#' @param min_date vroegste beschouwde datum yyyy-mm-dd
#' @param max_date laatst beschouwde datum yyyy-mm-dd
#'
#' @return dataset met alle resultaatinformatie voor de geselecteerde controlestalen
#' @export
#'
list_all_qc_data <- function(conn, min_date = "2020-01-01", max_date = "2021-01-01") {
  sql <- paste0(
    "select
  s.SAMPLE_NUMBER, s.TEXT_ID, s.SAMPLE_TYPE, s.SAMPLE_NAME,
  s.PRODUCT, s.PRODUCT_VERSION,
  bo.BATCH, bo.ORDER_NUMBER,
  r.ANALYSIS, r.NAME, r.ENTRY, r.ENTERED_ON, r.UNITS,
  ps.C_CERTIFIED_VALUE, ps.C_CERTIFIED_SD,
  ps.C_CTR_X, ps.C_CTR_SD, ps.MIN_VALUE, ps.MAX_VALUE,
  c.C_QC,
  qcs.C_QC_CTR_CHART,
  v.VERSION as ANALYSIS_VERSION
  from test t
  inner join result r on r.test_number = t.test_number
  inner join sample s on s.sample_number = t.sample_number
  inner join PRODUCT_SPEC ps on ps.PRODUCT = s.PRODUCT
  and ps.VERSION = s.PRODUCT_VERSION and s.PRODUCT_GRADE = ps.GRADE and ps.ANALYSIS = t.ANALYSIS and ps.COMPONENT = r.NAME
  inner join component c on c.NAME = ps.COMPONENT
  and c.ANALYSIS = ps.ANALYSIS
  inner join versions v on v.TABLE_NAME = 'ANALYSIS'
  and v.NAME = c.ANALYSIS
  and v.VERSION = c.VERSION
  inner join batch_objects bo on bo.OBJECT_ID = t.TEST_NUMBER
  inner join QC_SAMPLES qcs on qcs.NAME = s.PRODUCT_GRADE
  where r.ENTRY is not null
  and s.SAMPLE_TYPE is not null
  and s.SAMPLE_TYPE <> 'DUP'
  and r.ENTERED_ON > '", min_date, "'",
    " and ENTERED_ON <= '", max_date, "'",
  " and r.STATUS in ('E', 'M', 'A')
  and c.C_QC = 'T'
  and qcs.C_QC_CTR_CHART = 'T'")
  cat(sql)
  DBI::dbGetQuery(conn, sql)
}

#' Verkrijg de ELC data
#'
#' @param dbcon dbconnection object (DBI)
#' @param sqlfile path naar de file die de sql code bevat
#' @param keep aantal batches te behouden voor de figuur
#'
#' @return dataset met alle te verwerken gegevens
#' @export
get_ELC_data <- function(dbcon, sqlfile, keep = 30) {
  sqlcode <- readLines(sqlfile)
  sqlcode <- paste(sqlcode, collapse = "\n")
  plotdata <- DBI::dbGetQuery(dbcon, sqlcode)
  batchvolgorde <- plotdata %>%
    group_by(BATCH) %>%
    summarize(FIRST_ENTRY = min(ENTERED_ON)) %>%
    arrange(FIRST_ENTRY)
  n_batch <- nrow(batchvolgorde)
  batches_to_keep <- batchvolgorde[max(1, n_batch - keep + 1):n_batch, , drop = FALSE]
  batches_to_keep <- batches_to_keep %>% mutate(batchnr = 1:nrow(batches_to_keep))

  plotdata <- plotdata %>%
    inner_join(batches_to_keep) %>%
    mutate(combi = paste(ANALYSIS, SAMPLE_NAME, NAME, sep = "---")) %>%
    arrange(FIRST_ENTRY, BATCH, ORDER_NUMBER)

  attr(plotdata, "sqlcode") <- sqlcode
  plotdata
}

####################################################

#' Maak data aan voor elc shewhart html
#'
#' @param plotdata dataset typisch van get_ELC_data
#' @param colors vector van 5 kleuren die gebruikt worden in plot
#' @param base_size basis puntgrootte in de plot
#' @param expected_value indien NULL berekend uit de data, anders wordt deze gebruikt
#' @param expected_sd indien NULL berekend uit de data, anders wordt deze gebruikt
#' @param digits aantal digits te printen in de html
#'
#' @return lijst met 4 datasets: plot, borders, summary en tabel
#' @export
elc_htmldata <- function(plotdata,
                         colors = c("lightblue3", "green4", "gold", "red", "blue4"),
                         base_size = 1.5,
                         expected_value = NULL,
                         expected_sd = NULL,
                         digits = 5) {
  data <- plotdata %>%
    arrange(batchnr, ORDER_NUMBER) %>%
    mutate(check_rules = !duplicated(batchnr),
           order = 1:nrow(plotdata),
           waarde = as.numeric(ENTRY))
  #neem de limieten van de maximale product versie die in de data aanwezig is
  certified <- mean(data$C_CERTIFIED_VALUE[data$VERSION == max(data$VERSION)])
  certified_sd <- mean(data$C_CERTIFIED_SD[data$VERSION == max(data$VERSION)])

  ctr_x <- ifelse(is.null(expected_value),
                     mean(data$C_CTR_X[data$VERSION == max(data$VERSION)]),
                     expected_value)
  ctr_sd <- ifelse(is.null(expected_sd),
                        mean(data$C_CTR_SD[data$VERSION == max(data$VERSION)]),
                        expected_sd)

  s_borders <- data.frame(lim = -3:3,
                          val = ctr_x + -3:3 * ctr_sd,
                          color = colors[c(4,3,2,5,2,3,4)])

  #Gebruik enkel het eerste punt in een batch van een QC staal voor de berekening
  checkdata <- data %>%
    filter(check_rules == TRUE) %>%
    select(order, waarde)
  values <- checkdata$waarde

  #SOP_033 R1: 1x buiten 3s (out3s)
  checkdata$out3s <- qcc_rule01(values,
                                ctr_x - 3 * ctr_sd,
                                ctr_x + 3 * ctr_sd,
                                run = 1)
  #SOP_033 R2: 2 op 3 buiten 2s aan dezelfde kant (warn)
  checkdata$warn  <- qcc_rule05(values,
                                ctr_x - 2 * ctr_sd,
                                ctr_x + 2 * ctr_sd,
                                run = 3)
  #SOP_033 R4: 6 opeenvolgende stijgend of dalend (drift)
  checkdata$drift <- qcc_rule03(values,
                                run = 6)
  #SOP_033 R3: 9 opeenvolgende aan zelfde kant gemiddelde  (bias)
  checkdata$bias  <- qcc_rule02(values,
                                ctr_x,
                                run = 9)
  checkdata$eval <- paste0(ifelse(checkdata$out3s, 'R1', '--'),
                           ifelse(checkdata$warn,  'R2', '--'),
                           ifelse(checkdata$bias,  'R3', '--'),
                           ifelse(checkdata$drift, 'R4', '--'))

  chart_mean = mean(values)
  chart_sd = sd(values)

  #Maak de data klaar voor de plot
  subdata <- data %>%
    left_join(checkdata, by = c("order", "waarde")) %>%
    arrange(order) %>%
    mutate(color = ifelse(is.na(eval),
                          colors[1],
                          ifelse(out3s | warn,
                                 colors[4],
                                 ifelse(drift | bias,
                                        colors[3], colors[2]))),
           size = ifelse(out3s | warn | drift | bias,
                         1.5 * base_size,
                         base_size))

  #Maak de tabel die in de html verschijnt
  htmldata <- subdata %>%
    transmute(BATCH, TEXT_ID, waarde = round(waarde, 5), UNITS,
              eval = ifelse(is.na(eval), ".", eval))

  #maak tabel met data die tussen plot en tabel komt
  summarydata <- data.frame(param = c("gem", "sd"),
                            certified = round(c(certified, certified_sd),
                                              digits = digits),
                            ctr_fix = round(c(ctr_x, ctr_sd),digits = digits),
                            calculated = round(c(chart_mean, chart_sd), digits = digits))
  return(list(plot = subdata,
              borders = s_borders,
              summary = summarydata,
              tabel = htmldata))
}


##############################################################################

#' Plot ELC shewhart plot
#'
#' @param subdata  data om te plotten met kolommen batchnr, waarde, color, size
#' @param borders dataframe met de kolommen val en color om horizontale lijnen voor de s-grenzen te trekken
#' @param base_color basiskleur in de grafieken
#' @param max_s_plot Op hoeveel s moet de plot alles erbuiten niet meer tonen. Indien 0 of NA toon de hele plot
#' @import ggplot2
#' @return ggplot2 object
#' @export
#' @examples
#' {
#' g <- "green4"; b <- "lightblue3"; r = "red"
#' subdata <- data.frame(batchnr = c(1,1,1,2,3,4,5,5,5,6,7,8,8,9,10),
#' waarde = c(10,-8,20,32,-16,-36,63,16,8,21,1,8,14,-3,9),
#' color = c(g,b,b,r,g,r,r,b,b,g,g,g,b,g,g),
#' size = c(1,NA,NA,2,1,2,NA,NA,NA,1,1,1,NA,1,1),
#' UNITS = "m",
#' BATCH = LETTERS[1:15],
#' eval = c(1,NA,NA,1,1,1,1,NA,NA,1,1,1,NA,1,1))
#' borders <- data.frame(lim = -3:3, val = (-3:3)*10, color = c("red", "gold", "green4", "lightblue3", "green4", "gold", "red"))
#' ELC_shewhart_plot(subdata, borders, max_s_plot = 4)
#' ELC_shewhart_plot(subdata, borders, max_s_plot = 0)
#' }
#'
ELC_shewhart_plot <- function(subdata, borders,
                              base_color = "lightblue3",
                              max_s_plot = 5) {

  evaldata <- subdata %>% filter(!is.na(eval))
  zoom_y <- FALSE
  if (max_s_plot > 0 & !(is.na(max_s_plot))) {
    s1 <- borders[borders$lim == 1, "val"] - borders[borders$lim == 0, "val"]
    smin <- borders[borders$lim == 0, "val"] - max_s_plot *s1
    smax <- borders[borders$lim == 0, "val"] + max_s_plot *s1

    if (any(subdata$waarde>smax, na.rm = TRUE) | any(subdata$waarde<smin, na.rm = TRUE)) {
      zoom_y <-  TRUE
    }
  }
  p <-
    ggplot(subdata, aes(x = batchnr, y = waarde)) +
    geom_point(colour = subdata$color) +
    geom_path(data = evaldata, aes(x = batchnr, y = waarde), colour = base_color) +
    geom_point(data = evaldata, aes(x = batchnr, y = waarde),
               colour = evaldata$color) +
    geom_hline(data = borders, aes(yintercept = val),
               colour = borders$color) +
    scale_x_continuous(breaks = evaldata$batchnr,
                       labels = evaldata$BATCH) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ylab(paste0("Waarde [", max(subdata$UNITS), "]")) + xlab("") +
    ggtitle(subdata$combi[1])
  if (zoom_y){
    p <- p + coord_cartesian(ylim = c(smin, smax))
  }

  checkdata <<- ggplot_build(p) #om debugging mogelijk te maken

  return(p)
}


##############################################################

#' Elementaire testen (t-test en variantie test)
#'
#' @param data data waarin de data voor de t-test en F-test te vinden zijn
#' @param valuecol kolomnaam voor de waarden
#' @param grpcol kolomnaam voor de groepen
#' @param label identificatielabel voor resultaten
#'
#' @return data.frame met resultaten t-test en var.test
#' @export
#'
elc_htest <- function(data, valuecol = "WAARDE", grpcol = "grp", label = "" ) {
  x <- data[, valuecol]
  g <- data[, grpcol]
  x1 <- x[g == 1]
  x2 <- x[g != 1]
  n1 <- length(x1); n2 = length(x2)
  avg1 <- mean(x1, na.rm = TRUE); avg2 = mean(x2, nr.rm = TRUE)
  sdv1 <- sd(  x1, na.rm = TRUE); sdv2 = sd(  x2, na.rm = TRUE)

  ttest <- try(t.test(x1, x2))
  if (class(ttest) != "try-error") {
    tval_t <- ttest$statistic
    pval_t <- ttest$p.value
    lcl_t <-  ttest$conf.int[1]
    ucl_t <-  ttest$conf.int[2]
  } else {
    tval_t <- NA
    pval_t <- NA
    lcl_t <-  NA
    ucl_t <-  NA
  }
  vtest <- try(var.test(x1, x2))
  if (class(vtest) != "try-error") {
    fval_v <- vtest$statistic
    pval_v <- vtest$p.value
    lcl_v <-  vtest$conf.int[1]
    ucl_v <-  vtest$conf.int[2]
  } else {
    fval_v <- NA
    pval_v <- NA
    lcl_v <-  NA
    ucl_v <-  NA
  }
  bind_rows(
    data.frame(label = label, test = "stats",
               n1 = n1, n2 = n2,
               avg1 = avg1, avg2 = avg2,
               sdv1 = sdv1, sdv2 = sdv2),
    data.frame(label = label, test = "t.test",
               stat = tval_t, pval = pval_t,
               lcl = lcl_t, ucl = ucl_t),
    data.frame(label = label, test = "var.test",
               stat = fval_v, pval = pval_v,
               lcl = lcl_v, ucl = ucl_v))
}



