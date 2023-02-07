#' Krijg basisstatistieken
#'
#' @param x vector with results
#' @param keuze character vector with stats to be calculated (n, nas, mean, sd, se, median, min, max, q.025, q.975,q.25,q.75
#' )
#'
#' @return
#' named vector with the statistics
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' get_base_stats(x, keuze = c("n", "mean", "sd"))
get_base_stats <- function(x, keuze = c("n", "nas", "mean", "sd")) {
  nas <- sum(is.na(x))
  n <- length(x)
  sd <- sd(x, na.rm = TRUE)
  se <- sd/sqrt(n)
  minimum <- min(x)
  maximum <- max(x)
  q.025 <- quantile(x, prob = 0.025)
  q.975 <- quantile(x, prob = 0.975)
  q.25 <- quantile(x, prob = 0.25)
  q.75 <- quantile(x, prob = 0.75)
  avg <- mean(x, na.rm = TRUE)
  rv <- c(n = n, nas = nas, mean = avg, sd = sd, se = se, min = minimum, max = maximum,
          q.025 = q.025, q.975 = q.975, q.25 = q.25, q.75 = q.75)
  rv[keuze]
}

#' Verwijder waarden buiten een vooropgesteld aantal sigmas
#'
#' @param x vector of numerical values
#' @param nsigma numeric value containing the n-sigma values should be removed
#'
#' @return vector without outliers
#' @export
#'
#' @examples
#' x <- rnorm(100000)
#' length(remove_outliers(x))
remove_outliers <- function(x, nsigma = 3) {
  x <- x[!is.na(x)]
  repeat {
    avg <- mean(x)
    sd <- sd(x)
    if(is.na(sd)) return(x)
    rv <- x[(x >= avg - 3 * sd) & (x <= avg + 3 * sd)]

    if (length(rv) == length(x)) break
    x <- rv
  }
  rv
}

#' Calculate stats for ELC Yearchart
#'
#' @param data dataset in the specific format used in elc periodic
#'
#' @return dataset with base stats like n, mean sd, the same qfter removing outliers en the vqlues the previous period
#' @export
#'
calculate_elc_periodic_stats <- function(data) {
  aantallen <- data %>% filter(!is.na(PERIOD)) %>% group_by(PERIOD) %>% summarise(aantal = n())
  data_last <- data %>% filter(PERIOD == "LAST")


  #indien geen data het laatste jaar, stop de routine en vult ook niets in voor de voorgaande jaren
  if (!nrow(data_last)) {
    return(data.frame(n_orig = NA, mean_orig = NA, sd_orig = NA,
                      n = 0, mean = NA, sd = NA,
                      pval_t = NA, pval_f = NA,
                      n_prev = NA, mean_prev = NA, sd_prev = NA))
  }

  #data vorig vorige periode (evenveel elementen als laatste periode)
  data_prev <- data %>% filter(PERIOD != "LAST") %>%
    arrange(rownr) %>%
    slice_tail(n = aantallen %>% filter(PERIOD == "LAST") %>% pull(aantal))

  #bereken eigenschappen zonder waarden buiten 3s weg te gooien
  stats_last <- get_base_stats(data_last$VALUE)

  #itereer tot alle waarden binnen 3s liggen
  kept_values_last <- remove_outliers(data_last$VALUE)
  kept_values_prev <- remove_outliers(data_prev$VALUE)
  stats_last_cleaned <- get_base_stats(kept_values_last)
  stats_prev_cleaned <- get_base_stats(kept_values_prev)

  #neem de laatste waarden (evenveel als het laatste jaar) uit de vorige 2 periodes
  #haal hier ook alle waarden buiten 3s weg

  if (nrow(data_prev) < (nrow(data_last) - 3) | nrow(data_last) < 10) { #indien te weinig waarnemingen, doe geen test
    pval_t = NA
    pval_f = NA
  } else {
    pval_t <- t.test  (kept_values_last, kept_values_prev)$p.value
    pval_f <- var.test(kept_values_last, kept_values_prev)$p.value
  }
  stats_prev <- get_base_stats(data_last$VALUE[stats_last["n"]])
  data.frame(n_orig = round(stats_last["n"]),
             mean_orig = round(stats_last["mean"],4),
             sd_orig = round(stats_last["sd"],4),
             n = round(stats_last_cleaned["n"]),
             mean = round(stats_last_cleaned["mean"],4),
             sd = round(stats_last_cleaned["sd"],4),
             pval_t = round(pval_t,5),
             pval_f = round(pval_f,5),
             n_prev = round(stats_prev_cleaned["n"]),
             mean_prev = round(stats_prev_cleaned["mean"],4),
             sd_prev = round(stats_prev_cleaned["sd"],4))
}






####################################################

#' Genereer ELC data om te archiveren als plot in de lims db
#'
#' @param plotdata te plotten data die BATCHNR, ORDER, ENTRY bevat
#' @param colors basiskleuren voor de kaart
#' @param base_size basisgroote van de bollen
#' @param digits aantal cijfers na de komma behouden
#'
#' @return dataset met alle nodige info om de figuur na te maken zonder berekeningen
#' @export
elc_fixlim_data <- function(plotdata,
                            colors = c("lightblue3", "green4", "orange", "red", "blue4"),
                            base_size = 1.5,
                            digits = 5) {
  data <- plotdata %>%
    arrange(BATCHNR, ORDER_NUMBER) %>%
    mutate(ORDER = 1:nrow(.),
           ENTRY = as.numeric(ENTRY),
           LCL3S = C_CTR_X - 3 * C_CTR_SD,
           LCL2S = C_CTR_X - 2 * C_CTR_SD,
           LCL1S = C_CTR_X - 1 * C_CTR_SD,
           UCL1S = C_CTR_X + 1 * C_CTR_SD,
           UCL2S = C_CTR_X + 2 * C_CTR_SD,
           UCL3S = C_CTR_X + 3 * C_CTR_SD)

  checkdata <- data %>%
    filter(IN_STAT == TRUE) %>%
    select(ORDER, ENTRY, LCL3S, UCL3S, LCL2S,UCL2S)

  #SOP_033 R1: 1x buiten 3s (out3s)
  checkdata$OUT3S <- qcc_rule01(checkdata %>% pull(ENTRY),
                                checkdata %>% pull(LCL3S),
                                checkdata %>% pull(UCL3S),
                                run = 1)

  #SOP_033 R2a: 2 op 2 buiten 2s aan dezelfde kant (out2s)
  #ipv C_CTR_X hier (UCL_2S - UCL_2S)/2 om zo C_CTR_X niet mee te moeten nemen
  checkdata$OUT2S  <- qcc_rule05b(checkdata %>% pull(ENTRY),
                                  checkdata %>% pull(LCL2S),
                                  0.5 * checkdata %>% pull(UCL2S) -
                                  0.5 * checkdata %>% pull(UCL2S),
                                  run = 2)

  #SOP_033 R4: 6 opeenvolgende stijgend of dalend (drift)
  checkdata$DRIFT <- qcc_rule03(checkdata %>% pull(ENTRY),
                                run = 6)
  #SOP_033 R3: 9 opeenvolgende aan zelfde kant gemiddelde  (bias)
  checkdata$BIAS  <- qcc_rule02(checkdata %>% pull(ENTRY),
                                0.5 * checkdata %>% pull(UCL2S) -
                                0.5 * checkdata %>% pull(UCL2S),
                                run = 9)
  checkdata$EVAL <- paste0(ifelse(checkdata$OUT3S, 'R1', '--'),
                           ifelse(checkdata$OUT2S, 'R2', '--'), #keuze labo
                           ifelse(checkdata$BIAS,  'R3', '--'),
                           ifelse(checkdata$DRIFT, 'R4', '--'))

  subdata <- data %>%
    left_join(checkdata, by = c("ORDER", "ENTRY",
                                "LCL3S", "UCL3S",
                                "LCL2S", "UCL2S")) %>%
    arrange(ORDER) %>%
    mutate(COLOR = ifelse(is.na(EVAL),
                          colors[1],
                          ifelse(OUT3S | OUT2S,
                                 colors[4],
                                 ifelse(DRIFT | BIAS,
                                        colors[3], colors[2]))),
           SIZE = ifelse(is.na(OUT3S),
                         base_size,
                         ifelse(OUT3S | OUT2S | DRIFT | BIAS,
                         1.5 * base_size, base_size)))
  subdata

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
#' @param check_rules moeten de grenzen berekend worden of zitten die al als kolom in de data
#'
#' @return lijst met 4 datasets: plot, borders, summary en tabel
#' @export
elc_htmldata <- function(plotdata,
                         colors = c("lightblue3", "green4", "orange", "red", "blue4"),
                         base_size = 1.5,
                         expected_value = NULL,
                         expected_sd = NULL,
                         digits = 5,
                         check_rules = TRUE
                         ) {
  if (check_rules) {

  }

  data <- plotdata %>%
    arrange(BATCHNR, ORDER_NUMBER) %>%
    mutate(CHECK_RULES = !duplicated(BATCHNR),
           ORDER = 1:nrow(plotdata),
           ENTRY = as.numeric(ENTRY))
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
    filter(CHECK_RULES == TRUE) %>%
    select(ORDER, ENTRY)
  values <- checkdata$ENTRY

  #SOP_033 R1: 1x buiten 3s (out3s)
  checkdata$OUT3S <- qcc_rule01(values,
                                ctr_x - 3 * ctr_sd,
                                ctr_x + 3 * ctr_sd,
                                run = 1)
  #SOP_033 R2: 2 op 3 buiten 2s aan dezelfde kant (warn)
  checkdata$WARN  <- qcc_rule05(values,
                                ctr_x - 2 * ctr_sd,
                                ctr_x + 2 * ctr_sd,
                                run = 3)

  #SOP_033 R2a: 2 op 2 buiten 2s aan dezelfde kant (out2s)
  checkdata$OUT2S  <- qcc_rule05b(values,
                                ctr_x - 2 * ctr_sd,
                                ctr_x + 2 * ctr_sd,
                                run = 2)


  #SOP_033 R4: 6 opeenvolgende stijgend of dalend (drift)
  checkdata$DRIFT <- qcc_rule03(values,
                                run = 6)
  #SOP_033 R3: 9 opeenvolgende aan zelfde kant gemiddelde  (bias)
  checkdata$BIAS  <- qcc_rule02(values,
                                ctr_x,
                                run = 9)
  checkdata$EVAL <- paste0(ifelse(checkdata$OUT3S, 'R1', '--'),
                           #ifelse(checkdata$WARN,  'R2', '--'), afw tov iso
                           ifelse(checkdata$OUT2S,  'R2', '--'), #keuze labo
                           ifelse(checkdata$BIAS,  'R3', '--'),
                           ifelse(checkdata$DRIFT, 'R4', '--'))

  chart_mean = mean(values)
  chart_sd = sd(values)

  #Maak de data klaar voor de plot
  subdata <- data %>%
    left_join(checkdata, by = c("ORDER", "ENTRY")) %>%
    arrange(ORDER) %>%
    mutate(COLOR = ifelse(is.na(EVAL),
                          colors[1],
                          ifelse(OUT3S | OUT2S,
                                 colors[4],
                                 ifelse(DRIFT | BIAS,
                                        colors[3], colors[2]))),
           SIZE = ifelse(OUT3S | OUT2S | DRIFT | BIAS,
                         1.5 * base_size,
                         base_size),
           SIZE = ifelse(is.na(SIZE), base_size, SIZE),
           LCL3S = s_borders$val[s_borders$lim == -3],
           LCL2S = s_borders$val[s_borders$lim == -2],
           LCL1S = s_borders$val[s_borders$lim == -1],
           UCL1S = s_borders$val[s_borders$lim == 1],
           UCL2S = s_borders$val[s_borders$lim == 2],
           UCL3S = s_borders$val[s_borders$lim == 3])

  #Maak de tabel die in de html verschijnt
  htmldata <- subdata %>%
    transmute(BATCH, TEXT_ID, ENTRY = round(ENTRY, digits), UNITS,
              EVAL = ifelse(is.na(EVAL), ".", EVAL))


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
ELC_shewhart_plot <- function(subdata, borders = NULL,
                              base_color = "lightblue3",
                              max_s_plot = 5) {
  colnames(subdata) <- toupper(colnames(subdata))
  if (is.null(subdata$ENTRY)) subdata$ENTRY <- subdata$WAARDE

  if (is.null(borders)) {
    borders <- data.frame(lim = -3:3,
                          val = c(max(subdata$LCL3S), max(subdata$LCL2S),
                                  max(subdata$LCL1S), max(subdata$C_CTR_X),
                                  max(subdata$UCL1S), max(subdata$UCL2S),
                                  max(subdata$UCL3S)),
                          color = c("red", "orange", "green4", "blue4",
                                    "green4", "orange", "red"))
  }
  evaldata <- subdata %>% filter(!is.na(.data$EVAL))
  zoom_y <- FALSE
  if (max_s_plot > 0 & !(is.na(max_s_plot))) {
    s1 <- borders[borders$lim == 1, "val"] - borders[borders$lim == 0, "val"]
    smin <- borders[borders$lim == 0, "val"] - max_s_plot *s1
    smax <- borders[borders$lim == 0, "val"] + max_s_plot *s1

    if (any(subdata$ENTRY > smax, na.rm = TRUE) |
        any(subdata$ENTRY < smin, na.rm = TRUE)) {
      zoom_y <-  TRUE
    }
  }

  p <-
    ggplot(subdata, aes(x = .data$BATCHNR, y = .data$ENTRY)) +
    geom_point(colour = subdata$COLOR) +
    geom_path(data = evaldata, aes(x = .data$BATCHNR, y = .data$ENTRY), colour = base_color) +
    geom_point(data = evaldata, aes(x = .data$BATCHNR, y = .data$ENTRY),
               colour = evaldata$COLOR) +
    geom_hline(data = borders, aes(yintercept = .data$val),
               colour = borders$color) +
    scale_x_continuous(breaks = evaldata$BATCHNR,
                       labels = evaldata$BATCH) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ylab(paste0("Waarde [", max(subdata$UNITS), "]")) + xlab("") +
    ggtitle(subdata$COMBI[1])
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



