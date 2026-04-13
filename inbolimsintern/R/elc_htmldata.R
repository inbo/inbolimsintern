

#' Maak data aan voor elc shewhart html
#'
#' @param plotdata dataset typisch van get_ELC_data
#' @param colors vector van 5 kleuren die gebruikt worden in plot
#' @param base_size basis puntgrootte in de plot
#' @param expected_value indien NULL berekend uit de data, anders wordt deze gebruikt
#' @param expected_sd indien NULL berekend uit de data, anders wordt deze gebruikt
#' @param digits aantal digits te printen in de html
#' @param check_rules moeten de grenzen berekend worden of zitten die al als kolom in de data:  OUT3S, OUT2S, WARN, DRIFT, BIAS, COLOR, SIZE, LCL3S, LCL2S, LCL1S, UCL1S, UCL2S, UCL3S, X_CTR_X, C_CTR_SD, C_CERTIFIED_VALUE, C_CERTIFIED_SD, PRODUCT, PRODUCT_VERSION
#' @param reverse_table_order Moet de data van recent naar oud geschikt worden in de html tabel
#'
#' @return lijst met 4 datasets: plot, borders, summary en tabel
#' @export
elc_htmldata <-
  function(plotdata,
           colors = c("lightblue3", "green4", "orange", "red", "blue4"),
           base_size = 1.5,
           expected_value = NULL,
           expected_sd = NULL,
           digits = 5,
           check_rules = TRUE,
           reverse_table_order = TRUE) {

    data <- plotdata %>%
      arrange(BATCHNR, ORDER_NUMBER) %>%
      mutate(CHECK_RULES = !duplicated(BATCHNR),
             ORDER = 1:nrow(plotdata),
             ENTRY = as.numeric(ENTRY))


  #// Bereken de limieten indien gevraagd

  if (check_rules) {
    # >>> BEREKENEN REGELS
    #neem de limieten van de maximale product versie die in de data aanwezig is
    certified <- mean(data$C_CERTIFIED_VALUE[data$VERSION == max(data$VERSION)])
    certified_sd <- mean(data$C_CERTIFIED_SD[data$VERSION == max(data$VERSION)])
    product_version <- paste(sort(unique(data$VERSION)), collapse = ",")
    product <- tryCatch(
      {
        paste(sort(unique(data$PRODUCT)), collapse = ",")
      },
      error = function(e) {
        return(" ")
      }
    )

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
    
    #specifieke wijziging voor Blanco en PBL: rood kleuren vanaf +- 2s ipv 3s
    if ("SAMPLE_TYPE" %in% names(checkdata)) {
      if (checkdata$SAMPLE_TYPE %in% c("BLANCO", "PRBLANCO")) {
        checkdata$OUT3S <- qcc_rule01(values,
                                      ctr_x - 2 * ctr_sd,
                                      ctr_x + 2 * ctr_sd,
                                      run = 1)        
      }
    }
    
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

    values_clean <- values[(values >= ctr_x - 3 * ctr_sd) & (values <= ctr_x + 3 * ctr_sd)]
    chart_mean = mean(values)
    chart_sd = sd(values)
    chart_mean_clean = mean(values_clean)
    chart_sd_clean = sd(values_clean)

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
  } else {
    # >>> ACTIES ZONDER BEREKENEN REGELS
    if (!all(c("OUT3S", "OUT2S", "WARN", "DRIFT", "BIAS", "COLOR", "SIZE",
               "LCL3S", "LCL2S", "LCL1S", "UCL1S", "UCL2S", "UCL3S",
               "C_CTR_X", "C_CTR_SD", "C_CERTIFIED_VALUE", "C_CERTIFIED_SD",
               "PRODUCT", "PRODUCT_VERSION") %in% colnames(plotdata))) {
      stop("Not all necessary columns availaible to be able to skip checking the rules.")
    }
      subdata <- data
      product <- max(subdata$PRODUCT)
      product_version <- max(subdata$PRODUCT_VERSION)
      certified = max(subdata$C_CERTIFIED_VALUE)
      certified_sd = max(subdata$C_CERTIFIED_SD)
      ctr_x = max(subdata$C_CTR_X)
      ctr_sd = max(subdata$C_CTR_SD)

      values = subdata %>%
        filter(CHECK_RULES) %>%
        pull(ENTRY)

      minval <- min(subdata$LCL3S)
      maxval <- max(subdata$UCL3S)
      values_clean = subdata %>%
        filter(CHECK_RULES,
               ENTRY >= minval,
               ENTRY <= maxval) %>%
        pull(ENTRY)

      s_borders <- NULL

      chart_mean <- mean(values, na.rm = TRUE)
      chart_sd <- sd(values, na.rm = TRUE)
      chart_mean_clean <- mean(values_clean, na.rm = TRUE)
      chart_sd_clean <- sd(values_clean, na.rm = TRUE)
  }

  #\\ Maak HTML tabel

  #Maak de tabel die in de html verschijnt
  htmldata <- subdata %>%
    transmute(BATCH, TEXT_ID, ENTRY = round(ENTRY, digits), UNITS,
              EVAL = ifelse(is.na(EVAL), ".", EVAL))


  #\\ Maak summary tabel

  #maak tabel met data die tussen plot en tabel komt
  summarydata <- data.frame(product = c(product, product_version),
                            param = c("gem", "sd"),
                            certified = round(c(certified, certified_sd),
                                              digits = digits),
                            ctr_fix = round(c(ctr_x, ctr_sd),digits = digits),
                            calculated_incl_outliers = round(c(chart_mean, chart_sd), digits = digits),
                            calculated_excl_outliers = round(c(chart_mean_clean, chart_sd_clean), digits = digits))

  #waarden buiten 3s grenzen die mogelijks niet in grafiek staan
  fxavg <- summarydata %>% filter(param == "gem") %>% pull(ctr_fix)
  fxsd <- summarydata %>% filter(param == "sd") %>% pull(ctr_fix)
  mx <- fxavg + 3 * fxsd
  mn <- fxavg - 3 * fxsd
  out3sdata <- NULL
  try(out3sdata <- htmldata %>% filter(EVAL == ".", ENTRY > mx | ENTRY < mn))

  if (reverse_table_order) {
    htmldata <- htmldata[nrow(htmldata):1,,drop = FALSE]
  }

  return(list(plot = subdata,
              borders = s_borders,
              summary = summarydata,
              tabel = htmldata,
              out3s = out3sdata))
}

##############################################################################
