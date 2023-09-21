
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
