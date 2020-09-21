
##########################################################


#' Maak een plaatlayout rapport aan
#'
#' @param data datasset komende uit \link[inbolimsintern]{plate_setup_source}
#' @param Capilar de codes van de plaatkolommen, typisch A tot F
#' @param Lane de nummers van de plaatrijen, typisch 1 tot 12
#' @import dplyr
#' @importFrom dplyr do mutate filter transmute arrange select
#' @importFrom tidyr spread
#' @importFrom rlang .data
#'
#' @return dataset klaar om te exporteren naar excel of naar de C_DNA_RUN_REPORT_RESULTS tabel
#' @export
plate_create_report <- function(data,  Capilar = LETTERS[1:8], Lane = 1:12){

  data <-
    data %>%
    mutate(PARENT_SAMPLE = ifelse(.data$PARENT_SAMPLE == 0 & .data$PARENT_ALIQUOT !=0, .data$PARENT_ALIQUOT, .data$PARENT_SAMPLE),
           PLATE_SEQ = substring(.data$PLATE, regexpr("-", .data$PLATE) + 1, nchar(.data$PLATE)),
           PLATE_POSITION = paste0(LETTERS[.data$ROW_NUMBER], sprintf("%02d", .data$COLUMN_NUMBER)),
           PLATE_ID = paste0(.data$PLATE_SEQ, .data$PLATE_POSITION),
           PARENT_SAMPLE_BIS = ifelse(.data$PARENT_SAMPLE == 0, .data$SAMPLE_NUMBER, .data$PARENT_SAMPLE))

  dfResult <-
    expand.grid(PLATE = sort(unique(data$PLATE)),
                CAPILAR = Capilar,
                LANE = Lane,
                IXNO    = 1:8, #8 locaties om iets te schrijven per cel
                VALUE = NA,
                stringsAsFactors = F) %>%
    arrange(.data$PLATE, .data$LANE, .data$CAPILAR, .data$IXNO) %>%
    mutate(CAPILAR = as.character(.data$CAPILAR),
           PLATE_SEQ = substring(.data$PLATE, regexpr("-", .data$PLATE) + 1, nchar(.data$PLATE)),
           SAMEN = ifelse(.data$IXNO %in% 1:2, .data$IXNO,
                          ifelse(.data$IXNO %in% 3:4, 3,
                                 ifelse(.data$IXNO %in% 5:6, 4, 5))),
           PLATE_ID = paste0(.data$PLATE_SEQ, .data$CAPILAR, sprintf("%02d", .data$LANE)))

  for (i in 1:nrow(data)) {
    if (is.na(data$SAMPLE_NUMBER[i]) | data$SAMPLE_NUMBER[i] == 0) {
      next
    }
    tmp <- as.data.frame(data[i, , drop = FALSE])

    #zou telkens 8 elementen moeten weergeven
    selectie <- which(dfResult$PLATE == tmp$PLATE & dfResult$CAPILAR == LETTERS[tmp$ROW_NUMBER] & dfResult$LANE == tmp$COLUMN_NUMBER)
    dfResult[selectie[1],"VALUE"] <- tmp$LABEL
    dfResult[selectie[7],"VALUE"] <- paste("M", round(tmp$MilliQ,1))
    dfResult[selectie[8],"VALUE"] <- paste("D", round(tmp$DNA,1))

    #zet bij de substalen het ouderstaal tussen haakjes
    if (tmp$SAMPLE_TYPE == "SUBSAMPLE") {
      specimen_parent <- na.omit(data$LABEL[tmp$PARENT_SAMPLE == data$SAMPLE_NUMBER])
      if (length(specimen_parent == 1)) {
        dfResult[selectie[2], "VALUE"] <- paste0("(",specimen_parent, ")")
      }
    }

    corresp <- data$PLATE_ID[data$PARENT_SAMPLE_BIS == tmp$PARENT_SAMPLE & !is.na(data$PARENT_SAMPLE)]
    corresp <- corresp[corresp != tmp$PLATE_ID]
#print(i)
#print(corresp)
    if (length(corresp) & !(all(is.na(corresp)))) {
         for (k in 1:max(4,length(corresp))) {
           dfResult[selectie[2 + k], "VALUE"] <- corresp[k]
         }
    }
  }

  dfResult <-
    dfResult %>%
    arrange(.data$PLATE_SEQ, .data$CAPILAR, .data$LANE, .data$IXNO) %>%
    mutate(LANE = paste0("Lane", sprintf("%02d", .data$LANE))) %>%
    group_by(.data$PLATE, .data$PLATE_SEQ, .data$CAPILAR, .data$LANE, .data$SAMEN) %>%
    mutate(VALUE = ifelse(is.na(.data$VALUE), "", .data$VALUE)) %>%
    summarize(VALUE = paste(.data$VALUE, collapse = "  " ))


   dfResultWide <-
     dfResult %>%
     ungroup() %>%
     select(.data$PLATE, .data$CAPILAR, .data$LANE, .data$SAMEN, .data$VALUE) %>%
     tidyr::spread(key = .data$LANE, value = .data$VALUE)  %>%
     transmute(ID = 1:n(), .data$PLATE, .data$CAPILAR,
               .data$Lane01, .data$Lane02, .data$Lane03, .data$Lane04,
               .data$Lane05, .data$Lane06, .data$Lane07, .data$Lane08,
               .data$Lane09, .data$Lane10, .data$Lane11, .data$Lane12)

  dfResultWide
}
