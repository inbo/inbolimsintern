
#' Plaatdata aanmaken voor sequentie-analyse.
#'
#' @param DNA dataset met DNA_RUN_ID (1 cijfer die voor dit project gelijk blijft), SAMPLE_NUMBER (de sample_numbers uit LIMS), SAMPLE_TYPE (N = normaal sample, Q = QCmethod)
#' @param params de dataset met de parameters die vanuit het lims werden doorgegeven
#' @param qcmpos positie voor de qc method
#' @param blancpos Positie voor de blanco. NULL indien geen blanco's nodig zijn
#' @param platesize de grootte van de plaat
#' @importFrom dplyr filter transmute mutate arrange bind_rows bind_cols left_join inner_join n pull
#' @importFrom rlang .data
#'
#' @return dataset met naast de velden in DNA, de extra velden PLATE_SEQ (nummer van de plaat), PLATE_POSITION (positie op de plaat) en REP_SAMPLE_NUMBER (staalnummer waarvan dit een substaal is). Deze dataset moet per definitie meer rijen bevatten dan DNA, aangezien er replicates gelogd worden, en nieuwe controlestalen en blanco's indien de stalen over meerdere platen worden verdeeld
#' @export
#'
gen_seq_create_plates <- function(DNA, params,  qcmpos = 37, blancpos = NULL, platesize = 96) {
  
  run_id <- as.numeric(params$VALUE[params$ARG_NAME == "DNA_RUN_ID"])
  qcm_sample_number <- as.numeric(params$VALUE[params$ARG_NAME == "QCM_SAMPLE_NUMBER"])
  
  Nsamp <- nrow(DNA)
  Nqc <- length(c(qcmpos, blancpos))
  Nsamp_per_plate <- platesize - Nqc
  Nplates <- 1 + (Nsamp - 1) %/% Nsamp_per_plate
  
  DNA <- arrange(DNA, .data$SAMPLE_NUMBER)
  
  plate_positions <- expand.grid(PLATE_SEQ = 1:Nplates, NUM_POS = 1:platesize) %>% arrange(.data$PLATE_SEQ, .data$NUM_POS)
  plate_positions <- 
    mutate(plate_positions, 
           DNA_RUN_ID = run_id,
           SAMPLE_TYPE = ifelse(.data$NUM_POS %in% qcmpos, "Q", ifelse(.data$NUM_POS %in% blancpos, "B", "S")),
           PLATE_POSITION = paste0(LETTERS[1 + (.data$NUM_POS - 1) %% 8], sprintf("%02d", 1 + (.data$NUM_POS - 1) %/% 8)), 
           SAMPLE_NUMBER = NA,
           pp = 1:n())
  
  #doorloop de lege plaatsen op de platen en vul ze met stalen
  pp <- 1
  i <- 1
  while ( (pp <= max(plate_positions$pp)) & ( i <=  nrow(DNA))) {
    styp <-  plate_positions$SAMPLE_TYPE[pp]
    if (styp == "S") {
      plate_positions$SAMPLE_NUMBER[pp] <- DNA$SAMPLE_NUMBER[i]
      i <- i + 1
    } else if (styp %in% c("B", "Q")) {
      plate_positions$SAMPLE_NUMBER[pp] <- 0
    }
    pp <- pp + 1
  }
  
  last_plate <- max(plate_positions$PLATE_SEQ)
  
  #zet op de eerste plaat de qc method al klaar
  plate_positions$SAMPLE_NUMBER[plate_positions$NUM_POS %in% qcmpos & plate_positions$PLATE_SEQ == 1] <- qcm_sample_number
  
  #verschuif de qc method op de laatste plaat naar de eerste vrije positie
  eerstevrijeplaats <- 
    plate_positions %>%
    filter(.data$PLATE_SEQ == last_plate, is.na(.data$SAMPLE_NUMBER)) %>%
    filter(.data$NUM_POS == min(.data$NUM_POS))
    
  if (eerstevrijeplaats$NUM_POS < qcmpos) {
    qcsamprecord <- filter(plate_positions, .data$PLATE_SEQ == last_plate & .data$SAMPLE_TYPE == "Q")
    plate_positions$SAMPLE_NUMBER[plate_positions$pp == eerstevrijeplaats$pp] <- qcsamprecord$SAMPLE_NUMBER
    plate_positions$SAMPLE_TYPE[plate_positions$pp == eerstevrijeplaats$pp] <- qcsamprecord$SAMPLE_TYPE

    plate_positions$SAMPLE_NUMBER[plate_positions$pp == qcsamprecord$pp] <- NA
  }
  
  filter(plate_positions, !is.na(.data$SAMPLE_NUMBER)) %>% 
    transmute(.data$DNA_RUN_ID, .data$SAMPLE_NUMBER, DNA_ID = 1, .data$SAMPLE_TYPE, .data$PLATE_SEQ, .data$PLATE_POSITION, .data$NUM_POS, REP_SAMPLE_NUMBER = NA)
}
