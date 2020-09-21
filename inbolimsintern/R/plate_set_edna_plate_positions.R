
#' Data verder klaarzettten voor export naar lims platen
#'
#' @param data dataset komende uit de functie get_edna_plate_data
#' @param n_plates aantal platen die gevuld moeten worden
#' @param plate_size grootte van de plaat (standaard 96)
#' @param column_size  aantal lanes (standaard 8 voor een 96 wells plate)
#' @param qcm_pos posities van de qc_methods
#' @param n_blank aantal blanco's per plaat
#' @importFrom rlang .data
#' @import dplyr
#'
#' @return dataset die kan weggeschreven worden in C_DNA_EXTRACTION waar Lims dan verder mee aan de slag kan
#' @export
set_edna_plate_positions <- function(data, n_plates, plate_size = 96, column_size = 8, qcm_pos = 38, n_blank = 3) {
  
  ### >>> Maak lege plaat aan en vul de posities van de controlestalen (Q en B) in
  
  dfPlates <- 
    expand.grid(Plate = 1:n_plates, Position = 1:plate_size, type = NA, sample = NA, origsample = NA) %>%
    arrange(.data$Plate, .data$Position) %>%
    mutate(type = ifelse(.data$Position %in% qcm_pos, "Q", .data$type))
  for (i in 1:n_plates) {
    blank_candi <- 1:plate_size
    blank_pos <- sample(blank_candi[-qcm_pos], size = n_blank)
    dfPlates <- 
      dfPlates %>%
      mutate(type = ifelse(.data$Position %in% blank_pos & .data$Plate == i, "B", .data$type))
  }
  
  ### >>> Splits de plaat op in referentiestalen en echte stalen
  
  dfPlateRefs <- dfPlates %>%
    filter(!is.na(.data$type)) %>% 
    mutate(sample = ifelse(.data$type == "B", -1, 
                           ifelse(.data$type == "Q", -2, NA)))
  
  dfPlateSamps <- dfPlates %>% 
    filter(is.na(.data$type)) %>% 
    select(.data$Plate, .data$Position) %>%
    slice(1:nrow(data)) %>% 
    bind_cols(data)
  
  dfPlateAll <- dfPlateRefs %>%
    bind_rows(dfPlateSamps) %>% 
    arrange(.data$Plate, .data$Position)
  
  #De laatste plaat moet nog eens gerandomiseerd worden
  dfPlateLast  <- filter(dfPlateAll, .data$Plate == max(.data$Plate))
  dfPlateAllmin1 <- filter(dfPlateAll, .data$Plate < max(.data$Plate))
  
  ### >>> Randomiseer de laatste plaat
  
  #indien de plaat minder plaatsen bevat dan waar de QC method staat, randomiseer alle staaltypes
  if (nrow(dfPlateLast) < qcm_pos[length(qcm_pos)]) {
    whi_normal <- which(dfPlateLast$type %in% c("N", "D"))
    whi_qc <- which(dfPlateLast$type %in% c("Q", "B"))
    randomisation <- sample(1:nrow(dfPlateLast))
    dfPlateLastR <- dfPlateLast %>%
      slice(randomisation) %>% 
      mutate(Position = 1:nrow(dfPlateLast))
    
    #sorteer de normale stalen volgens staalnummer zoals in dfPlateLast
    dfPlateLastR$sample[dfPlateLastR$type %in% c("N","D")] <- dfPlateLast$sample[whi_normal]
    dfPlateLastR$origsample[dfPlateLastR$type %in% c("N","D")]  <- dfPlateLast$origsample[whi_normal]
    dfPlateLastR$type[dfPlateLastR$type %in% c("N","D")]  <- dfPlateLast$type[whi_normal]
    dfPlateAll <- bind_rows(dfPlateAllmin1, dfPlateLastR)
    
    #indien de QC method niet op de laatste positie staat, omdat de plaat verder gevuld is dan de QCM positie 
    #dan worden enkel de blanco's gerandomiseerd
  } else {
    blank_pos <- dfPlateLast %>% filter(.data$type == "B" & Position > qcm_pos[length(qcm_pos)]) %>% pull(.data$Position)
    
    #indien er blanco's voorkomen nadat de QC method voorkomt
    if (length(blank_pos) > 0) {
      dfPlateLastR <- dfPlateLast %>% mutate(Position = 1:nrow(dfPlateLast))
      whi_normal <- which(dfPlateLast$type %in% c("N", "D"))
      whi_qc <- which(dfPlateLast$type %in% "Q")
      if (length(whi_qc)) {
        randomisation <- sample((1:nrow(dfPlateLast))[-whi_qc])
      } else {
        randomisation  <- sample(1:nrow(dfPlateLast))
      }
      dfPlateLastR$sample[-whi_qc] <- dfPlateLast$sample[randomisation]
      dfPlateLastR$origsample[-whi_qc] <- dfPlateLast$origsample[randomisation]
      dfPlateLastR$type[-whi_qc] <- dfPlateLast$type[randomisation]
      
      dfPlateLastR$sample[dfPlateLastR$type %in% c("N","D")] <- dfPlateLast$sample[whi_normal]
      dfPlateLastR$origsample[dfPlateLastR$type %in% c("N","D")]  <- dfPlateLast$origsample[whi_normal]
      dfPlateLastR$type[dfPlateLastR$type %in% c("N","D")]  <- dfPlateLast$type[whi_normal]
      
      dfPlateAll <- bind_rows(dfPlateAllmin1, dfPlateLastR)  
      
      #indien alle blanco's voor de QC method komen, is extra randomisatie niet meer nodig  
    } else {
      dfPlateAll <- bind_rows(dfPlateAllmin1, dfPlateLast)
    }
  }
  dfPlateAll
}


