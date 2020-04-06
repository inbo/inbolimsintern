
#' Brondata voor plaatopvulschema klaarzetten
#'
#' @param data dataset die uit de tabel C_DNA_RUN_REPORT komt
#'
#' @return design dataset voor de plaatlayout
#' @import dplyr
#' @importFrom dplyr do mutate filter transmute arrange select
#' @importFrom tidyr spread
#' @importFrom rlang .data
#' @export
gen_plate_setup_source <- function(data){
  # 
  # dfDesign <- 
  #   data %>%
  #   dplyr::transmute(Replicate = .data$ID, 
  #                    Specimen = as.character(ifelse(.data$Specimen == "leeg", NA, .data$Specimen)),
  #                    Plate = factor(.data$Plate, levels = sort(unique(.data$Plate))),
  #                    Plate_Seq = as.numeric(.data$Plate),
  #                    Capilar = LETTERS[.data$Capilar],
  #                    .data$Lane, 
  #                    SampleType = ifelse(is.na(.data$SampleType) | .data$SampleType == "", "N", 
  #                                        as.character(.data$SampleType)), 
  #                    .data$SampleNumber,
  #                    .data$MilliQ,
  #                    .data$DNA,
  #                    .data$ParentSample, 
  #                    .data$ParentAliquot, 
  #                    ParentAliquotBis = ifelse(.data$ParentAliquot == 0, .data$SampleNumber, .data$ParentAliquot),
  #                    Location = paste0(.data$Plate_Seq, .data$Capilar, .data$Lane)) %>%
  #   dplyr::arrange(.data$Plate, .data$Lane, .data$Capilar)
  # 
  # allsamps <- dfDesign %>% select(.data$SampleNumber, .data$MilliQ, .data$DNA)
  # 
  # dfDesign <- 
  #   bind_cols(
  #     select(dfDesign, -.data$MilliQ, -.data$DNA),
  #     dfDesign %>%
  #       mutate(Nr = row_number()) %>%
  #       rowwise() %>%
  #       do({
  #         if (.data$SampleType == "SUBSAMPLE") {
  #           pid <- which(allsamps$SampleNumber == data$ParentAliquot)
  #           if (length(pid)) {
  #             mqparent <- allsamps[pid, "MilliQ"]
  #             dnaparent <- allsamps[pid, "DNA"]  
  #             rv <- data.frame(MilliQ = mqparent, DNA = dnaparent)
  #           } else {
  #             rv <- data.frame(MilliQ = .data$MilliQ, DNA = .data$DNA)        
  #           }
  #         } else {
  #           rv <- data.frame(MilliQ = .data$MilliQ, DNA = .data$DNA)   
  #         }
  #         rv
  #       }))
  # dfDesign
}
