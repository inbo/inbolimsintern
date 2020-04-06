
#' Randomiseer data, kies substalen, en maak de platen aan
#'
#' @param DNA dataset met DNA_RUN_ID (1 cijfer die voor dit project gelijk blijft), SAMPLE_NUMBER (de sample_numbers uit LIMS), SAMPLE_TYPE (N = normaal sample, Q = QCmethod)
#' @param ... argumenten die doorgegeven worden aan de functie \link{randomiseCapilar}
#' @importFrom dplyr filter transmute mutate arrange bind_rows bind_cols left_join inner_join n pull
#' @importFrom rlang .data
#'
#' @return dataset met naast de velden in DNA, de extra velden PLATE_SEQ (nummer van de plaat), PLATE_POSITION (positie op de plaat) en REP_SAMPLE_NUMBER (staalnummer waarvan dit een substaal is). Deze dataset moet per definitie meer rijen bevatten dan DNA, aangezien er replicates gelogd worden, en nieuwe controlestalen en blanco's indien de stalen over meerdere platen worden verdeeld
#' @export
#'
gen_ms_create_plates <- function(DNA, ...) {

  run_id <- DNA$DNA_RUN_ID[1]

  #De originele staalnummers (die teruggelinkt zullen worden aan de tabel)

  unique_non_QC_orig <-
    DNA %>%
    dplyr::filter(.data$SAMPLE_TYPE == "N") %>%
    dplyr::transmute(non_QC = as.numeric(as.character(.data$SAMPLE_NUMBER))) %>%
    dplyr::arrange(.data$non_QC)

  #defintie van bestaande QC stalen
  QC_samplenumber <-
    DNA %>%
    dplyr::filter(.data$SAMPLE_TYPE == "Q") %>%
    dplyr::transmute(QCsample = .data$SAMPLE_NUMBER) %>%
    dplyr::arrange(.data$QCsample) %>%
    dplyr::pull(.data$QCsample)

  if (length(QC_samplenumber) == 0) {
    QC_samplenumber <- -1
  }

  QCdata <- data.frame(Capilar = c("E", "F"), Line = c(5, 5), ID = c(0, QC_samplenumber), Type = c("B", "Q"))

  #Voer de randomisatiefunctie randomiseCapilar van Thierry uit
  #nCapilar, Lines, rReplicates, minReplicates, fillPlate, firstLabID, prefix via ... ingevuld
  datasetlist <-
    inbolimsintern::randomiseCapilar(
      Specimens = DNA$SAMPLE_NUMBER[DNA$SAMPLE_TYPE == "N"],
      Group = rep("N", sum(DNA$SAMPLE_TYPE == "N")),
      QC = QCdata
    )

  #Converteer datasetlist die uit randomiseCapilar komt naar een data.frame
  dataset <-
    dplyr::inner_join(datasetlist$Specimens, datasetlist$Replicates, by = "Specimen") %>%
    dplyr::arrange(.data$Plate, .data$Lane, .data$Capilar) %>%
    dplyr::mutate(Group = ifelse(duplicated(.data$Specimen) & .data$Group == "N", "S", as.character(.data$Group)),
                  Specimen = as.numeric(as.character(.data$Specimen)),
                  Replicate = 1:n(),
                  Specimen_tmp = .data$Specimen)

  #overview of non_QC_samples after
  unique_non_QC_after <-
    dataset %>%
    dplyr::filter(.data$Group == "N") %>%
    dplyr::transmute(Specimen = as.numeric(as.character(.data$Specimen)))

  #Bind the original samples before and after together (Orig an After must be different)
  dfLink <-
    dplyr::bind_cols(unique_non_QC_orig, unique_non_QC_after) %>%
    dplyr::select(Orig = .data$non_QC, After = .data$Specimen) %>%
    dplyr::bind_rows(data.frame(Orig = QC_samplenumber, After = QC_samplenumber))

  if (is.null(dataset$NUM_POS)) dataset$NUM_POS <- NA
  dataset <-
    dataset %>%
    dplyr::left_join(dfLink, by = c("Specimen" = "After")) %>%
    dplyr::mutate(SampleNumber = ifelse(is.na(.data$Orig) & .data$Group == "B", 0, .data$Orig)) %>%
    dplyr::arrange(.data$Plate, .data$Lane, .data$Capilar) %>% #check if N always before S per sample
    dplyr::transmute(DNA_ID = .data$Replicate, DNA_RUN_ID = run_id,
                     SAMPLE_NUMBER = .data$SampleNumber, SAMPLE_TYPE = .data$Group,
                     PLATE_SEQ = .data$Plate, PLATE_POSITION = paste0(.data$Capilar, .data$Lane),
                     REP_SAMPLE_NUMBER = 0, NUM_POS = .data$NUM_POS) %>%
    dplyr::filter(.data$SAMPLE_TYPE != "X")

  dataset
}
