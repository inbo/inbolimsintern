
#'Randomise specimens over different capilar AFLP plates.
#'
#'This function randomises Specimens over the required number of plates
#'depending on the size of the plates, the minimum ratio of replications and
#'the number of quality control specimens per plate.
#'
#'
#'@param Specimens Either the number of specimens or a vector with the names of
#'the specimens.
#'@param Group A vector indication the a priori clustering of specimens. Must
#'be as long as the number of specimens and in the same order. When missing, no
#'a priori clustering is assumed. Defaults to NULL.
#'@param FirstLabID Start the replicate numbering at this number. Defaults to
#'1.
#'@param Prefix Optional prefix for the replicate names. Defaults to "".
#'@param nCapilar Number of available capilars. Defaults to 8.
#'@param nLines Number of available lines on a plate. Defaults to 12.
#'@param QC A data.frame with the positions of the quality control samples. See
#'the examples.
#'@param rReplicates Percentage of the samples that are reserved for
#'replicates. Default = 0.1 (10\%).
#'@param minReplicates Minimum number of samples reserved for replicates.
#'Defaults to 8. Only relevant when the number of specimens is very low.
#'@param fillPlate If TRUE, all plates will be filled with samples. If needed,
#'extra specimens are replicated.  If FALSE, only the lines will be filled.
#'Possibly leaves one or more lines on the last plate without samples. Defaults
#'to TRUE.
#'@return Results in an AFLP object with randomised replicates.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@keywords design
#'@importFrom utils head tail
#'@importFrom stats aggregate
#'@importFrom rlang .data
#'@importFrom ggplot2 rel
#'@examples
#'
#'	randomiseCapilar(100)
#'
#'	#example with quality control samples
#'	QCsamples <- data.frame(
#'    Capilar = c("E", "F"), 
#'    Line = c(5, 5), 
#'    ID = c("BL", "QCmethod"), 
#'    Type = c("Blanco","QC")
#'  )
#'	nSpecimens <- 180
#'	Group <- sample(10, nSpecimens, replace = TRUE)
#'	randomiseCapilar(nSpecimens, Group, 
#'    FirstLabID = 626, Prefix = "C/11/", QC = QCsamples)
#'
#'@export
randomiseCapilar <- function(Specimens, Group, FirstLabID = 1, Prefix = "", nCapilar = 8, nLines = 12, QC, rReplicates = 0.1, minReplicates = 8, fillPlate = FALSE){
 
  Plate <- Line <- Replicate <- Capilar <- Specimen <- NULL #nodig door de with functie? is niet duidelijk?
  
  ### CHECK INPUT
  if (is.numeric(Specimens) & length(Specimens) == 1) {
    Specimens <- seq_len(Specimens)
  }
  if (missing(Group)) {
    specList <- data.frame(Specimen = Specimens, Group = NA)
  } else {
    specList <- data.frame(Specimen = Specimens, Group = Group)
  }
  if (missing(QC)) {
    QC <- data.frame()
    specList$Specimen <- factor(specList$Specimen)
  } else {
    specList <- rbind(specList, data.frame(Specimen = QC$ID, Group = QC$Type))
    specList$Specimen <- factor(specList$Specimen)
    QC$ID <- factor(QC$ID, levels = levels(specList$Specimen))
  }
  
  ### INIT VARIABLES
  
  Specimens <- factor(Specimens, levels = levels(specList$Specimen))
  nPlate <- ceiling(length(Specimens) / ((nCapilar * nLines - nrow(QC)) * (1 - rReplicates)))
  Design <- expand.grid(Capilar = LETTERS[seq_len(nCapilar)], Line = seq_len(nLines), 
                        Plate = seq_len(nPlate), Specimen = factor(NA, levels = levels(Specimens)), Replicate = NA)
  
  ###
  
  #!
  if (nrow(QC) > 0) {
    Design <- merge(Design, QC, all.x = TRUE)
    Design$Replicate <- with(Design, ifelse(is.na(ID), NA, paste(ID, Plate, sep = "_")))
    Design$Specimen <- Design$ID
    Design$ID <- NULL
    Design$Type <- NULL
  }
  
  #!
  if (!fillPlate) {
    lastLines <- ceiling((
      length(Specimens) + 
        pmax(
          ceiling(
            length(Specimens) * 
              (1 / (1 - rReplicates) - 1)
          ), 
          minReplicates
        ) + 
        nPlate * nrow(QC)
    ) / nCapilar) %% nLines
    if (lastLines > 0) {
      toReposition <- Design[Design$Plate == nPlate & !is.na(Design$Replicate) & Design$Line > lastLines, ]
      Design <- subset(Design, Plate < nPlate | Line <= lastLines)
      if (nrow(toReposition) > 0) {
        toReposition$Line <- lastLines
        while (nrow(toReposition) > 0) {
          x <- head(toReposition, 1)
          if (is.na(Design$Replicate[with(Design, Capilar == x$Capilar & Line == x$Line & Plate == nPlate)])) {
            Design$Replicate[with(Design, Capilar == x$Capilar & Line == x$Line & Plate == nPlate)] <- x$Replicate
            Design$Specimen[with(Design, Capilar == x$Capilar & Line == x$Line & Plate == nPlate)] <- x$Specimen
            toReposition <- tail(toReposition, -1)
          } else {
            if (toReposition$Line[1] > 1) {
              toReposition$Line[1] <- toReposition$Line[1] - 1
            } else {
              toReposition$Line[1] <- lastLines
              toReposition$Capilar[1] <- sample(Design$Capilar[Design$Plate == nPlate & is.na(Design$Replicate)], 1)
            }
          }
        }
      }
    }
  }
  QualityControl <- subset(Design, !is.na(Replicate))
  Design <- subset(Design, is.na(Replicate))
  Design <- Design[with(Design, order(Plate, Line, Capilar)), ]
  Design$Replicate <- sprintf("%s%04i", Prefix, seq_len(nrow(Design)) + FirstLabID - 1)
  Specimens <- sample(Specimens)
  
  ###
  
  #
  while (sum(is.na(Design$Specimen)) - length(Specimens) >= ifelse(nPlate > 1, 5, 3) & length(Specimens) > 0) {
    Design$Prob <- 
      (
        table(subset(Design, is.na(Specimen))$Plate)[Design$Plate] / table(Design$Plate)[Design$Plate]
        * 
          table(subset(Design, is.na(Specimen))$Capilar)[Design$Capilar] / table(Design$Capilar)[Design$Capilar]
      ) ^ 10
    
    Remain <- aggregate(Prob ~ Plate + Capilar, data = subset(Design, is.na(Specimen)), FUN = mean)
    Remain2 <- aggregate(Prob ~ Plate + Capilar, data = subset(Design, is.na(Specimen)), FUN = length)
    Remain <- Remain[Remain2$Prob >= 2, ]
    rm(Remain2)
    if (nrow(Remain) == 0) {
      break
    }
    i <- Remain[sample(seq_len(nrow(Remain)), 1, prob = Remain$Prob), 1:2]
    lines <- sample(subset(Design, is.na(Specimen) & Plate == i$Plate & Capilar == i$Capilar)$Line, 2)
    Design$Specimen[with(Design, Plate == i$Plate & Capilar == i$Capilar & Line %in% lines)] <- Specimens[1]
    Design$Prob <- 
      (
        table(subset(Design, is.na(Specimen))$Plate)[Design$Plate] / table(Design$Plate)[Design$Plate]
        * 
          table(subset(Design, is.na(Specimen))$Capilar)[Design$Capilar] / table(Design$Capilar)[Design$Capilar]
      ) ^ 10
    
    #add replicate within plate and between capilar
    Remain <- subset(Design, is.na(Specimen) & Plate == i$Plate & Capilar != i$Capilar)
    j <- Remain[sample(seq_len(nrow(Remain)), 1, prob = Remain$mean), 1:3]
    Design$Specimen[with(Design, Plate == j$Plate & Capilar == j$Capilar & Line == j$Line)] <- Specimens[1]
    if (length(unique(Design$Plate)) > 1) {
      Design$Prob <- 
        (
          table(subset(Design, is.na(Specimen))$Plate)[Design$Plate] / table(Design$Plate)[Design$Plate]
          * 
            table(subset(Design, is.na(Specimen))$Capilar)[Design$Capilar] / table(Design$Capilar)[Design$Capilar]
        ) ^ 10
      #add replicate between plate and within capilar
      Remain <- subset(Design, is.na(Specimen) & Plate != i$Plate & Capilar == i$Capilar)
      k <- Remain[sample(seq_len(nrow(Remain)), 1, prob = Remain$mean), 1:3]
      Design$Specimen[with(Design, Plate == k$Plate & Capilar == k$Capilar & Line == k$Line)] <- Specimens[1]
      Design$Prob <- 
        (
          table(subset(Design, is.na(Specimen))$Plate)[Design$Plate] / table(Design$Plate)[Design$Plate]
          * 
            table(subset(Design, is.na(Specimen))$Capilar)[Design$Capilar] / table(Design$Capilar)[Design$Capilar]
        ) ^ 10
      #add replicate between plate and between capilar
      Remain <- subset(Design, is.na(Specimen) & Plate != i$Plate & Capilar != i$Capilar)
      m <- Remain[sample(seq_len(nrow(Remain)), 1, prob = Remain$Prob), ]
      Design$Specimen[with(Design, Plate == m$Plate & Capilar == m$Capilar & Line == m$Line)] <- Specimens[1]
    }
    Specimens <- Specimens[-1]
  }
  
  #
  while (any(is.na(Design$Specimen)) & length(Specimens) > 0) {
    toDo <- which(is.na(Design$Specimen))
    if (length(toDo) > 1) {
      Design$Specimen[sample(toDo, 1)] <- Specimens[1]
    } else {
      Design$Specimen[toDo] <- Specimens[1]
    }
    Specimens <- Specimens[-1]
  }
  
  #
  while (any(is.na(Design$Specimen))) {
    Current <- table(factor(Design$Specimen))
    Current <- max(Current) - Current
    if (any(Current > 0)) {
      newSpecimen <- sample(names(Current), 1, prob = Current)
    } else {
      newSpecimen <- sample(names(Current), 1)
    }
    toDo <- which(is.na(Design$Specimen))
    if (length(toDo) > 1) {
      Design$Specimen[sample(toDo, 1)] <- newSpecimen
    } else {
      Design$Specimen[toDo] <- newSpecimen
    }
  }  
  Design$Prob <- NULL
  Design <- rbind(Design, QualityControl)
  Design$Lane <- factor(Design$Line)
  Design$Specimen <- factor(Design$Specimen)
  Design$Replicate <- factor(Design$Replicate)
  Design$Plate <- factor(Design$Plate)
  Design <- Design[with(Design, order(Plate, Lane, Capilar)), ]
  
  ### RETURN
    
  if (nrow(QC) > 0) {
    return(
      #new("AFLP",
        list(
          Specimens = specList,
          Replicates = Design[, c("Replicate", "Specimen", "Plate", "Capilar", "Lane")],
          QC = list(
            Specimens = data.frame(Specimen = QC$ID, Type = QC$Type),
            Replicates = merge(data.frame(Specimen = QC$ID, Type = QC$Type), Design)[, c("Replicate", "Type")]
          )
      )
    )
  } else {
    return(
      list(
          Specimens = specList,
          Replicates = Design[, c("Replicate", "Specimen", "Plate", "Capilar", "Lane")]
      )
    )
  }
}
