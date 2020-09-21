
#' Shewhart plot
#'
#' @param plotdata a dataset typically obtained through the function lims_shewhart
#' @param red_rules numeric vector of rule violations that should be marked as red
#' @param orange_rules numeric vector of rule violations that should be marked as orange
#' @param yellow_rules numeric vector of rule violations that should be marked as yellow
#' @param ... not currently in use
#' @importFrom ggplot2 ggplot geom_line geom_point aes xlab ylab 
#' @importFrom rlang .data
#' @return Shewhart ggplot 
#' @export
gg_lims_shewhart <- function(plotdata, red_rules = 1:2, orange_rules = 3:6, yellow_rules = 7:8, ...){
  if (!all(red_rules, orange_rules, yellow_rules) %in% 1:8) stop("chosen rules not valid, should be a vector of integers between 1 and 8")
  if (length(red_rules)) {
    plotdata$RR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",red_rules))]) > 0)
  } else {
    plotdata$RR <- 0
  }
  
  if (length(orange_rules)) {
    plotdata$OR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",orange_rules))]) > 0)
  } else {
    plotdata$OR <- 0
  }
  
  if (length(yellow_rules)) {
    plotdata$YR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",yellow_rules))]) > 0)
  } else {
    plotdata$YR <- 0
  }
  plotdata$color <- ifelse(plotdata$RR, "red", ifelse(plotdata$OR, "orange", ifelse(plotdata$YR, "yellow", "black")))
  plotdata
  
  value <- color <- Nr <- NULL #to avoid NOTE in as-cran
  g <- 
    ggplot(plotdata, aes(x = .data$Nr, y = .data$value)) +
    geom_line(aes(y = .data$lcl_1s), lty = 2, color = "gold") + 
    geom_line(aes(y = .data$ucl_1s), lty = 2, color = "gold") +
    geom_line(aes(y = .data$lcl_2s), lty = 2, color = "orange") + 
    geom_line(aes(y = .data$ucl_2s), lty = 2, color = "orange") +
    geom_line(aes(y = .data$lcl_3s), lty = 2, color = "red") + 
    geom_line(aes(y = .data$ucl_3s), lty = 2, color = "red") +
    geom_line(aes(y = .data$center)) +
    geom_line(lty = 3) +
    geom_point(data = subset(plotdata, color == "black"), color = "darkgreen") +
    geom_point(data = subset(plotdata, color == "red"), color = "red", size = rel(3)) +
    geom_point(data = subset(plotdata, color == "orange"), color = "orange", size = rel(2)) +
    geom_point(data = subset(plotdata, color == "yellow"), color = "yellow") +
    xlab("") + ylab("")
  
  g
}
