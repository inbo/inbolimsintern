
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


