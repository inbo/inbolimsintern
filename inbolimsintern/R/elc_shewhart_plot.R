
#' Plot ELC shewhart plot
#'
#' @param subdata  data om te plotten met kolommen batchnr, waarde, color, size
#' @param borders dataframe met de kolommen val en color om horizontale lijnen voor de s-grenzen te trekken
#' @param base_color basiskleur in de grafieken
#' @param max_s_plot Op hoeveel s moet de plot alles erbuiten niet meer tonen. Indien 0 of NA toon de hele plot
#' @param interactive Moet de plot een interactief plotly object worden?
#' @param title De titel die aan de plot toegevoegd wordt
#' @param fig_height De hoogte van de figuren in pixels
#' @import ggplot2
#' @importFrom plotly ggplotly
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
#' ORDER = 1:15,
#' eval = c(1,NA,NA,1,1,1,1,NA,NA,1,1,1,NA,1,1))
#' borders <- data.frame(lim = -3:3, val = (-3:3)*10,
#' color = c("red", "gold", "green4", "lightblue3", "green4", "gold", "red"))
#' ELC_shewhart_plot(subdata, borders, max_s_plot = 4)
#' ELC_shewhart_plot(subdata, borders, max_s_plot = 0)
#' ELC_shewhart_plot(subdata, borders, max_s_plot = 4, interactive = TRUE)
#' }
#'
ELC_shewhart_plot <- function(subdata, borders = NULL,
                              base_color = "lightblue3",
                              max_s_plot = 5,
                              interactive = FALSE,
                              title = NULL,
                              fig_height = 600) {
  colnames(subdata) <- toupper(colnames(subdata))
  if (is.null(subdata$ENTRY)) subdata$ENTRY <- subdata$WAARDE
  
  if(is.null(base_color)) {
    base_color <- (subdata %>% filter(is.na(.data$EVAL) | .data$EVAL == FALSE) %>% 
                     pull(.data$COLOR))[1]
    if (is.na(base_color)) base_color <- "lightblue3"
  }
  blue_palette <- colorRampPalette(c("darkblue", base_color))
  
  if (is.null(borders)) {
    borders <- data.frame(lim = -3:3,
                          val = c(max(subdata$LCL3S), max(subdata$LCL2S),
                                  max(subdata$LCL1S), max(subdata$C_CTR_X),
                                  max(subdata$UCL1S), max(subdata$UCL2S),
                                  max(subdata$UCL3S)),
                          color = c("red", "orange", "green4", "blue4",
                                    "green4", "orange", "red"))
  }
  if (is.null(title)) {
    title <- subdata$COMBI[1]
  }
  
  # Add hover text to the data (only used in interactive plots)
  subdata$hover_text <- paste("Batch:", subdata$BATCH,
                              "<br>Value:", round(subdata$ENTRY, 2),
                              "<br>Ingelezen:", as.Date(subdata$ENTERED_ON))
  
  # Convert integer64 to numeric/integer to avoid issues
  subdata <- subdata %>%
    arrange(BATCHNR, ORDER) %>%
    mutate(BATCHNR = as.numeric(BATCHNR)) %>%  # Handle integer64
    group_by(BATCHNR) %>%
    mutate(ORDER_WITHIN_BATCH = row_number()) %>%
    ungroup()
  
  max_order <- max(subdata$ORDER_WITHIN_BATCH)
  subdata <- subdata %>%
    mutate(COLOR = ifelse(ORDER_WITHIN_BATCH > 1,
                          blue_palette(max_order)[ORDER_WITHIN_BATCH],
                          COLOR))
  
  evaldata <- subdata %>%
    filter(!is.na(.data$EVAL) & (.data$EVAL != FALSE)) %>%
    arrange(.data$BATCHNR)
  
  zoom_y <- FALSE
  if (max_s_plot > 0 & !(is.na(max_s_plot))) {
    s1 <- (borders[borders$lim == 1, "val"] - borders[borders$lim == -1, "val"])/2
    smin <- borders[borders$lim == 0, "val"] - max_s_plot * s1
    smax <- borders[borders$lim == 0, "val"] + max_s_plot * s1
    
    if (any(subdata$ENTRY > smax, na.rm = TRUE) |
        any(subdata$ENTRY < smin, na.rm = TRUE)) {
      zoom_y <- TRUE
    }
  }
  
  # replace micro symbols
  pattern <- "(\\u00B5|<b5>)(m|S)"
  replacement <- "u\\2"
  units <- gsub(pattern, replacement, max(subdata$UNITS))
  
  # Calculate text size based on number of points
  n_points <- nrow(evaldata)
  text_size <- if(n_points <= 50) {
    8
  } else {
    max(8 * (50/n_points), 4)
  }
  
  suppressWarnings({
    p <- ggplot(data = subdata,
                mapping = aes(x = .data$BATCHNR,
                              y = .data$ENTRY)) +
      geom_point(mapping = aes(text = .data$hover_text),
                 colour = subdata$COLOR) +
      geom_path(data = evaldata,
                mapping = aes(x = .data$BATCHNR,
                              y = .data$ENTRY,
                              group = 1),  # Add explicit group = 1
                colour = base_color) +
      geom_point(data = evaldata,
                 mapping = aes(x = .data$BATCHNR,
                               y = .data$ENTRY,
                               text = .data$hover_text),
                 colour = evaldata$COLOR) +
      geom_hline(data = borders,
                 mapping = aes(yintercept = .data$val),
                 colour = borders$color) +
      scale_x_continuous(breaks = evaldata$BATCHNR,
                         labels = evaldata$BATCH) +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0.5,
                                       size = text_size)) +
      ylab(paste0("Waarde [", units, "]")) +
      xlab(title)
    
    if (zoom_y){
      p <- p + coord_cartesian(ylim = c(smin, smax))
    }
    checkdata <<- ggplot_build(p)
  })
  
  if (!interactive) {
    return(p)
  } else {
    p_interactive <- ggplotly(p,
                              tooltip = "text",
                              height = fig_height)
    return(p_interactive)
  }
}
