

#' Maak een HTML rapport 
#'
#' @param data dataset verkregen na de functie select_control_samples
#' @param path pad indien afwijkend van de werkdirectory. Zeker eindigen met "/". Zorg dat deze directory reeds bestaat.
#' @param digits vector met 2 elementen, het eerste hoeveel digits in de data, het tweede hoeveel in de samenvattende statistieken
#'
#' @importFrom grDevices dev.off png
#' @importFrom stats median na.omit
#' @importFrom utils write.csv2
#' @importFrom dplyr bind_rows arrange desc group_by mutate
#' @return HTML rapport met QC kaarten
#' @export
#'
#' @examples
#' \dontrun{
#' batch = "IC_AN-190430-1"
#' analysis = "IC_ANIONEN"
#' components = c("NO2", "NO3")
#' testdf <- select_control_samples(conn, num = 30, batch, analysis, components)
#' html_qc_report(testdf)
#' }
html_qc_report <- function(data, path = "", digits =  c(5,5)){
  
  ### Identificeer directories, en verwijder vorige HTML bestanden
  
  SAMPLE_NAME <- NAME <- C_DATE_BATCHRUN <- ENTERED_ON <- ANALYSIS <- Nr <- NULL 
  
  htmlfile <- paste0(path, "index.html")
  figpath <- paste0(path, "figure_html/")
  if (!dir.exists(figpath)) dir.create(figpath)
  
  tmp <- list.files(figpath)
  for (i in 1:length(tmp)) try(file.remove(paste0(figpath, tmp[i])), silent = TRUE)
  
  ### Maak een HTML template, met de regels van boven
  
  cat("<HTML><HEAD><TITLE>QC-chart</TITLE></HEAD><BODY>\n", file = htmlfile, append = FALSE)
  
  cat(file = htmlfile, append = TRUE,
      paste(sep = "\n",
            "<h2><b>De regels zijn als volgt:</b></h2><p>",
            "<ol>",
            "<li><b>Regel 1:</b> 1 point is outside the control limits. -->	A large shift.</li>",
            "<li><b>Regel 2:</b> 8/9 points on the same size of the center line.	--> A small sustained shift.</li>",
            "<li><b>Regel 3:</b> 6 consecutive points are steadily increasing or decreasing.	--> A trend or drift up or down.</li>",
            "<li><b>Regel 4:</b> 14 consecutive points are alternating up and down.	--> Non-random systematic variation.</li>",
            "<li><b>Regel 5:</b> 2 out of 3 consecutive points are more than 2 sigmas from the center line in the same direction. -->	A medium shift</li>",
            "<li><b>Regel 6:</b> 4 out of 5 consecutive points are more than 1 sigma from the center line in the same direction. -->	A small shift.</li>",
            "<li><b>Regel 7:</b> 	15 consecutive points are within 1 sigma of the center line.	--> Stratification.</li>",
            "<li><b>Regel 8:</b> 8 consecutive points on either side of the center line with not within 1 sigma.	 --> A mixture pattern.</li>",
            "</ol><p>"
      ))
  
  ### QC Controlekaarten
  
  for (i in unique(data$SAMPLE_NAME)) {
    newdata <- subset(data, SAMPLE_NAME == i)
    for (j in unique(newdata$NAME)) {
      j2 <- convert_to_simple_ascii(j, replace = cbind(181,117))
      cat("<p><h1>",'<span style = "color:red">', j2,"</span>", i, "</h1>\n\n", file = htmlfile, append = TRUE)
      ppdata <- subset(newdata, NAME == j, c("TEXT_ID", "NAME", "BATCH", "ENTRY", "ENTERED_ON", "CV", "DISPLAY_STRING"))
      ppdata$ENTERED_ON <- substring(ppdata$ENTERED_ON,1,10)
      ppdata$Nr <- 1:nrow(ppdata)
      unit <- ppdata$DISPLAY_STRING[1]
      
      qcdata <- lims_shewhart.lims_data(ppdata, entrycol = "ENTRY")
      qcdata[["RVIOL"]] <- paste0("'", qcdata[["RVIOL"]])
      qcdata[["RVIOL"]] <- gsub("1", "X", qcdata[["RVIOL"]])

      g <- gg_lims_shewhart(qcdata)
      #g <- plotly::ggplotly(g)
      cvval <- mean(ppdata$CV)
      
      png(filename = paste0(figpath,"QC_",i,j2,".png"), width = 960, height = 620)
      print(g)
      dev.off()
      
      Sys.sleep(0.1)
      figsav <- paste0("figure_html/","QC_",i,j2,".png")
      cat("\n<img src=",figsav,"</img>", file = htmlfile, append = TRUE)
      
      ppdata <- cbind(ppdata, "Rule viol" = qcdata[["RVIOL"]])
      gem <- mean(ppdata$ENTRY, na.rm = TRUE)
      med <- median(ppdata$ENTRY, na.rm = TRUE)
      sd <- sd(ppdata$ENTRY, na.rm = TRUE)
      lcl <- gem - 1:3 * sd
      ucl <- gem + 1:3 * sd
      cv <- mean(ppdata$CV)
      
      cat("<p><b>gem</b> = ", round(gem,digits[2]), "; <b>CV</b> = ", cv, "<br></br>", file = htmlfile, append = TRUE)
      cat("<b>med</b> = ", round(med, digits[2]), "; <b>sd</b> = ", round(sd, digits[2]), "<br></br>\n", file = htmlfile, append = TRUE)
      cat("<b>UNIT</b> = ", unit, "<br></br></p>\n", file = htmlfile, append = TRUE)
      
      itab <- data.frame(LCL = lcl, UCL = ucl)
      row.names(itab) <- c("1s", "2s", "3s")
      print(xtable::xtable(itab, digits = digits[2]), type = "html", file = htmlfile, append = TRUE, include.rownames = TRUE)
      
      ppdata$CV <- NULL
      ppdata$NAME <- convert_to_simple_ascii(ppdata$NAME, replace = cbind(181,117))
      print(xtable::xtable(ppdata, digits = digits[1]), type = "html", digits = digits[1], file = htmlfile, append = TRUE, include.rownames = FALSE )
      
    }
  }
  
  ### EINDIG HTML en Toon deze
  
  cat("\n</BODY></HTML>", file = htmlfile, append = TRUE)
  print(htmlfile)
  
  shell.exec(htmlfile)
}



