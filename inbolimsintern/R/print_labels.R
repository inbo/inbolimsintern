
#' Print projectlabels on zebra printer or as pdf
#'
#' @param data A dataframe containing columns: external_id, internal_id, project_id
#' @param mode Character: "api" for virtual preview, "real" for physical printing
#' @param format Character: "png" for a single label in RStudio Viewer, "pdf" for all labels in a browser
#' @importFrom glue glue
#' @importFrom httr post
#' @export
#' @examples
#' \dontrun {
#' Load required libraries
#' 'glue' allows us to insert R variables into strings easily using {brackets}
#' 'httr' is used to talk to the Labelary API over the internet
#' library(glue)
#' library(httr)
#' # --- GLOBAL SETTINGS ---
#' # Set the network path for your shared Zebra printer
#' PRINTER_PATH <- "\\\\inbo-print-pr\\G35_LABO_ZEBRA"

#' # Physical label dimensions in inches (70mm x 32mm approx)
#' # Labelary uses these to define the 'canvas' size in the preview
#' LABEL_DIMS <- "2.75x1.25"

#' ###Preview in RStudio Viewer
#' #print_zebra_proj(df, mode = "api", format = 'pdf')

#' ### Send to the real Zebra
#' # print_zebra_proj(df, mode = "real")
#' }
#' 
#' 
print_zebra_proj <- function(data, mode = c("api", "real"), format = c("png", "pdf"), path = PRINTER_PATH) {
  
  # Validate arguments to ensure user didn't mistype mode or format
  mode <- match.arg(mode)
  format <- match.arg(format)
  total_count <- nrow(data)
  
  # --- STEP 1: GENERATE ZPL CODE ---
  # We loop through every row of your data and create a 'block' of ZPL code.
  # We use ^XA to start a label and ^XZ to end it.
  zpl_list <- lapply(1:total_count, function(i) {
    glue(
      "^XA",
      "^PW826^LL378",                      # Set canvas for 300 DPI (70x32mm)
      
      # COUNTER: Top-right corner (X=720, Y=30). Font size 25 dots.
      "^FO720,30^A0N,25,25^FD{i}/{total_count}^FS",
      
      # EXTERNAL ID: Centered at top. ^FB defines a block 826 wide, 'C' is center.
      "^FO0,70^A0N,40,40^FB826,1,0,C^FD{data$external_id[i]}^FS",
      
      # LAB ID (Internal): Centered, large bold font (70 dots high).
      "^FO0,120^A0N,70,70^FB826,1,0,C^FD{data$internal_id[i]}^FS",
      
      # BARCODE: Code 128. ^BY3 sets bar width. ^FO200 creates the requested left-gap.
      "^BY3,3,100^FO200,205^BCN,,N,N,N^FD{data$internal_id[i]}^FS",
      
      # PROJECT ID: Centered at bottom edge.
      "^FO0,325^A0N,35,35^FB826,1,0,C^FD{data$project_id[i]}^FS",
      
      "^XZ"
    )
  })
  
  # Combine all individual label strings into one massive string
  full_payload <- paste(zpl_list, collapse = "\n")
  
  # --- STEP 2: HANDLE OUTPUT MODES ---
  
  if (mode == "api") {
    # We call the Labelary API at 12dpmm (300 DPI) to match your physical printer
    if (format == "png") {
      # PNG Mode: Preview ONLY the first label in the RStudio Viewer pane
      message("Rendering single PNG preview in RStudio...")
      url <- glue("http://api.labelary.com/v1/printers/12dpmm/labels/{LABEL_DIMS}/0/")
      # We send only zpl_list[[1]] because PNGs cannot be multi-page
      response <- POST(url, body = zpl_list[[1]], encode = "raw")
      
      if (status_code(response) == 200) {
        tmp <- tempfile(fileext = ".png")
        writeBin(content(response, "raw"), tmp)
        if (requireNamespace("rstudioapi", quietly = TRUE)) rstudioapi::viewer(tmp)
      }
      
    } else {
      # PDF Mode: Preview ALL labels in your default web browser/PDF viewer
      message(glue("Generating {total_count}-page PDF preview..."))
      url <- glue("http://api.labelary.com/v1/printers/12dpmm/labels/{LABEL_DIMS}.pdf")
      # We send the full_payload (all 50+ labels)
      response <- POST(url, body = full_payload, encode = "raw")
      
      if (status_code(response) == 200) {
        tmp <- tempfile(fileext = ".pdf")
        writeBin(content(response, "raw"), tmp)
        browseURL(tmp)
      }
    }
    
  } else {
    # REAL Mode: Send directly to the physical Zebra printer
    message(glue("Sending {total_count} labels to printer share: {path}"))
    
    # Write the raw ZPL to a temporary text file
    temp_zpl <- tempfile(fileext = ".zpl")
    writeLines(full_payload, temp_zpl)
    
    # Use Windows 'copy /B' to push the file to the print queue as raw binary
    # This bypasses the driver and speaks ZPL directly to the printer
    shell(paste0('copy /B "', temp_zpl, '" "', path, '"'))
    
    # Clean up the temporary file from your computer
    unlink(temp_zpl)
    message("Success: Data transferred to print spooler.")
  }
}

##############################################################################

# Load required libraries
# 'glue' allows us to insert R variables into strings easily using {brackets}
# 'httr' is used to talk to the Labelary API over the internet
# library(glue)
# library(httr)

# --- GLOBAL SETTINGS ---
# Set the network path for your shared Zebra printer
# PRINTER_PATH <- "\\\\inbo-print-pr\\G35_LABO_ZEBRA"

# #Physical label dimensions in inches (70mm x 32mm approx)
# #Labelary uses these to define the 'canvas' size in the preview
#LABEL_DIMS <- "2.75x1.25"

# ###Preview in RStudio Viewer
#print_zebra_proj(df, mode = "api", format = 'pdf')

# ### Send to the real Zebra
# print_zebra_proj(df, mode = "real")

