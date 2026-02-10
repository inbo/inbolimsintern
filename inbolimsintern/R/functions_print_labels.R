#' Get Printer Configuration from LIMS
#'
#' Fetches hardware-specific settings (DPI, Dimensions, Speed, Darkness) for a given printer name.
#'
#' @param conn A DBI connection object (e.g., from `lims_connect()`).
#' @param printer_object_name Character. The unique NAME of the printer in `T_PRINTER`.
#'
#' @return A named list containing printer parameters.
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- limsdb_connect()
#' config <- get_printer_config(conn, "G35_LABO_ZEBRA")
#' }
get_printer_config <- function(conn, printer_object_name) {
  query <- glue::glue_sql("
    SELECT NAME, PRINTER_PORT, LABEL_PRINTER, TOP_OFFSET, LEFT_OFFSET, 
           WIDTH, LENGTH, DPI, DARKNESS, PRINT_RATE, OUTPUT_FILE_PATH 
    FROM [dbo].[T_PRINTER] 
    WHERE NAME = {printer_object_name}", 
                          .con = conn
  )
  
  config <- DBI::dbGetQuery(conn, query)
  
  if (nrow(config) == 0) {
    stop("Printer config not found for: ", printer_object_name)
  }
  
  as.list(config)
}

######################################

#' Get Label Format Lines from LIMS
#'
#' Retrieves the specific design lines (fields, coordinates, fonts) for a given label format.
#'
#' @param conn A DBI connection object (e.g., from `lims_connect()`).
#' @param format_name Character. The unique name of the format in `T_LABEL_FORMAT`.
#'
#' @return A dataframe containing the layout instructions for the label.
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- limsdb_connect()
#' format_lines <- get_label_format(conn, "PROJ_LABEL_EXTID")
#' }
get_label_format <- function(conn, format_name) {
  query <- glue::glue_sql("
    SELECT T_LABEL_FORMAT, LINE_NUMBER, TYPE, DESCRIPTION, ORDER_NUMBER, 
           FIELD_NAME, LINKED_TABLE_FIELD, SUBROUTINE, FIXED_TEXT, 
           START_POSITION, END_POSITION, FONT, FONT_HEIGHT, FONT_WIDTH, 
           ORIENTATION, TOP_POSITION, LEFT_POSITION, HEIGHT, LENGTH, 
           WIDTH, ROUNDING, GRAPHIC, NATIVE_CODE, THICKNESS, 
           ASCII_IMAGE, SUBROUTINE_PARAMETER, BLOCK_WIDTH, JUSTIFY, 
           HIDE_TIME_ON_DATE
    FROM [D0015_08_Lims].[dbo].[T_LABEL_FORMAT_LINE]
    WHERE T_LABEL_FORMAT = {format_name}
    ORDER BY ORDER_NUMBER", 
                          .con = conn
  )
  
  format_df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(format_df) == 0) {
    stop("No label format lines found for format: ", format_name)
  }
  
  format_df
}

################################################################################

#' Adjust label format positions to fit label height
#'
#' @param format_lines Original format lines from LIMS
#' @param target_height_mm Target label height in mm
#' @param original_height_mm Original designed height (default: 30mm)
#' @return Adjusted format_lines
adjust_label_positions <- function(format_lines, target_height_mm, original_height_mm = 30) {
  
  # Calculate scaling factor
  scale_factor <- target_height_mm / original_height_mm
  
  # Find the bottom-most element
  max_pos <- max(format_lines$TOP_POSITION + format_lines$FONT_HEIGHT, na.rm = TRUE)
  
  message("Original layout extends to: ", max_pos, "mm")
  message("Target label height: ", target_height_mm, "mm")
  message("Scale factor: ", round(scale_factor, 3))
  
  # Adjust positions and heights proportionally
  format_lines$TOP_POSITION <- format_lines$TOP_POSITION * scale_factor
  format_lines$FONT_HEIGHT <- format_lines$FONT_HEIGHT * scale_factor
  format_lines$FONT_WIDTH <- format_lines$FONT_WIDTH * scale_factor
  
  new_max_pos <- max(format_lines$TOP_POSITION + format_lines$FONT_HEIGHT, na.rm = TRUE)
  message("Adjusted layout extends to: ", round(new_max_pos, 2), "mm")
  
  return(format_lines)
}

################################################################################

#' Create Dynamic ZPL from LIMS Format Lines
#'
#' Iterates through label format definitions and maps them to sample data.
#'
#' @param data_row A single row of sample data (e.g., `SAMPLE_ID`, `PROJECT`).
#' @param format_lines Dataframe. Rows from `T_LABEL_FORMAT_LINE` defining fields and positions.
#' @param config List. The printer configuration object returned by `get_printer_config`.
#' @param index Integer. Current label index in the batch.
#' @param total Integer. Total labels in the batch.
#' @param apply_offsets Logical. Whether to apply TOP_OFFSET and LEFT_OFFSET from config. 
#'        Set FALSE for preview, TRUE for physical printing.
#'
#' @return A character string of ZPL code for a single label.
#' @export
dynamic_label_template <- function(data_row, format_lines, config, index, total) {
  
  dpmm <- as.numeric(config$DPI) / 25.4 
  len_dots <- round(as.numeric(config$WIDTH) * dpmm)
  hgt_dots <- round(as.numeric(config$LENGTH) * dpmm)
  
  # Dynamic offset calculation (keep your existing logic)
  max_content_y_mm <- max(format_lines$TOP_POSITION + format_lines$FONT_HEIGHT)
  current_offset <- as.numeric(config$TOP_OFFSET)
  if ((max_content_y_mm + current_offset) > as.numeric(config$LENGTH)) {
    current_offset <- max(0, as.numeric(config$LENGTH) - max_content_y_mm)
    message("Label ", index, ": Content overflow detected. Reducing TOP_OFFSET to ", round(current_offset, 2), "mm")
  }
  
  zpl <- c(
    "^XA",
    "^MNN",        # Gap-sensing labels
    "^JUS",        # Auto-calibrate before printing
    "~JA",         # ← ADD THIS: Clear all jobs in buffer
    glue::glue("^MD{config$DARKNESS}"),
    glue::glue("^PR{config$PRINT_RATE}"),
    glue::glue("^PW{len_dots}"),
    glue::glue("^LL{hgt_dots}")
  )
  
  # ... rest of your existing code for fields/barcodes ...
  
  for (i in seq_len(nrow(format_lines))) {
    line <- format_lines[i, ]
    x <- round((line$LEFT_POSITION + config$LEFT_OFFSET) * dpmm)
    y <- round((line$TOP_POSITION + current_offset) * dpmm)
    content <- as.character(data_row[[line$FIELD_NAME]])
    if (length(content) == 0 || is.na(content)) content <- ""
    
    if (grepl("CODE_128", line$FONT, ignore.case = TRUE)) {
      bar_h <- round(line$FONT_HEIGHT * dpmm)
      zpl <- c(zpl,
               glue::glue("^BY2,2.0,{bar_h}"),
               glue::glue("^FO{x},{y}^BC{line$ORIENTATION},,N,N,N^FD{content}^FS"))
    } else {
      f_h <- round(line$FONT_HEIGHT * dpmm)
      f_w <- round(line$FONT_WIDTH * dpmm)
      
      if (!is.na(line$BLOCK_WIDTH) && line$BLOCK_WIDTH > 0) {
        b_w <- if(line$BLOCK_WIDTH > 100) round(line$BLOCK_WIDTH) else round(line$BLOCK_WIDTH * dpmm)
        b_w <- min(b_w, len_dots - x - 10)
        justify <- if(!is.na(line$JUSTIFY)) substr(line$JUSTIFY, 1, 1) else "L"
        zpl <- c(zpl, glue::glue("^FO{x},{y}^A0{line$ORIENTATION},{f_h},{f_w}^FB{b_w},1,0,{justify},0^FD{content}^FS"))
      } else {
        zpl <- c(zpl, glue::glue("^FO{x},{y}^A0{line$ORIENTATION},{f_h},{f_w}^FD{content}^FS"))
      }
    }
  }
  
  zpl <- c(zpl, "^XZ")
  return(paste0(paste(zpl, collapse = "\n"), "\n"))
}

################################################################################


#' Master function to handle API previews or direct network printing.
#'
#' @param dataset Dataframe. The sample data to print.
#' @param printer_config List. Hardware settings.
#' @param format_lines Dataframe. Label design definitions.
#' @param mode Character. "api" for preview, "real" for physical print.
#' @param format Character. "png" (single) or "pdf" (batch) for API previews.
#' @param output_path Character. Path to save the output file.
#' @param abort_real_show_payload stop just before printing on the printer and show the payload that would be sent
#'
#' @export
#' @examples
#' \dontrun{
#' conn <- limsdb_connect()
#' p_cfg <- get_printer_config(conn, "G35_LABO_ZEBRA")
#' f_lns <- get_label_format(conn, 'PROJ_LABEL_EXTID')
#' f_lns <- adjust_label_positions(f_lns, target_height_mm = p_cfg$LENGTH)

#' samples <- data.frame(SAMPLE_ID = "extern id van dit", TEXT_ID="D26-123456-01", PROJECT="V-25W100-05")
#' 
#' # Preview as PDF
#' print_lims_labels(samples, p_cfg, f_lns, mode = "api", format = "pdf")
#' }
#' @export
print_lims_labels <- function(dataset, printer_config, format_lines, 
                              mode = c("api", "real"), 
                              abort_real_show_payload = FALSE,
                              format = c("png", "pdf"),
                              output_path = "C:/Labels/label_output.pdf") {
  
  mode <- match.arg(mode)
  format <- match.arg(format)
  
  # Generate ZPL list
  zpl_list <- lapply(seq_len(nrow(dataset)), function(i) {
    dynamic_label_template(dataset[i, ], format_lines, printer_config, i, nrow(dataset))
  })
  
  if (mode == "api") {
    # Combine all labels for the API
    full_payload <- paste(zpl_list, collapse = "\n")
    
    # 300 DPI corresponds to 12 dpmm in Labelary
    # Dimensions: 69.85mm x 31.75mm is approx 2.75 x 1.25 inches
    url <- "http://api.labelary.com/v1/printers/12dpmm/labels/2.75x1.25/"
    
    res <- httr::POST(
      url, 
      body = full_payload, 
      encode = "raw",
      httr::add_headers("Accept" = if(format == "pdf") "application/pdf" else "image/png")
    )
    
    if (httr::status_code(res) != 200) stop("API Error: ", httr::content(res, "text"))
    
    content_raw <- httr::content(res, "raw")
    if (format == "pdf") {
      writeBin(content_raw, output_path)
      message("Success! Multi-page PDF created at: ", output_path)
      if (.Platform$OS.type == "windows") shell.exec(normalizePath(output_path))
    } else {
      # For PNG, RStudio viewer will show the first label of the batch
      tmp <- tempfile(fileext = ".png")
      writeBin(content_raw, tmp)
      rstudioapi::viewer(tmp)
    }
    
  }
  if (mode == "real") {
    if (abort_real_show_payload) {
      cat(paste(zpl_list, collapse = "\n---\n"))
      return(invisible(zpl_list))
    }
    
    # Real Printer: One-by-One Loop
    printer_path <- paste0("\\\\inbo-print-pr\\", printer_config$PRINTER_PORT)
    
    # ADD THIS: Reset printer memory
    reset_zpl <- "~JA"  # Delete all jobs in buffer
    tmp_reset <- tempfile(fileext = ".zpl")
    writeLines(reset_zpl, tmp_reset)
    shell(paste0('copy /B "', tmp_reset, '" "', printer_path, '"'), intern = TRUE)
    Sys.sleep(1)
    
    calibration_zpl <- "^XA^JUS^XZ\n"
    tmp_cal <- tempfile(fileext = ".zpl")
    writeLines(calibration_zpl, tmp_cal)
    shell(paste0('copy /B "', tmp_cal, '" "', printer_path, '"'), intern = TRUE)
    Sys.sleep(1)  # Give printer time to calibrate
    for (i in seq_along(zpl_list)) {
      cat(sprintf("\n=== LABEL %d ZPL ===\n", i))
      cat(zpl_list[[i]])
      tmp_zpl <- tempfile(fileext = ".zpl")
      writeLines(zpl_list[[i]], tmp_zpl)
      shell(paste0('copy /B "', tmp_zpl, '" "', printer_path, '"'), intern = TRUE)
      Sys.sleep(2.5) 
      
      if (i %% 10 == 0) {
        message(sprintf("Printed %d of %d labels", i, length(zpl_list)))
      }
    }
  }
  message(sprintf("Completed: %d labels sent to printer", length(zpl_list)))
  invisible(zpl_list)
}