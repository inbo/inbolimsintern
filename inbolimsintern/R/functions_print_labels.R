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
dynamic_label_template <- function(data_row, format_lines, config, index, total, 
                                   apply_offsets = FALSE) {
  
  dpmm <- as.numeric(config$DPI) / 25.4
  len <- round(config$WIDTH * dpmm)
  hgt <- round(config$LENGTH * dpmm)
  
  # Only apply offsets if requested (typically for physical printer calibration)
  top_offset <- if (apply_offsets && !is.na(config$TOP_OFFSET)) config$TOP_OFFSET else 0
  left_offset <- if (apply_offsets && !is.na(config$LEFT_OFFSET)) config$LEFT_OFFSET else 0
  

  # Initialize ZPL with printer settings
  zpl <- c(
    "^XA",
    "^MCY", # Clears storage to prevent the 'missing last label' buffer issue
    "^MTT",
    "^MNY",
    glue::glue("^MD{config$DARKNESS}"),
    glue::glue("^PR{config$PRINT_RATE}"),
    glue::glue("^PW{len}"),
    glue::glue("^LL{hgt}")
  )
  # 
  # Process each format line
  for (i in seq_len(nrow(format_lines))) {
    line <- format_lines[i, ]
    
    # Apply offsets only if requested
    x <- round((line$LEFT_POSITION + left_offset) * dpmm)
    y <- round((line$TOP_POSITION + top_offset) * dpmm)
    
    # Get content from data row
    content <- data_row[[line$FIELD_NAME]]
    if (is.null(content) || is.na(content)) content <- ""
    
    if (grepl("CODE_128", line$FONT, ignore.case = TRUE)) {
      bar_height <- round(line$FONT_HEIGHT * dpmm)
      bar_module_width <- 2
      
      zpl <- c(zpl,
               glue::glue("^BY{bar_module_width},2.5,{bar_height}"),
               glue::glue("^FO{x},{y}^BC{line$ORIENTATION},,N,N,N^FD{content}^FS")
      )
    } else {
      h <- round(line$FONT_HEIGHT * dpmm)
      w <- round(line$FONT_WIDTH * dpmm)
      
      max_width <- if (!is.na(line$BLOCK_WIDTH) && line$BLOCK_WIDTH > 0) {
        round(line$BLOCK_WIDTH)
      } else {
        NULL
      }
      
      if (!is.null(max_width)) {
        justify <- ifelse(!is.na(line$JUSTIFY), substr(line$JUSTIFY, 1, 1), "L")
        zpl <- c(zpl,
                 glue::glue("^FO{x},{y}^A0{line$ORIENTATION},{h},{w}^FB{max_width},1,0,{justify},0^FD{content}^FS")
        )
      } else if (w == 0 || w == h) {
        zpl <- c(zpl,
                 glue::glue("^FO{x},{y}^A0{line$ORIENTATION},{h}^FD{content}^FS")
        )
      } else {
        zpl <- c(zpl,
                 glue::glue("^FO{x},{y}^A0{line$ORIENTATION},{h},{w}^FD{content}^FS")
        )
      }
    }
  }
  
  zpl <- c(zpl, "^XZ")
  zpl <- paste(zpl, collapse = "\n")
  zpl
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
print_lims_labels <- function(dataset, 
                              printer_config, 
                              format_lines, 
                              mode = c("api", "real"), 
                              abort_real_show_payload = FALSE,
                              format = c("png", "pdf"),
                              output_path = "C:/Labels/label_output.pdf") {
  
  mode <- match.arg(mode)
  format <- match.arg(format)
  
  # Calculate dimensions
  dpmm_val <- round(as.numeric(printer_config$DPI) / 25.4)
  width_in <- round(printer_config$WIDTH / 25.4, 2)
  height_in <- round(printer_config$LENGTH / 25.4, 2)
  
  # Generate ZPL for all labels
  zpl_list <- lapply(seq_len(nrow(dataset)), function(i) {
    dynamic_label_template(dataset[i, ], format_lines, printer_config, i, nrow(dataset))
  })
  # New version (guarantees the printer flushes the last label)
  full_payload <- paste0(paste(zpl_list, collapse = "\n"), "\n")
  
  if (mode == "api") {
    # Build Labelary URL - FIXED for PDF
    if (format == "pdf") {
      # For PDF: use special endpoint with .pdf extension
      url <- sprintf("http://api.labelary.com/v1/printers/%ddpmm/labels/%sx%s/0/label.pdf",
                     dpmm_val, width_in, height_in)
    } else {
      # For PNG: standard endpoint
      url <- sprintf("http://api.labelary.com/v1/printers/%ddpmm/labels/%sx%s/0/",
                     dpmm_val, width_in, height_in)
    }
    
    # Make API request with proper headers
    res <- httr::POST(
      url, 
      body = full_payload,
      httr::add_headers(
        "Accept" = if (format == "pdf") "application/pdf" else "image/png",
        "Content-Type" = "application/x-www-form-urlencoded"
      ),
      encode = "raw"
    )
    
    # Check for errors
    if (httr::status_code(res) != 200) {
      error_msg <- httr::content(res, "text", encoding = "UTF-8")
      stop(
        "Labelary API Error (", httr::status_code(res), "): ", error_msg,
        "\nURL: ", url,
        "\nDPI: ", printer_config$DPI, 
        ", Width: ", printer_config$WIDTH, "mm",
        ", Height: ", printer_config$LENGTH, "mm",
        "\nDPMM: ", dpmm_val,
        ", Width(in): ", width_in,
        ", Height(in): ", height_in
      )
    }
    
    # Get the response content
    content_raw <- httr::content(res, "raw")
    
    # Verify we got the right content type
    content_type <- httr::headers(res)$`content-type`
    message("Received content type: ", content_type)
    
    # Save and display output
    if (format == "pdf") {
      # Ensure directory exists
      output_dir <- dirname(output_path)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      # Verify it's actually a PDF
      if (!grepl("application/pdf", content_type)) {
        warning("Expected PDF but got: ", content_type)
        # Save anyway but with .png extension
        output_path <- sub("\\.pdf$", ".png", output_path)
      }
      
      writeBin(content_raw, output_path)
      message("File saved to: ", output_path)
      message("File size: ", length(content_raw), " bytes")
      
      # Open if on Windows
      if (.Platform$OS.type == "windows") {
        tryCatch({
          shell.exec(normalizePath(output_path))
        }, error = function(e) {
          message("Could not open file automatically: ", e$message)
          message("Please open manually: ", normalizePath(output_path))
        })
      }
    } else {
      # PNG preview
      tmp <- tempfile(fileext = ".png")
      writeBin(content_raw, tmp)
      message("PNG saved to: ", tmp)
      
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::viewer(tmp)
      }
    }
    
  } else { #mode is real
    
    test_labels <<- full_payload
    
    if (abort_real_show_payload) {
      cat(full_payload)
      return(invisible(full_payload))  
    }
    
    # Direct network printing
    printer_path <- paste0("\\\\inbo-print-pr\\", printer_config$PRINTER_PORT)
    tmp_zpl <- tempfile(fileext = ".zpl")
    writeLines(full_payload, tmp_zpl)
    
    result <- shell(paste0('copy /B "', tmp_zpl, '" "', printer_path, '"'), 
                    intern = TRUE)
    
    if (length(result) > 0) {
      message("Label sent to printer: ", printer_config$PRINTER_PORT)
    }
  }
  
  invisible(full_payload)
}


##############################################################################

#' Generate Dynamic ZPL with Diagnostics
dynamic_label_template_debug <- function(data_row, format_lines, config, index, total) {
  
  dpmm <- as.numeric(config$DPI) / 25.4
  
  message("=== Label Generation Debug ===")
  message("DPI: ", config$DPI, " | DPMM: ", round(dpmm, 2))
  message("Label size: ", config$WIDTH, "mm x ", config$LENGTH, "mm")
  message("Label size (dots): ", round(config$WIDTH * dpmm), " x ", round(config$LENGTH * dpmm))
  
  # Initialize ZPL with printer settings
  zpl <- c(
    "^XA",
    glue::glue("^MD{config$DARKNESS}"),
    glue::glue("^PR{config$PRINT_RATE}"),
    glue::glue("^PW{round(config$WIDTH * dpmm)}"),
    glue::glue("^LL{round(config$LENGTH * dpmm)}")
  )
  
  # Process each format line
  for (i in seq_len(nrow(format_lines))) {
    line <- format_lines[i, ]
    
    # Calculate positions
    x <- round((line$LEFT_POSITION + config$LEFT_OFFSET) * dpmm)
    y <- round((line$TOP_POSITION + config$TOP_OFFSET) * dpmm)
    
    content <- data_row[[line$FIELD_NAME]]
    if (is.null(content) || is.na(content)) content <- ""
    
    if (grepl("CODE_128", line$FONT, ignore.case = TRUE)) {
      bar_height <- round(line$FONT_HEIGHT * dpmm)
      bar_module_width <- max(2, min(4, round(line$FONT_WIDTH)))
      
      message("\nBarcode: ", line$FIELD_NAME)
      message("  Position: (", x, ", ", y, ") dots = (", line$LEFT_POSITION, ", ", line$TOP_POSITION, ") mm")
      message("  Height: ", bar_height, " dots = ", line$FONT_HEIGHT, "mm")
      message("  Module width: ", bar_module_width)
      message("  Content: ", content)
      
      zpl <- c(zpl,
               glue::glue("^BY{bar_module_width},2.5,{bar_height}"),
               glue::glue("^FO{x},{y}^BC{line$ORIENTATION},,N,N,N^FD{content}^FS")
      )
    } else {
      h <- round(line$FONT_HEIGHT * dpmm)
      w <- round(line$FONT_WIDTH * dpmm)
      
      message("\nText: ", line$FIELD_NAME)
      message("  Position: (", x, ", ", y, ") dots = (", line$LEFT_POSITION, ", ", line$TOP_POSITION, ") mm")
      message("  Height: ", h, " dots = ", line$FONT_HEIGHT, "mm")
      message("  Width: ", w, " dots = ", line$FONT_WIDTH, "mm")
      message("  Content: ", content)
      
      if (w == 0 || w == h) {
        zpl <- c(zpl,
                 glue::glue("^FO{x},{y}^A0{line$ORIENTATION},{h}^FD{content}^FS")
        )
      } else {
        zpl <- c(zpl,
                 glue::glue("^FO{x},{y}^A0{line$ORIENTATION},{h},{w}^FD{content}^FS")
        )
      }
    }
  }
  
  zpl <- c(zpl, "^XZ")
  result <- paste(zpl, collapse = "")
  
  message("\n=== Generated ZPL ===")
  message(result)
  message("=====================\n")
  
  result
}

# Test with debug version
print_lims_labels_debug <- function(dataset, printer_config, format_lines, 
                                    mode = "api", format = "png",
                                    output_path = "C:/Labels/label_output.pdf") {
  
  # Use debug template
  zpl <- dynamic_label_template_debug(dataset[1, ], format_lines, printer_config, 1, nrow(dataset))
  
  # Continue with normal API call...
  mode <- match.arg(mode, c("api", "real"))
  format <- match.arg(format, c("png", "pdf"))
  
  dpmm_val <- round(as.numeric(printer_config$DPI) / 25.4)
  width_in <- round(printer_config$WIDTH / 25.4, 2)
  height_in <- round(printer_config$LENGTH / 25.4, 2)
  
  if (mode == "api") {
    if (format == "pdf") {
      url <- sprintf("http://api.labelary.com/v1/printers/%ddpmm/labels/%sx%s/0/label.pdf",
                     dpmm_val, width_in, height_in)
    } else {
      url <- sprintf("http://api.labelary.com/v1/printers/%ddpmm/labels/%sx%s/0/",
                     dpmm_val, width_in, height_in)
    }
    
    res <- httr::POST(
      url, 
      body = zpl,
      httr::add_headers(
        "Accept" = if (format == "pdf") "application/pdf" else "image/png",
        "Content-Type" = "application/x-www-form-urlencoded"
      ),
      encode = "raw"
    )
    
    if (httr::status_code(res) != 200) {
      stop("Labelary API Error (", httr::status_code(res), ")")
    }
    
    content_raw <- httr::content(res, "raw")
    
    if (format == "pdf") {
      writeBin(content_raw, output_path)
      shell.exec(normalizePath(output_path))
    } else {
      tmp <- tempfile(fileext = ".png")
      writeBin(content_raw, tmp)
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        rstudioapi::viewer(tmp)
      }
    }
  }
  
  invisible(zpl)
}

#print_lims_labels_debug(samples, p_cfg, f_lns, mode = "api", format = "png")