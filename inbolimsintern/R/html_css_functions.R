
#' Get Embedded Assets
#' 
#' @export
get_labware_assets <- function() {
  css_path <- system.file("css", "inbo_labware_styles.css", package = "inbolimsintern")
  logo_path <- system.file("images", "inbo-logo.svg", package = "inbolimsintern")
  
  css_txt <- if(file.exists(css_path)) paste(readLines(css_path, warn = FALSE), collapse = "\n") else ""
  logo_64 <- if(file.exists(logo_path)) paste0("data:image/png;base64,", base64enc::base64encode(logo_path)) else ""
  
  list(css = css_txt, logo = logo_64)
}

################################################################################

#' Get LabWare CSS string
#' @export
get_labware_css <- function() {
  css_path <- system.file("css", "inbo_labware_styles.css", package = "inbolimsintern")
  
  if (css_path == "") {
    warning("CSS file not found in package inst/css/")
    return("")
  }
  
  # Read the file content as a single string
  return(paste(readLines(css_path, warn = FALSE), collapse = "\n"))
}

################################################################################


#' Encode a package image to Base64
#' 
#' @param filename Name of the image file in inst/images/
#' @export
get_embedded_image <- function(filename) {
  img_path <- system.file("images", filename, package = "inbolimsintern")
  
  if (img_path == "") {
    warning("Image not found: ", filename)
    return("")
  }
  
  # Determine MIME type
  ext <- tools::file_ext(filename)
  mime <- paste0("image/", ext)
  
  # Read binary and encode
  encoded <- base64enc::base64encode(img_path)
  return(paste0("data:", mime, ";base64,", encoded))
}

