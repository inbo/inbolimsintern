#' Load environment variables from an encrypted .Renviron file
#'
#' This function searches for a '.Renviron.enc' file in the current working
#' directory and the user's home directory, decodes its Base64 content,
#' and loads the variables for the current R session.
#' @param path optional path (if not provided, R will search for it)
#' @return Invisibly returns `TRUE` on success or `FALSE` on failure.
load_encrypted_renviron <- function(path) {

  cat(as.character(Sys.time()),"\n", file = "decoding_log.log", append = FALSE)
  # Define potential paths for the encrypted file
  paths_to_check <- c(
    file.path(getwd(), ".Renviron.enc"),
    file.path(path.expand("~"), ".Renviron.enc"),
    file.path("D:", "LWL8DEV", "Client", ".Renviron.enc"), #temporary location
    file.path("D:", "LWL8UAT", "Client", ".Renviron.enc"), #temporary location
    file.path("D:", "LWL8PRD", "Client", ".Renviron.enc")  #temporary location
  )
  cat(paths_to_check, file = "decoding_log.log", append = TRUE)
  if (!missing(path)) {
    paths_to_check <- c(path, paths_to_check)
  }

  # Find the first path that actually exists
  found_path <- NULL
  for (path in paths_to_check) {
    if (file.exists(path)) {
      found_path <- path
      break
    }
  }
  cat(found_path,"\n", file = "decoding_log.log", append = TRUE)

  if (is.null(found_path)) {
    # It's not an error if the file doesn't exist; just means we can't load it.
    stop("ERROR: .Renviron file not found:", found_path)
  }

  e <- try({
    encoded_content <- readLines(found_path, warn = FALSE)[1]
  })
  if (inherits(e, "try-error")) {
    stop(e)
  }

  e <- try({
    decoded_content <- rawToChar(base64enc::base64decode(encoded_content))
  })
  if (inherits(e, "try-error")) {
    stop(e)
  }
  cat(decoded_content, file = "decoding_log.log", append = TRUE)

  e <- try({
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    writeLines(decoded_content, temp_file)
  })
  if (inherits(e, "try-error")) {
    stop(e)
  }

  e <- try({
    readRenviron(temp_file)
  })
  if (inherits(e, "try-error")) {
    stop("Failed to read or decode the encrypted .Renviron file: ", found_path, call. = FALSE)
  } else {
    message("Environment variables retrieved")
  }
  invisible()
}

#################################
#################################
#################################


#' Detect the current operating environment (DEV, UAT, or PRD or LW7)
#'
#' This function attempts to identify the environment by checking system
#' variables, hostnames, or known directory structures.
#' @param odbc the name of the odbc passed on by commandArgs(trailingOnly = TRUE)[1]
#'
#' @return A string: "DEV", "UAT", or "PRD" or "LW7". Defaults to "PRD" with a warning
#'   if no specific environment can be identified.
get_current_environment <- function(odbc) {
  env <-
    dplyr::case_when(
      length(grep("PRD", odbc)) == 1 ~ "PRD",
      length(grep("UAT", odbc)) == 1 ~ "UAT",
      length(grep("DEV", odbc)) == 1 ~ "DEV",
      length(grep("D0015", odbc)) == 1 ~ "LW7",
      .default = "NA" )
  # Method 1: Check for a system environment variable (most reliable)
  # On the server, you could set an OS-level variable R_ENV="PRD"
  # env <- toupper(Sys.getenv("R_ENV"))
  if (env %in% c("DEV", "UAT", "PRD", "LW7")) {
    return(env)
  }

  # Give error if environment not detected
  stop("Could not detect environment")
}


#################################
#################################
#################################

#' Obfuscation script
#'
#' Encode some plain text files to binary files to make them less searchable
#'
#' @param input_file file to be obfuscated
#' @param output_file file to obfuscate to
#'
#' @returns encoded file
#' @importFrom base64enc base64encode
#' @noRd
#'
obfuscate_text_file <- function(input_file = ".Renviron",
                                output_file = ".Renviron.enc"){
  if (!file.exists(input_file)) {
    stop("Input file not found: ", input_file, call. = FALSE)
  }

  # Read the entire file content into a single string
  renviron_content <- readChar(input_file, file.info(input_file)$size)

  # Encode the string using Base64
  encoded_content <- base64enc::base64encode(charToRaw(renviron_content))

  # Write the encoded content to the output file
  e <- try(writeLines(encoded_content, output_file))
  if (!inherits(e, "try-error")) {
    message("Successfully created encrypted file: ", output_file)
    message("You can now safely delete the plain-text '", input_file, "' from this location.")
  } else {
    warning("encoding failed")
  }
}

