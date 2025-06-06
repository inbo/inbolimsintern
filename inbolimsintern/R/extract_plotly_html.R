

# Function to get self-contained plotly HTML out of the html generated by plotly
#'
#' @param p plotly object
#'
#' @return extracted html of plotly interactive graphic
#' @export
#'
extract_plotly_html <- function(p) {

  # Create a temporary file
  temp_file <- tempfile(fileext = ".html")

  # Save as self-contained
  htmlwidgets::saveWidget(
    partial_bundle(p),
    file = temp_file,
    selfcontained = TRUE
  )

  # Read the entire file
  full_html <- readLines(temp_file)

  # Extract dependencies from head
  head_start <- which(grepl("<head>", full_html)) + 1
  head_end <- which(grepl("</head>", full_html)) - 1
  deps <- full_html[head_start:head_end]

  # Extract body content
  body_start <- which(grepl('<body style="background-color: white;">', full_html)) + 1
  body_end <- which(grepl("</body>", full_html)) - 1
  body_content <- full_html[body_start:body_end]

  # Clean up
  unlink(temp_file)

  # Return both parts
  return(list(
    dependencies = paste(deps, collapse = "\n"),
    plot_content = paste(body_content, collapse = "\n")
  ))
}

