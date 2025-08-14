#' Save HTML Widget with Fallback Options
#'
#' Attempts to save an HTML widget as a self-contained HTML file using pandoc.
#' If pandoc is not available or fails to run, falls back to creating a
#' non-self-contained HTML file with external dependencies in a separate folder.
#'
#' @param widget An HTML widget object (e.g., from plotly, DT, leaflet, etc.)
#'   to be saved as an HTML file.
#' @param filename Character string specifying the output filename.
#'   Default is "output.html".
#' @param libdir Character string specifying the directory name for external
#'   dependencies when creating non-self-contained HTML. Default is "output_files".
#'   Only used when pandoc is unavailable or fails.
#'
#' @return Invisibly returns NULL. The function is called for its side effects
#'   of creating HTML files and printing status messages.
#' @importFrom htmlwidgets saveWidget
#' @details
#' The function first checks if pandoc is available and executable. If found,
#' it attempts to create a self-contained HTML file where all dependencies
#' are embedded. If pandoc is not available or the self-contained approach fails,
#' it creates a non-self-contained HTML file with dependencies stored in the
#' specified `libdir` folder.
#'
#' When using the non-self-contained approach, make sure to distribute both
#' the HTML file and the dependencies folder together.
#'
#' @examples
#' \dontrun{
#' # Create a simple plotly widget
#' library(plotly)
#' p <- plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", mode = "markers")
#'
#' # Save with default settings
#' save_report_widget(p)
#'
#' # Save with custom filename
#' save_report_widget(p, filename = "my_plot.html")
#'
#' # Save with custom filename and library directory
#' save_report_widget(p,
#'                   filename = "scatter_plot.html",
#'                   libdir = "plot_dependencies")
#'
#' # Example with DT datatable
#' library(DT)
#' dt <- datatable(iris)
#' save_report_widget(dt, filename = "iris_table.html")
#' }
#'
#' @seealso
#' \code{\link[htmlwidgets]{saveWidget}} for the underlying widget saving function
#' \code{\link[rmarkdown]{find_pandoc}} for pandoc detection
#'
#' @export
save_report_widget <- function(widget, filename = "output.html", libdir = "output_files") {
  # Try to find pandoc
  pandoc_found <- !is.null(rmarkdown::find_pandoc(dir = NULL)) &&
    nzchar(Sys.which("pandoc"))

  if (pandoc_found) {
    message("Pandoc found -> generating self-contained HTML.")
    pandoc_ran <- try(htmlwidgets::saveWidget(widget, file = filename, selfcontained = TRUE))
    if (inherits(pandoc_ran, "try-error")){
      message("Pandoc found but not runnable -> falling back to non-self-contained HTML.")
      htmlwidgets::saveWidget(widget, file = filename, selfcontained = FALSE, libdir = libdir)
      message("Please make sure to include the `", libdir, "` folder when sharing the HTML file.")
    }
  } else {
    message("Pandoc not found -> falling back to non-self-contained HTML.")
    htmlwidgets::saveWidget(widget, file = filename, selfcontained = FALSE, libdir = libdir)
    message("Please make sure to include the `", libdir, "` folder when sharing the HTML file.")
  }
}
