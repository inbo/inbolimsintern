#' Save HTML Widget Report with Pandoc Detection
#'
#' This function saves an HTML widget to a file, automatically detecting whether
#' Pandoc is available and choosing the appropriate saving method. If Pandoc is
#' found and functional, it creates a self-contained HTML file. Otherwise, it
#' falls back to creating a non-self-contained HTML file with external dependencies.
#'
#' @param widget An HTML widget object to be saved (e.g., plotly, DT, leaflet)
#' @param filename Character string specifying the output filename.
#'   Default is "output.html"
#' @param libdir Character string specifying the directory name for external
#'   library files when creating non-self-contained HTML. Default is "output_files"
#'
#' @return NULL (invisible). The function is called for its side effects of
#'   creating HTML files and printing status messages.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Checks for Pandoc availability using \code{rmarkdown::find_pandoc()}
#'         and \code{Sys.which("pandoc")}
#'   \item If Pandoc is found, attempts to create a self-contained HTML file
#'         using \code{htmlwidgets::saveWidget()} with \code{selfcontained = TRUE}
#'   \item If Pandoc is not found or fails to run, falls back to creating a
#'         non-self-contained HTML file with external dependencies stored in
#'         the specified \code{libdir}
#'   \item Provides informative messages about the chosen method and any
#'         additional steps required for sharing the files
#' }
#'
#' Self-contained HTML files include all CSS and JavaScript dependencies inline,
#' making them easier to share as a single file. Non-self-contained files require
#' the accompanying library directory to function properly.
#'
#' @note
#' When creating non-self-contained HTML files, ensure that the \code{libdir}
#' folder is included when sharing or deploying the HTML file, as it contains
#' necessary CSS and JavaScript dependencies.
#'
#' @examples
#' \dontrun{
#' # Save a plotly widget as self-contained HTML (if Pandoc available)
#' library(plotly)
#' p <- plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", mode = "markers")
#' save_widgets_report(p, "my_plot.html")
#'
#' # Save with custom library directory
#' save_widgets_report(p, "report.html", libdir = "assets")
#'
#' # Save a DT datatable
#' library(DT)
#' dt <- datatable(iris)
#' save_widgets_report(dt, "data_table.html")
#' }
#'
#' @seealso
#' \code{\link[htmlwidgets]{saveWidget}}, \code{\link[rmarkdown]{find_pandoc}}
#'
#' @export
save_widgets_report <- function(widget, filename = "output.html", libdir = "output_files") {
  # Try to find pandoc
  pandoc_found <- !is.null(rmarkdown::find_pandoc(dir = NULL)) &&
    nzchar(Sys.which("pandoc"))

  if (pandoc_found) {
    message("Pandoc found — generating self-contained HTML.")
    pandoc_ran <- try(htmlwidgets::saveWidget(widget, file = filename, selfcontained = TRUE))
    if (inherits(pandoc_ran, "try-error")){
      message("Pandoc found but not runnable — falling back to non-self-contained HTML.")
      htmlwidgets::saveWidget(widget, file = filename, selfcontained = FALSE, libdir = libdir)
      message("Please make sure to include the `", libdir, "` folder when sharing the HTML file.")
    }
  } else {
    message("Pandoc not found — falling back to non-self-contained HTML.")
    htmlwidgets::saveWidget(widget, file = filename, selfcontained = FALSE, libdir = libdir)
    message("Please make sure to include the `", libdir, "` folder when sharing the HTML file.")
  }
}
