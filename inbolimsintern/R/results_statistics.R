#' Make a csv and histograms about data statistics
#'
#' @param path directory to store file in
#' @param basename base file name to store csv and png 
#'
#' @returns a csv and png file
#' @export
#'
results_statistics <- function(path, basename) {
  require(dplyr)
  require(ggplot2)
  # Call the named function from plumber_logic_functions.R
  sqlfile = paste0(file.path(path, basename), ".sql")
  outdata = paste0(file.path(path, basename), ".csv")
  outfig =  paste0(file.path(path, basename), ".png")
  
  qry = paste(readLines(sqlfile), collapse = "\n")
  data <- DBI::dbGetQuery(global_conn, qry) |> 
    mutate(ENTRY = as.numeric(ENTRY))
  
  summary_df <- data %>%
    dplyr::mutate(ENTRY = as.numeric(ENTRY)) %>%
    dplyr::group_by(ANALYSIS, NAME) %>%
    dplyr::summarise(
      n       = sum(!is.na(ENTRY)),
      mean    = mean(ENTRY, na.rm = TRUE),
      sd      = sd(ENTRY, na.rm = TRUE),
      `0%`    = quantile(ENTRY, 0, na.rm = TRUE),
      `2.5%`  = quantile(ENTRY, 0.025, na.rm = TRUE),
      `25%`   = quantile(ENTRY, 0.25, na.rm = TRUE),
      `50%`   = quantile(ENTRY, 0.50, na.rm = TRUE),
      `75%`   = quantile(ENTRY, 0.75, na.rm = TRUE),
      `97.5%` = quantile(ENTRY, 0.975, na.rm = TRUE),
      `100%`  = quantile(ENTRY, 1, na.rm = TRUE),
      .groups = "drop"
    )
  
  readr::write_delim(summary_df, file = outdata, delim = "\t")
  
  ggplot(data, aes(x = ENTRY)) + geom_histogram() +
    facet_wrap(~NAME, scales = "free") +
    scale_x_log10()
  ggsave(filename = outfig)    
}