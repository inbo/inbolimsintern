library(DBI)
library(tidyverse)
library(inbolimsintern)
library(openxlsx)


conn <- limsdb_connect(uid = "LimsAppDbo", pwd = "7b51ea28b4")

result_tbl <- tbl(conn, "result")
sample_tbl <- tbl(conn, "sample")

data <- result_tbl |>
  inner_join(sample_tbl,
             by = "SAMPLE_NUMBER",
             suffix = c("_res", "_samp")
  ) |>
  filter(
    ANALYSIS %in% c(
      "ICP_CEC_BACL2_V", "ICP_CEC_COHEX_V", "ICP_MET_OPT8300_W",
      "ICP_MET_OXAL", "ICP_MINMET_OPT8300_W", "ICP_MINMET_HNO3_12",
      "P_TOT_L_ICP_W"
    ),
    is.na(SAMPLE_TYPE),
    ENTERED_ON > "2018-01-01",
    STATUS_res == "A",
    STATUS_samp == "A"
  ) |>
  select(ANALYSIS, NAME, ENTERED_ON, ENTRY, VALUE = NUMERIC_ENTRY) |>
  collect() |>
  filter(!is.na(VALUE))


data_tab <- data |>
  group_by(ANALYSIS, NAME) |>
  summarize(
    n = n(),
    avg = mean(VALUE, na.rm = TRUE),
    # This turns the named vector from quantile() into a single row of columns
    quantile(VALUE, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = TRUE) |>
      as_tibble_row(),
    .groups = "drop"
  )

#########################

# 2. Create Styles
header_style <- createStyle(
  fontColour = "#ffffff", fgFill = "#2c3e50",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "TopBottomLeftRight"
)

body_style <- createStyle(halign = "right", border = "TopBottomLeftRight")

# Decimal style for the numeric columns (avg and quantiles)
num_format <- createStyle(numFmt = "0.000")



########################

wb <- createWorkbook()
unique_analyses <- unique(data_tab$ANALYSIS)

#########################

for (anly in unique_analyses) {
  # Add a worksheet named after the analysis (truncated to 31 chars for Excel limits)
  sheet_name <- substr(anly, 1, 31)
  addWorksheet(wb, sheet_name)

  # Filter data for this specific tab
  tab_summary <- data_tab %>% filter(ANALYSIS == anly)
  tab_raw <- data %>% filter(ANALYSIS == anly)

  # Write the summary table to the Excel sheet
  writeData(wb, sheet_name, tab_summary, startCol = 1, startRow = 1)

  # Apply Formatting
  # - Body borders
  addStyle(wb, sheet_name, body_style,
           rows = 2:(nrow(tab_summary) + 1), cols = 1:ncol(tab_summary),
           gridExpand = TRUE
  )

  # - Numeric formatting for columns 4 to end (avg + quantiles)
  addStyle(wb, sheet_name, num_format,
           rows = 2:(nrow(tab_summary) + 1), cols = 4:ncol(tab_summary),
           gridExpand = TRUE
  )

  # - Column Widths
  setColWidths(wb, sheet_name, cols = 1:ncol(tab_summary), widths = "auto")

  # - Freeze Pane (keeps headers visible while scrolling)
  freezePane(wb, sheet_name, firstRow = TRUE)


  # 4. Generate Histograms for this analysis
  # We create a faceted plot containing all NAMEs in this analysis
  p <- ggplot(tab_raw, aes(x = VALUE)) +
    geom_histogram(fill = "#34495e", color = "white", bins = 30) +
    scale_x_log10() +
    facet_wrap(~NAME, scales = "free") +
    theme_minimal() +
    labs(title = paste("Log-Distribution for Analysis:", anly), x = "Value", y = "Frequency")

  temp_plot <- tempfile(fileext = ".png")
  ggsave(temp_plot, plot = p, width = 10, height = 7, units = "in")

  insertImage(wb, sheet_name, temp_plot,
              startCol = 1, startRow = nrow(tab_summary) + 3,
              width = 10, height = 7, units = "in"
  )
}

# 5. Save the Workbook
saveWorkbook(wb, "Lab_Analysis_ResultsICP_7_methods.xlsx", overwrite = TRUE)

cat("Excel file created successfully with separate tabs and histograms.")
