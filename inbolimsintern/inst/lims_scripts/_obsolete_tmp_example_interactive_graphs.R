library(ggplot2)
library(plotly)
library(DT)
library(htmltools)
library(htmlwidgets)

# Create widgets
plotly1 <- ggplotly(ggplot(mtcars, aes(wt, mpg)) + geom_point())
plotly2 <- ggplotly(ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point())
table1 <- datatable(mtcars)
table2 <- datatable(iris)

# Create placeholder widget — empty widget as shell
placeholder <- htmlwidgets::createWidget("html", list(), package = "htmlwidgets")

# Build full layout
layout <- tagList(
  tags$div(
    tags$head(tags$title("Self-contained Report")),
    tags$body(
      tags$div(
        style = "position: fixed; top: 60px; left: 0; width: 200px; padding: 10px; background-color: #f9f9f9; border-right: 1px solid #ccc; height: 100%; overflow-y: auto;",
        h3("Table of Contents"),
        tags$ul(
          tags$li(tags$a(href = "#introduction", "Introduction")),
          tags$li(tags$a(href = "#mtcars", "mtcars")),
          tags$li(tags$a(href = "#iris", "iris")),
          tags$li(tags$a(href = "#conclusion", "Conclusion"))
        )
      ),
      tags$div(
        style = "margin-left: 220px; padding: 20px;",
        h1("Data Analysis Report"),
        tags$h2(id = "introduction", "Introduction"),
        p("This is a self-contained HTML report created from R via Rscript."),

        tags$h2(id = "mtcars", "mtcars Plot"),
        plotly1,
        table1,

        tags$h2(id = "iris", "iris Plot"),
        plotly2,
        table2,

        tags$h2(id = "conclusion", "Conclusion"),
        p("This concludes the report.")
      )
    )
  )
)

# Combine layout and widget into one object that saveWidget will accept
output <- htmlwidgets::prependContent(placeholder, layout)

# Save as self-contained
htmlwidgets::saveWidget(output, "output.html", selfcontained = TRUE)
