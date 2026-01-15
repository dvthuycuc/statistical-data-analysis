topic_overview_ui <- function(id = "overview") {
  tagList(
    h3("Overview"),
    tags$ul(
      tags$li("Objective of dashboard"),
      tags$li("Data structure"),
      tags$li("Data overview"),
      tags$li("Data example")
    ),
    tags$h4("Example formula"),
    withMathJax(
      helpText("Mean: \\( \\bar{x} = \\frac{1}{n}\\sum_{i=1}^n x_i \\)"),
      helpText("Variance: \\[ s^2 = \\frac{1}{n-1}\\sum_{i=1}^n (x_i-\\bar{x})^2 \\]")
    )
  )
}

topic_descriptive_statistic_ui <- function(id = "overview") {
  tagList(
    h3("Descriptive Statistic", style = "text-align: center;"),
    h4("Objective:"),
    tags$ul(
      tags$li("Measures of Central Tendency"),
      tags$li("Measures of Variability or Dispersion"),
      tags$li("Quantiles and Percentiles"),
      tags$li("Visualization"),
      tags$li("Visualing Groups")
    ),
  )
}