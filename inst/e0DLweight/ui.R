
ui <- fluidPage(
	titlePanel("Explorer of Double Logistic Weights in Life Expectancy BHM"),
  sidebarLayout(
    sidebarPanel(
      numericInput("country", "UN country code:", 68),
      sliderInput("weight", "Weights:", min = 0, max = 1, value = 0.5)
    ),
    mainPanel(plotOutput("DLPlot"))
  )
)


