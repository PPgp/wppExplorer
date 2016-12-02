
ui <- fluidPage(
	titlePanel("Explorer of Double Logistic Weights in Life Expectancy BHM"),
  sidebarLayout(
    sidebarPanel(
      numericInput("country", "UN country code:", 68),
      HTML("<p><small><a href='http://unstats.un.org/unsd/methods/m49/m49alpha.htm' target='_blank'>See UN country codes</a></small></p>"),
      sliderInput("weight", "Weights:", min = 0, max = 1, value = 0.5)
    ),
    mainPanel(plotOutput("DLPlot"))
  )
)


