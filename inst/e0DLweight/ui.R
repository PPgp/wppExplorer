
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("country", "Country code:", 68),
      sliderInput("weight", "Weights:", min = 0, max = 1, value = 0.5)
    ),
    mainPanel(plotOutput("DLPlot"))
  )
)


