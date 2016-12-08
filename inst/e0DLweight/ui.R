
ui <- fluidPage(
	titlePanel("Explorer of Double Logistic Weights in Life Expectancy BHM"),
  sidebarLayout(
    sidebarPanel(
      #numericInput("country", "UN country code:", 68),
      uiOutput('cselection'),
      #numericInput("refcountry", "Reference country:", 600),
      uiOutput('cselection.ref'),
      #HTML("<p><small><a href='http://unstats.un.org/unsd/methods/m49/m49alpha.htm' target='_blank'>See UN country codes</a></small></p>"),
      sliderInput("weight", "Country weight:", min = 0, max = 1, value = 1)
    ),
    mainPanel(plotOutput("DLPlot"), 
              plotOutput("e0Plot"))
  )
)


