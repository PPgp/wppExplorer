geochartPrereqs <- tagList(
  tags$head(
    tags$script(src="https://www.google.com/jsapi"),
    tags$script(src="geochart.js")
  )
)

geochart <- function(id, options=list()) {
  tags$div(id=id, class="shiny-geochart-output", `data-options`=RJSONIO::toJSON(options))
}

googleLineChart <- function(id, options=list()) {
  tags$div(id=id, class="google-linechart-output", `data-options`=RJSONIO::toJSON(options))
}

row <- function(...) {
	tags$div(class="row", ...)
}

col <- function(width, ...) {
	tags$div(class=paste0("span", width), ...)
}

 
shinyUI(pageWithSidebar(
  headerPanel(paste("WPP", substr(wppExplorer:::wpp.data.env$package, 4, 8), "Explorer")),
  sidebarPanel(
    geochartPrereqs,
    uiOutput('yearUI'),
    selectInput('indicator', 'Indicator:', wppExplorer:::wpp.data.env$indicators),
    textOutput('indicatorDesc'),
    conditionalPanel(condition="input.indicator > 13",
    	tags$head(tags$style(type="text/css", "#selagesmult { height: 150px; width: 70px}"),
    			  tags$style(type="text/css", "#selages { height:25px; width: 70px}"),
    			  tags$style(type="text/css", "#indsexmult { height: 50px; width: 90px}"),
    			  tags$style(type="text/css", "#indsex { height: 25px; width: 90px}")),
    	row(
			col(1, ''),
			col(4, uiOutput('sexselection')),
    		col(3, uiOutput('ageselection'))
    	)
    ),
    selectInput('uncertainty', 'Uncertainty:', structure(as.character(1:3), names=c('80%', '95%', '+-1/2child')))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('Map',
		textOutput('mapyear'),
		geochart('map'),
		conditionalPanel(condition='input.map_selection',
				checkboxInput('normalizeCountryPlot', 'Fixed scale', FALSE),
				plotOutput('countryPlot', height='300px'))
      ),
      tabPanel('Data', 
		textOutput('year1'),
      	tableOutput('table')
      ),
      tabPanel('Sortable Data', 
		textOutput('year2'),
      	tableOutput('stable')
      ),
      tabPanel('Trends & Pyramids',
  		tags$head(
			tags$style(type="text/css", "#seltcountries { height: 400px; width: 150px}")
			),
			tags$div(
				class = "container",
				row(
					col(0.5, ''),
					col(2, uiOutput('cselection')),
				  	col(7, tabsetPanel(
				  				tabPanel('Median',
				  					googleLineChart('trends', options=list(height=400))),
				  				tabPanel('Probabilistic trends', plotOutput('probtrends')),
				  				tabPanel('Pyramids', plotOutput('pyramids'))
				  			)
				  		)
 					),
 				row(
 					col(0.5, ''),
 					col(9, tableOutput('trendstable'))
 					)
 				)
 		),
 	tabPanel('Histogram',
 		textOutput('year3'),
      	checkboxInput('fiXscaleHist', 'Fixed x-axis over time', TRUE),
      	plotOutput('hist')
    ) #end tabPanel
  ) #end tabsetPanel
  ) #end mainPanel
))