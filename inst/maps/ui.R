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
  headerPanel(paste("WPP", substr(wpp.package, 4, 8), "Explorer")),
  sidebarPanel(
    geochartPrereqs,
    uiOutput('yearUI'),
    selectInput('indicator', 'Indicator', indicatorChoices),
    textOutput('indicatorDesc')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('Map',
               geochart('map'),
               textOutput('mapyear'),
               conditionalPanel(condition='input.map_selection',
                                checkboxInput('normalizeCountryPlot', 'Fixed scale', FALSE)),
               plotOutput('countryPlot', height='300px')
      ),
      tabPanel('Data', tableOutput('table')),
      tabPanel('Sortable Data', tableOutput('stable')),
      tabPanel('Histogram', plotOutput('hist')),
       tabPanel('Trends',
  		tags$head(
       		tags$style(type="text/css", "#seltcountries { height: 400px; width: 150px}")
			),
			tags$div(
    			class = "container",
				row(
					col(0.5, ''),
			      col(2,
						uiOutput('cselection')
						#selectInput('seltcountries', 'Select countries:', sort(data.env$iso3166[,'name'])=sort(data.env$iso3166[,'charcode']), multiple=TRUE)
						),
				  col(7, 
 						googleLineChart('trends', options=list(height=500))
 						)
 					)
 					,
 				row(
 					col(0.5, ''),
 					col(9, tableOutput('trendstable'))
 					)
 				)
    )
  ))
))