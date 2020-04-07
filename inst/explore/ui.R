library(shinythemes)

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

googleHistogram <- function(id, options=list()) {
  tags$div(id=id, class="google-histogram-output", `data-options`=RJSONIO::toJSON(options))
}

 
shinyUI(
	fluidPage(theme = shinytheme("yeti"),
 	titlePanel(paste("WPP", wppExplorer:::get.wpp.year(), "Explorer"),
 	           title = HTML(paste("<h2>WPP", wppExplorer:::get.wpp.year(), 
 	                              "Explorer</h2><h5>Exploratory interface to the UN's World Population Projections</h5>")
 	                        )
 	           ),
    sidebarLayout(
  	sidebarPanel(
    	shinyjs::useShinyjs(),
    	geochartPrereqs,
    	tags$head(
         		#tags$style(type="text/css", ".jslider { max-width: 50px; }"),
         		#tags$style(type='text/css', ".well { padding: 0px; margin-bottom: 5px; max-width: 100px; }"),
				tags$style(type='text/css', ".span4 { max-width: 270px; }")
		),
    	uiOutput('yearUI'),
    	hr(),
    	selectInput('indicator', h5('Indicator:'), wppExplorer:::wpp.data.env$indicators),
    	conditionalPanel(condition=paste("input.indicator >", sum(attr(wppExplorer:::wpp.data.env$indicators, "settings")$by.age == FALSE)),
    		tags$head(tags$style(type="text/css", "#selagesmult { height: 150px; width: 85px}"),
    			  tags$style(type="text/css", "#selages { width: 85px}"),
    			  tags$style(type="text/css", "#indsexmult { height: 55px; width: 95px}"),
    			  tags$style(type="text/css", "#indsex { width: 95px}")
    			  ),
    		fluidRow(
				column(4, offset=2, uiOutput('sexselection')),
    			column(3, offset=1, uiOutput('ageselection'))
    		)
    	),
    	htmlOutput('indicatorDesc'),
    	hr(),
    	selectInput('uncertainty', h5('Uncertainty:'), structure(as.character(1:3), names=c('80%', '95%', '+-1/2child')), 
    			multiple=TRUE, selected=1),
    	textOutput('uncertaintyNote'),
    	#shinythemes::themeSelector(),
    	hr(),
    	HTML("<p><small><b>Data Source:</b> United Nations, Department of Economic and Social Affairs, Population Division: <a href='http://population.un.org/wpp' target='_blank'>World Population Prospects 2019</a>. Made available under a <a href='http://creativecommons.org/licenses/by/3.0/igo'>Creative Commons license CC BY 3.0 IGO</a>.</small></p>"),
		HTML("<p><small><b>Methodology:</b> <a href='https://www.un.org/development/desa'  target='_blank'>UN DESA</a> and <a href='http://bayespop.csss.washington.edu' target='_blank'>BayesPop research group</a> at University of Washington supported by <a href='https://www.nichd.nih.gov' target='_blank'>NICHD</a> (<a href='https://www.stat.washington.edu/raftery' target='_blank'>Adrian Raftery</a>, PI).</small></p>"),
		HTML("<p><small><b>User Interface:</b> Hana &#352;ev&#269;&#237;kov&#225;, <a href='http://bayespop.csss.washington.edu' target='_blank'>BayesPop research group</a>, <a href='https://www.csss.washington.edu' target='_blank'>CSSS</a>, University of Washington.</small></p>"),
		width=3
	),
  	mainPanel(
    	shinyjs::useShinyjs(),
    	tabsetPanel(
      		tabPanel('Map',
      	 		fluidRow(
      	 			column(6, checkboxInput('normalizeMapAndCountryPlot', 'Fixed scale over time', TRUE))
      	 		),
      	 		fluidRow(
      	 			column(6, offset=5, textOutput('mapyear'))	
				),		
				hr(),
				geochart('map'),
				#htmlOutput('mapgvis'),
				hr(),
				conditionalPanel(condition='input.map_selection',
					plotOutput('countryPlot', height='300px'))
      		),
      	tabPanel('Sortable Data', 
      		fluidRow(
      			column(6, checkboxInput('includeAggr2', 'Include Aggregations', FALSE))
      		),
      		fluidRow(
      	 		column(3, offset=5, textOutput('year2')),
      	 		column(1, offset=2, downloadLink("download", "Download", class = "fa fa-download alignright"))
      	 	),
			hr(),
      		DT::dataTableOutput('stable')
      	),
      	tabPanel('Trends & Pyramids',
  			tags$head(
				tags$style(type="text/css", "#seltcountries { height: 450px}"),
				tags$style(type="text/css", "#trendstable { overflow-x: scroll}")
			),
			#tags$div(
			fluidPage(
				#class = "container",
				fluidRow(HTML("<br>")),
				fluidRow(
					column(3, uiOutput('cselection')),
				  	column(9, 
				  		tabsetPanel(
				  			tabPanel('Trends', 
				  				plotOutput('probtrends', #height="400px", width="650px", 
				  							click = "probtrends_values", hover = "probtrends_values", 
				  							dblclick = "probtrends_zoom_reset", 
				  							brush = brushOpts(id = "probtrends_zoom", resetOnNew = TRUE)),
				  				flowLayout(
									checkboxInput('trend.logscale', 'Log scale', FALSE),
									textOutput("probtrends_selected")
								)
				  			),
				  			tabPanel('Age Profile', 
				  				googleLineChart('age.profileM', options=list(height=200)),
				  				googleLineChart('age.profileF', options=list(height=200)),
				  				checkboxInput('aprofile.logscale', 'Log scale', FALSE)),
				  			tabPanel('Pyramids', 
				  				plotOutput('pyramids', click = "pyramid_values", hover = "pyramid_values", 
				  						dblclick = "pyramid_zoom_reset",
				  						brush = brushOpts(id = "pyramid_zoom", resetOnNew = TRUE)
								),
				  				flowLayout(
				  					checkboxInput('proppyramids', 'Pyramid of proportions', FALSE),
				  					textOutput("pyramid_selected")
								)
				  			), 
				  			type="pills"	  			
				  		)
				  	)
 				),
 				fluidRow(
 					column(12,
 							textOutput('trendstabletitle'),
 						   	tableOutput('trendstable')
 					)
 				)
 			)
 		), # end "Trends & Pyramids tab
 		tabPanel('Histogram',
 			flowLayout(
      			checkboxInput('fiXscaleHist', 'Fixed x-axis over time', FALSE)
      		),
      		hr(),
      		htmlOutput('ghist')
    	),
      	tabPanel('Rosling Chart',
			htmlOutput('graphgvis'),
			HTML("<br/>"),
			fluidRow(column(1, "")),
			fluidRow(
				column(3, offset=1, textOutput('AddIndicatorText')),
				column(1, actionButton("AddIndicator", "Add indicator"))
			),
			HTML("<br/><i><small>*If you don't see a graph above, make sure Adobe Flash Player is installed and enabled in your browser.</small></i>")
      	),
      	tabPanel("Help",
      		includeHTML("README.html")
      	)
  	) #end tabsetPanel
  ) #end mainPanel
)))