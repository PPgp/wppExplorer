shinyServer(function(input, output, session) {
  indicatorData <- reactive({
    lookupByIndicator(input$indicator)
  })
  
   indicatorDataLow <- reactive({
    getUncertainty(input$indicator, 'low')
  })
  
  indicatorDataHigh <- reactive({
    getUncertainty(input$indicator, 'high')
  })
  
  data <- reactive({
    filterByYear(indicatorData(), input$year)
  })
  
  rangeForAllYears <- reactive({
    range(indicatorData()$value)
  })
  
  output$yearUI <- renderUI({
    yearRange <- range(indicatorData()$Year)
    sliderInput('year', 'Year', format="0000", animate=TRUE,
                min=yearRange[1], max=yearRange[2], value=yearRange[1], step=5)
  })
  
  output$indicatorDesc <- renderText({
  	#input$indicator
    #as.character(Series[Series$SeriesCode == input$indicator,]$Long.definition)
    ""
  })
  output$mapyear <- renderText(paste('Year:', input$year))
  output$map <- reactive({
    if (is.null(input$year))
      return(NULL)
    df <- data()
    if (nrow(df) == 0)
      return(NULL)
    list(data = df,
         options = list(
           colorAxis = list(
             minValue = min(indicatorData()$value),
             maxValue = max(indicatorData()$value)
           )
         )
    )
  })
  
  #col <- c('0x0000CC', '0x00CCFF', '0x33FF66', '0xFFFF66', '0xFF9900', '0xFF3300')
	#geo <- gvisGeoMap(data, locationvar="iso", numvar=what, hovervar=hovervar, 
	#			options=list(height=500, width=900, dataMode='regions',
	#			colors=paste('[', paste(col, collapse=', '), ']')))

  output$countryPlot <- renderPlot({
    if (is.null(input$map_selection))
      return(NULL)

	data <- indicatorData()
    data.l <- indicatorDataLow()
    data.h <- indicatorDataHigh()
    df <- filterByCountry(data, input$map_selection)
    low <- filterByCountry(data.l, input$map_selection)
    high <- filterByCountry(data.h, input$map_selection)
    ylim <- range(c(df$value, low$value, high$value))
    if (input$normalizeCountryPlot)
      ylim <- range(c(data$value, data.l$value, data.h$value)) 
    plot(df, type='n', ylim=ylim)
    title(main = paste(data.env$iso3166$name[data.env$iso3166$charcode==input$map_selection]))
    lines(df$Year, df$value, type='l')
    if(!is.null(low)) {
    	ipres <- which.max(df$Year[df$Year<min(low$Year)])
    	lines(c(df$Year[ipres], low$Year), c(df$value[ipres],low$value), lty=2)
    	lines(c(df$Year[ipres], high$Year),c(df$value[ipres], high$value), lty=2)
    }
    abline(v=input$year, col=3, lty=3)
  })

 output$table <- renderTable({
 	data <- merge(data(), data.env$iso3166[,c('charcode', 'name')], by='charcode')#
	data[,c('charcode', 'name', 'value')]
 }, include.rownames = FALSE, include.colnames = FALSE)
  
  output$stable <- renderGvis({
  	data <- indicatorData()
  	year.data <- filterByYear(data, input$year) 
  	year.data <- merge(year.data, data.env$iso3166[,c('charcode', 'name')], by='charcode')
  	low <- indicatorDataLow()
  	data <- cbind(year.data[,c('charcode', 'name', 'value')], rank=rank(year.data$value))
  	if(!is.null(low)) {
  		data.l <- filterByYear(low, input$year)
  		 if(nrow(data.l) > 0) {  		
    		data.h <- filterByYear(indicatorDataHigh(), input$year)
  			colnames(data.l)[colnames(data.l)=='value'] <- 'low'
  			data <- merge(data, data.l, by='charcode')
  			colnames(data.h)[colnames(data.h)=='value'] <- 'high'
  			data <- merge(data, data.h, by='charcode')
  		}
  	}
  	colnames(data)[1] <- 'code'
  	gvisTable(data, options=list(width=600, height=600, page='disable', pageSize=198))
  	})
  	
  output$hist <- renderPlot({
    hist(data()$value, breaks=20, xlim=rangeForAllYears())
    print(qplot(data()$value, binwidth=diff(rangeForAllYears())/20, xlim=rangeForAllYears(), ylim=c(0,10), xlab='value'))
  })
  
  output$cselection <- renderUI({
  	clist <- vector('list', nrow(data.env$iso3166))
  	o <- order(data.env$iso3166[,'name'])
  	codes <- as.character(data.env$iso3166[o,'charcode'])
  	names(clist) <- paste(codes, data.env$iso3166[o,'name'])
  	clist[] <- codes
  	do.call('selectInput', list('seltcountries', 'Select countries:', clist, multiple=TRUE, selected=names(clist)[1]))
	})
	
  get.trends <- reactive({
  	if(is.null(input$seltcountries)) return(NULL)
  	#data <- merge(indicatorData(), data.env$iso3166[,c('charcode', 'name')], by='charcode')
  	data <- indicatorData()
    data <- filterByMultipleCountryNames(data, input$seltcountries)
    if(nrow(data) <= 0) return(NULL)
   	hrange <- range(data$Year)
    vrange <- range(data$value)
    casted <- dcast(data, Year ~ charcode, mean)
    list(casted=casted, hrange=hrange, vrange=vrange)
  })
  
  output$trends <- reactive({
	data <- get.trends()
	if(is.null(data)) return(data)
    list(data = preserveStructure(data$casted),
         options = list(
           hAxis = list(viewWindowMode = 'explicit', viewWindow = list(
             min = data$hrange[1], max = data$hrange[2]
           ), format="####"),
           vAxis = list(viewWindowMode = 'explicit', viewWindow = list(
             min = data$vrange[1], max = data$vrange[2]
           ))
         )
    )
  })
  output$trendstable <- renderTable({
	data <- get.trends()
	if(is.null(data)) return(data)
	df <- t(data$casted[,-1]) # remove year column
	colnames(df) <- as.integer(data$casted[,'Year'])
	df
	}, include.rownames = TRUE)  
})