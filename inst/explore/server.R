shinyServer(function(input, output, session) {
  indicatorData <- reactive({
  	#if(input$indicator>7) browser()
  	#print(c(input$indicator, input$indsex, input$selagesmult, input$selages))
    wppExplorer:::lookupByIndicator(input$indicator, input$indsexmult, input$indsex, input$selagesmult, input$selages)
  })
  
	indicator.fun <- reactive({
		attr(data.env$indicators, 'fun')[as.integer(input$indicator)]
	})
	
   indicatorDataLow <- reactive({
    wppExplorer:::getUncertainty(input$indicator, 'low', input$indsexmult, input$indsex, input$selagesmult, input$selages)
  })
  
  indicatorDataHigh <- reactive({
    wppExplorer:::getUncertainty(input$indicator, 'high', input$indsexmult, input$indsex, input$selagesmult, input$selages)
  })
  
  data <- reactive({
    wpp.by.year(indicatorData(), input$year)
  })
  
  rangeForAllYears <- reactive({
    range(indicatorData()$value)
  })
  
  output$yearUI <- renderUI({
	data <- indicatorData()
  	if(nrow(data)==0) return(NULL)
  	animationOptions(interval = 1500)
    yearRange <- range(data$Year)
    value <- yearRange[1]
    #print(c('slider1: ', value, yearRange, data.env$year.range, input$year))
    if(is.null(data.env$year.range)) data.env$year.range <- yearRange
    else {
    	if(data.env$year.range[1] != yearRange[1]) {
    		data.env$year.range <- yearRange
    		if(!is.null(input$year) && input$year >= yearRange[1]) value <- input$year
    	}
    }
    #print(c('slider2: ', value, yearRange, data.env$year.range, input$year))
    sliderInput('year', 'Year', format="0000", animate=TRUE,
                min=yearRange[1], max=yearRange[2], value = value, step=5)
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
    df <- wpp.by.country(data, input$map_selection)
    low <- wpp.by.country(data.l, input$map_selection)
    high <- wpp.by.country(data.h, input$map_selection)
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
  	#data <- indicatorData()
  	#year.data <- wpp.by.year(data, input$year)
  	year.data <- data()
  	if(nrow(year.data)==0) invalidateLater(1000, session)
  	year.data <- merge(year.data, data.env$iso3166[,c('charcode', 'name')], by='charcode')
  	low <- indicatorDataLow()
  	data <- cbind(year.data[,c('charcode', 'name', 'value')], rank=rank(year.data$value))
  	if(!is.null(low)) {
  		data.l <- wpp.by.year(low, input$year)
  		 if(nrow(data.l) > 0) {  		
    		data.h <- wpp.by.year(indicatorDataHigh(), input$year)
  			colnames(data.l)[colnames(data.l)=='value'] <- 'low'
  			data <- merge(data, data.l, by='charcode')
  			colnames(data.h)[colnames(data.h)=='value'] <- 'high'
  			data <- merge(data, data.h, by='charcode')
  		}
  	}
  	#print(c('sort table: ', input$year, dim(data), dim(year.data)))
  	#print(head(data))
  	#if(nrow(data)==0) browser()
  	colnames(data)[1] <- 'code'
  	gvisTable(data, options=list(width=600, height=600, page='disable', pageSize=198))
  	})
  	
  output$hist <- renderPlot({
  	data <- data()
  	if(is.null(data) || nrow(data)<=0) return(NULL)
  	data <- data$value
  	xlim <- if(input$fiXscaleHist) rangeForAllYears() else range(data)
  	binw <- diff(xlim)/20
    print(qplot(data()$value, binwidth=binw, xlim=c(xlim[1]-binw/2, xlim[2]+binw/2), xlab='value'))
  })
        
  output$ageselection <- renderUI({
  	if(indicator.fun()=='fertage'){
  		ages <- paste(seq(15, by=5, length=7), seq(19, by=5, length=7), sep='-')
  	} else {
  		if(indicator.fun()=='mortagesex')
  			ages <- c(0,1,seq(5, by=5, length=19))
  		else ages <- paste(seq(0, by=5, length=20), seq(4, by=5, length=20), sep='-')
  		ages <- c(ages, '100+')
  	}
  	if (attr(data.env$indicators, 'no.sum')[as.integer(input$indicator)]) { # no multiple choices allowed
  		multiple <- FALSE
		name <- 'selages'
		selected<-NULL
  	} else { 		
  		multiple <- TRUE
  		name <- 'selagesmult'
  		selected <- ages[1]
  	}
  	selectInput(name, 'Age:', ages, multiple=multiple, selected=selected)
	})
	
	output$sexselection <- renderUI({
		choices<-if(indicator.fun()=='fertage') c(Female="F") else c(Female="F", Male="M")
		if(attr(data.env$indicators, 'no.sum')[as.integer(input$indicator)]){
  			multiple <- FALSE
  			selected <- NULL
  			name <- 'indsex'
  		} else {
  			multiple <- TRUE
  			selected <- names(choices)
  			name <- 'indsexmult'
  		}
  		selectInput(name, 'Sex:', choices=choices, selected=selected, multiple=multiple)
	})
	
  output$cselection <- renderUI({
  	o <- order(data.env$iso3166[,'name'])
  	codes <- as.character(data.env$iso3166[o,'charcode'])
  	names <- paste(codes, data.env$iso3166[o,'name'])
  	countries <- structure(
  		codes,
  		names = names
  	)
  	do.call('selectInput', list('seltcountries', 'Select countries:', countries, multiple=TRUE, 
  					selected=names[1]))
	})
	
  get.trends <- reactive({
  	if(is.null(input$seltcountries)) return(NULL)
  	#data <- merge(indicatorData(), data.env$iso3166[,c('charcode', 'name')], by='charcode')
  	data <- indicatorData()
    data <- wpp.by.countries(data, input$seltcountries)
    if(nrow(data) <= 0) return(NULL)
   	hrange <- range(data$Year)
    vrange <- range(data$value)
    casted <- dcast(data, Year ~ charcode, mean)
    list(casted=casted, hrange=hrange, vrange=vrange)
  })
  
  output$trends <- reactive({
	data <- get.trends()
	if(is.null(data)) return(data)
    list(data = wppExplorer:::preserveStructure(data$casted),
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
	if(nrow(df) > 1 && attr(data.env$indicators, 'sum.in.table')[as.integer(input$indicator)]) {
		df <- rbind(df, colSums(df))
		rownames(df)[nrow(df)] <- 'Sum'
	}
	df
	}, include.rownames = TRUE)
	
})