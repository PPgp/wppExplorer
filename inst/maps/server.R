library(ggplot2)

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
  	data <- merge(data(), data.env$iso3166[,c('charcode', 'name')], by='charcode')
  	data <- cbind(data[,c('charcode', 'name', 'value')], rank=rank(data$value))
  	colnames(data)[1] <- 'code'
  	gvisTable(data, options=list(width=600, height=600, page='disable', pageSize=198))
  	})
  	
  output$hist <- renderPlot({
    hist(data()$value, breaks=20, xlim=rangeForAllYears())
    print(qplot(data()$value, binwidth=diff(rangeForAllYears())/20, xlim=rangeForAllYears(), ylim=c(0,10), xlab='value'))
  })
  
  output$trends <- reactive({
    data <- indicatorData()
    hrange <- range(data$Year)
    vrange <- range(data$value)
    #data <- data[data$Year <= input$year,]

    casted <- dcast(data, Year ~ charcode)

    list(data = preserveStructure(casted),
         options = list(
           hAxis = list(viewWindowMode = 'explicit', viewWindow = list(
             min = hrange[1], max = hrange[2]
           ), format="####"),
           vAxis = list(viewWindowMode = 'explicit', viewWindow = list(
             min = vrange[1], max = vrange[2]
           ))
         )
    )
  })
})