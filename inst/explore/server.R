library(reshape2)
library(googleVis)
library(plyr)
library(ggplot2)

shinyServer(function(input, output, session) {
  indicatorData <- reactive({
    wppExplorer:::lookupByIndicator(input$indicator, input$indsexmult, input$indsex, input$selagesmult, input$selages)
  })
  
	indicator.fun <- reactive({
		wppExplorer:::ind.fun(as.integer(input$indicator))
	})
	
   indicatorDataLow <- reactive({
    wppExplorer:::getUncertainty(input$indicator, input$uncertainty, 'low', input$indsexmult, input$indsex, input$selagesmult, input$selages)
  })
  
  indicatorDataHigh <- reactive({
    wppExplorer:::getUncertainty(input$indicator, input$uncertainty, 'high', input$indsexmult, input$indsex, input$selagesmult, input$selages)
  })
  
  data <- reactive({
    wpp.by.year(indicatorData(), input$year)
  })
  
  rangeForAllYears <- reactive({
    range(indicatorData()$value)
  })
  
  pyramid.data <- reactive({
  	wppExplorer:::get.pyramid.data(input$year, input$seltcountries)
  })
  
  pyramid.data.low <- reactive({
  	wppExplorer:::get.pyramid.data(input$year, input$seltcountries, input$uncertainty, bound='low')
  })
  
   pyramid.data.high <- reactive({
  	wppExplorer:::get.pyramid.data(input$year, input$seltcountries, input$uncertainty, bound='high')
  })
  
  age.profile.mortM <- reactive({
  	wppExplorer:::get.pyramid.data(input$year, input$seltcountries, indicators=c(M='mxM'), load.pred=FALSE)
  })
  
  age.profile.mortF <- reactive({
  	wppExplorer:::get.pyramid.data(input$year, input$seltcountries, indicators=c(F='mxF'), load.pred=FALSE)
  })
  
  age.profile.popM <- reactive({
  	wppExplorer:::get.pyramid.data(input$year, input$seltcountries, indicators=c(M='popM'))
  })
  
  age.profile.popF <- reactive({
  	wppExplorer:::get.pyramid.data(input$year, input$seltcountries, indicators=c(F='popF'))
  })
  
  age.profile.fert <- reactive({
  	wppExplorer:::get.age.profile.fert(input$year, input$seltcountries)
  })

  
  data.env <- function() wppExplorer:::wpp.data.env
    
  output$yearUI <- renderUI({
	data <- indicatorData()
  	if(nrow(data)==0) return(NULL)
  	animationOptions(interval = 1500)
    yearRange <- range(data$Year)
    value <- yearRange[1]
    #print(c('slider1: ', value, yearRange, data.env()$year.range, input$year))
    if(is.null(data.env()$year.range)) wppExplorer:::set.data.env('year.range', yearRange)
    else {
    	if(data.env()$year.range[1] != yearRange[1]) {
    		wppExplorer:::set.data.env('year.range', yearRange)
    		if(!is.null(input$year) && input$year >= yearRange[1]) value <- input$year
    	}
    }
    #print(c('slider2: ', value, yearRange, data.env()$year.range, input$year))
    sliderInput('year', 'Year', format="####", 
    			animate=TRUE,
                min=yearRange[1], max=yearRange[2], value = value, step=5)
  })
  
  output$indicatorDesc <- renderText({
  	#input$indicator
    #as.character(Series[Series$SeriesCode == input$indicator,]$Long.definition)
    ""
  })
  year.output <- reactive(paste('Year:', input$year))
  output$mapyear <- renderText(year.output())
  output$year1 <- renderText(year.output())
  output$year2 <- renderText(year.output())
  output$year3 <- renderText(year.output())
  
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
    title(main = paste(data.env()$iso3166$name[data.env()$iso3166$charcode==input$map_selection]))
    lines(df$Year, df$value, type='l')
    if(!is.null(low)) {
    	ipres <- which.max(df$Year[df$Year<min(low$Year)])
    	lines(c(df$Year[ipres], low$Year), c(df$value[ipres],low$value), lty=2)
    	lines(c(df$Year[ipres], high$Year),c(df$value[ipres], high$value), lty=2)
    }
    abline(v=input$year, col=3, lty=3)
  })

 output$table <- renderTable({
 	data <- merge(data(), data.env()$iso3166[,c('charcode', 'name')], by='charcode')#
	data[,c('charcode', 'name', 'value')]
 }, include.rownames = FALSE, include.colnames = FALSE)
  
  output$stable <- renderGvis({
  	#data <- indicatorData()
  	#year.data <- wpp.by.year(data, input$year)
  	year.data <- data()
  	if(nrow(year.data)==0) invalidateLater(1000, session)
  	year.data <- merge(year.data, data.env()$iso3166[,c('charcode', 'name')], by='charcode')
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
  	if (wppExplorer:::ind.no.age.sum(as.integer(input$indicator))) { # no multiple choices allowed
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
		if(wppExplorer:::ind.no.age.sum(as.integer(input$indicator))){
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
  	o <- order(data.env()$iso3166[,'name'])
  	codes <- as.character(data.env()$iso3166[o,'charcode'])
  	names <- paste(codes, data.env()$iso3166[o,'name'])
  	countries <- structure(
  		codes,
  		names = names
  	)
  	do.call('selectInput', list('seltcountries', 'Select countries/areas:', countries, multiple=TRUE, 
  					selected=names[1]))
	})
	
  cast.profile.data <- function(data) {
    vrange <- range(data$value)
    hrange <- if(is.element('15-19', data$age)) c(0, length(unique(data$age))) else range(data$age)
    #browser()
    data <- dcast(data, age.num + age ~ charcode, mean)
    data$age.num <- NULL
    list(casted=data, hrange=hrange, vrange=vrange)
  	
  }	
	
  filter.trend.data <- function(data, countries, cast=TRUE){
  	data <- wpp.by.countries(data, countries)
    if(is.null(data) || nrow(data) <= 0) return(NULL)
   	hrange <- range(data$Year)
    vrange <- range(data$value)
    if(cast)
    	data <- dcast(data, Year ~ charcode, mean)
    list(casted=data, hrange=hrange, vrange=vrange)
  }
  
  get.trends <- reactive({
  	if(is.null(input$seltcountries)) return(NULL)
  	filter.trend.data(indicatorData(), input$seltcountries)
  })
  

  get.age.profiles <- reactive({
  	if(is.null(input$seltcountries)) return(NULL)
  	filter.age.profiles(indicatorData(), input$seltcountries, input$year)
  })
  
  get.trends.nocast <- reactive({
  	if(is.null(input$seltcountries)) return(NULL)
  	filter.trend.data(indicatorData(), input$seltcountries, cast=FALSE)
  })
  
  get.trends.low <- reactive({
  	if(is.null(input$seltcountries)) return(NULL)
  	filter.trend.data(indicatorDataLow(), input$seltcountries, cast=FALSE)
  })
  
  get.trends.high <- reactive({
  	if(is.null(input$seltcountries)) return(NULL)
  	filter.trend.data(indicatorDataHigh(), input$seltcountries, cast=FALSE)
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
           ), logScale=input$median.logscale)
         )
    )
  })
  
  show.age.profile <- function(sex, fun, logscale=FALSE, year) {
  	if(fun=='mortagesex')
		data <- do.call(paste0('age.profile.mort', sex), list())
	else {
		if(fun %in% c('tpop', 'tpopF', 'tpopM', 'popagesex'))
			data <- do.call(paste0('age.profile.pop', sex), list())
		else {
			if(fun %in% c('fert', 'fertage') && sex=='F') {
				data <- age.profile.fert()
			} else {
  				return(list(data=data.frame(age=c(0,0), v=c(0,0)),
  						#data.frame(age=seq(0,100, by=5), value=rep(0, 21)), 
  						options=list(title=paste(c(F='Female', M='Male')[sex], ': No age profiles for this indicator.'),
  							legend= list(position="none"),
  							hAxis = list(viewWindow = list(min=-1, max=1)),
  							vAxis = list(viewWindow = list(min=-1, max=1))
  						)))
			}
		}
	}
	if(is.null(data)) return(NULL)
	data <- cast.profile.data(data)
	#browser()
	#print(data)
    list(data = wppExplorer:::preserveStructure(data$casted),
         options = list(
           hAxis = list(slantedText=fun!='mortagesex',
           				viewWindowMode = 'explicit', viewWindow = list(
           				min = data$hrange[1], max = data$hrange[2])),
           #), format="####"),
           vAxis = list(viewWindowMode = 'explicit', viewWindow = list(
             min = data$vrange[1], max = data$vrange[2]
           ), logScale=logscale),
           legend = list(position="right"),
           title = paste(year, c(F='Female', M='Male')[sex])
         )
    )
  }
  
  output$age.profileM <- reactive({
  	show.age.profile('M', indicator.fun(), input$aprofile.logscale, input$year)
  })
  output$age.profileF <- reactive({
	show.age.profile('F', indicator.fun(), input$aprofile.logscale, input$year)
  })
  
  output$probtrends <- renderPlot({
  	data <- get.trends.nocast()
  	if(is.null(data)) return(data)
  	data <- data$casted
  	low <- get.trends.low()
  	if(!is.null(low)) {
  		high <- get.trends.high()
  		low.high <- merge(low$casted, high$casted, by=c('charcode', 'Year'))
  		colnames(low.high)[3:4] <- c('low', 'high')
  		min.year <- min(low.high$Year)
  		data <- merge(data, low.high, by=c('charcode', 'Year'), all=TRUE)
  		idx <- which(data$Year == min.year-5)
  		data$low[idx] <- data$value[idx]
  		data$high[idx] <- data$value[idx]
  	}
  	g <- ggplot(data, aes(x=Year,y=value,colour=charcode)) + geom_line() + theme(legend.title=element_blank())
  	if(!is.null(low)) g <- g + geom_ribbon(aes(ymin=low, ymax=high, linetype=NA), alpha=0.3)
  	print(g)
  })
  
  .is.pyramid.indicator <- function() {
  	 if(!indicator.fun() %in% c('tpop', 'tpopF', 'tpopM', 'popagesex')) {
  		df <- data.frame(x=0, y=0, lab='No pyramid data for this indicator.')
  		g <- ggplot(df, aes(x=x, y=y, label=lab)) + geom_text() + scale_y_continuous(name='') + scale_x_continuous(name='')
  		print(g)
  		return(FALSE)
  	}
  	TRUE
  }
  .get.prop.data <- function(data, tpop) {
	tpop <- wppExplorer::wpp.by.countries(wppExplorer::wpp.by.year(tpop, input$year), input$seltcountries)
  	colnames(tpop)[2] <- 'tpop' 
  	data <- merge(data, tpop, by='charcode')  		
  	data <- ddply(data, 'charcode', mutate, value = value/tpop)
  	data$tpop <- NULL
  	data
  }
  .get.pyramid.data <- function(proportion=FALSE) {
  	data <- pyramid.data()
  	if(proportion) {
  		tpop <- wppExplorer::wpp.indicator('tpop')
  		data <- .get.prop.data(data, tpop)
  	}
	low <- pyramid.data.low()
  	if(!is.null(low) && nrow(low)>0) {
  		high <- pyramid.data.high()
  		if(proportion) {
			#browser()
			which.pi <- wppExplorer:::.get.pi.name(as.integer(input$uncertainty))
  			tpop <- wppExplorer::wpp.indicator('tpop.ci', which.pi=which.pi, bound='low')
  			low <- .get.prop.data(low, tpop)
  			lowval <- low$value
  			tpop <- wppExplorer::wpp.indicator('tpop.ci', which.pi=which.pi, bound='high')
  			high <- .get.prop.data(high, tpop)
  			low$value <- pmin(low$value, high$value)
  			high$value <- pmax(high$value, lowval)
  		}
  		low.high <- merge(low, high, by=c('charcode', 'age', 'age.num', 'sex'), sort=FALSE)
  		colnames(low.high)[5:6] <- c('low', 'high')
  		data <- merge(data, low.high, all=TRUE, sort=FALSE)
	} else low <- NULL
  	data <- data[order(data$age.num),]
  	data
  }
  
  .print.pyramid <- function(data) {
  	data.range <- range(data$value)
  	g <- ggplot(data, aes(y=value, x=reorder(age, age.num), group=charcode, colour=charcode)) + geom_line(subset=.(sex=='F')) + geom_line(subset=.(sex=='M'), aes(y=-1*value)) + scale_x_discrete(name="") + scale_y_continuous(labels=function(x)abs(x)) + coord_flip() + ggtitle(input$year) + theme(legend.title=element_blank())
  	g <- g + geom_text(data=NULL, y=-data.range[2]/2, x=20, label="Male", colour='black')
  	g <- g + geom_text(data=NULL, y=data.range[2]/2, x=20, label="Female", colour='black')
  	g <- g + geom_hline(yintercept = 0)
  	#browser()
  	if(is.element('low', colnames(data))) {
  		g <- g + geom_ribbon(subset=.(sex=='F'), aes(ymin=low, ymax=high, linetype=NA), alpha=0.3)
  		g <- g + geom_ribbon(subset=.(sex=='M'), aes(ymin=-high, ymax=-low, linetype=NA), alpha=0.3)
  	}
  	print(g)
  }
  
  output$pyramids <- renderPlot({
  	if(!.is.pyramid.indicator()) return()
	data <- .get.pyramid.data(proportion=input$proppyramids)
	.print.pyramid(data)
  })
  
  
  # .get.digits <- reactive({
  	# print(wppExplorer:::ind.digits(as.integer(input$indicator)))
  	# wppExplorer:::ind.digits(as.integer(input$indicator))
  # })
  
  # format_num <- function(col, digits) {
  	# format <- paste0("%.", digits, 'f')
	# if (is.numeric(col)) sprintf(format, col)
	# else col
# }

  output$trendstable <- renderTable({
	data <- get.trends()
	if(is.null(data)) return(data)
	df <- as.data.frame(data$casted[,-1])
	if(ncol(df) > 1 && wppExplorer:::ind.sum.in.table(as.integer(input$indicator))) {
		df <- cbind(df, rowSums(df))
		colnames(df)[ncol(df)] <- 'Sum'
	} else colnames(df) <- input$seltcountries # one country selected
	df <- t(df)
	#df <- t(as.data.frame(lapply(df, format_num, digits=wppExplorer:::ind.digits(as.integer(input$indicator)))))
	# df <- t(data$casted[,-1]) # remove year column
	#browser()
	colnames(df) <- as.integer(data$casted[,'Year'])
	# if(nrow(df) > 1 && wppExplorer:::ind.sum.in.table(as.integer(input$indicator))) {
		# df <- rbind(df, colSums(df))
		# rownames(df)[nrow(df)] <- 'Sum'
	# }
	df
	}, include.rownames = TRUE)

  output$trendstabletitle <- renderText({
 	wppExplorer:::get.indicator.title(input$indicator, input$indsexmult, input$indsex, input$selagesmult, input$selages) 	
   })
})