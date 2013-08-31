
# if (file.exists('data/HNP.RData')) {
  # load('data/HNP.RData')
# } else {
   local({
    # Country <- read.csv('data/HNP_Country.csv', fileEncoding='latin1')
    # Country <<- data.frame(CountryCode=Country$CountryCode, Long.Name=Country$Long.Name)
    # Series <<- read.csv('data/HNP_Series.csv', fileEncoding='latin1')
    # Data <<- read.csv('data/HNP_Data.csv', fileEncoding='latin1')
    
      yearCols <<- sapply(seq(1955, 2010, by=5), function(x) as.character(x))
    # for (year in yearCols) {
      # Data[[year]] <<- Data[[paste0('X', year)]]
      # Data[[paste0('X', year)]] <<- NULL
    # }
   })
  # save(file='data/HNP.RData', Country, Series, Data, yearCols)
  	data.env <<- new.env()
  	data('iso3166', envir=data.env)
  	data.env$datasets.obs <- c('tfr', 'e0F', 'e0M', 'popF', 'popM')
	data.env$datasets.pred <- c('tfrprojMed', 'e0Fproj', 'e0Mproj', NA, NA)
	data.env$datasets.low <- c('tfrprojLow', rep(NA, 4))
	data.env$datasets.high <- c('tfrprojHigh', rep(NA, 4))
	data.env$indicator.names <- c('Total Fertility Rate', 'Female Life Expectancy', 'Male Life Expectancy', 'Female Population', 'Male Population')
 #}


indicatorChoices <- structure(
   as.character(1:5),
   names = data.env$indicator.names
 )


lookupByIndicator <- function(indicator) {
	indicator <- as.numeric(indicator)
	# load observed data
	if(!is.null(data.env[[data.env$datasets.obs[indicator]]])) return(data.env[[data.env$datasets.obs[indicator]]])
	do.call('data', list(data.env$datasets.obs[indicator], package=wpp.package, envir=data.env))
  	data <- data.env[[data.env$datasets.obs[indicator]]]
  	if(!is.na(data.env$datasets.obs[indicator])){
  		year.cols.idx <- grep('^[0-9]{4}', colnames(data))
  		remove <- which(as.integer(substr(colnames(data), 1, 4)[year.cols.idx]) > 2005)
  		if(length(remove) > 1) data <- data[,-year.cols.idx[remove]]
  		# load predictions
  		do.call('data', list(data.env$datasets.pred[indicator], package=wpp.package, envir=data.env))
  		data.pred <- data.env[[data.env$datasets.pred[indicator]]]
  		data <- merge(data, data.pred, by='country_code')
  	}
  	year.cols.idx <- grep('^[0-9]{4}', colnames(data))
  	dupl.year <- duplicated(substr(colnames(data)[year.cols.idx], 1,4), fromLast=TRUE)
 	if(any(dupl.year)) year.cols.idx <- year.cols.idx[-which(dupl.year)]
  	year.cols <- colnames(data)[year.cols.idx]
	data <- merge(data.env$iso3166[,c('uncode', 'name', 'charcode')], data, by.x='uncode', by.y='country_code')
  	data <- data[,-which(colnames(data)=='uncode')] 
  	data <- melt(data,
               id.vars = 'charcode', 
               measure.vars = year.cols,
               variable.name = 'Year',
               na.rm=TRUE)
  data$Year <- as.numeric(substr(as.character(data$Year),1,4))
  data.env[[data.env$datasets.obs[indicator]]] <- data
  data
}

getUncertainty <- function(indicator, what='low') {
	indicator <- as.numeric(indicator)
	datasetname <- paste0('datasets.', what)
	if(is.na(data.env[[datasetname]][indicator])) return(NULL)
	if(!is.null(data.env[[data.env[[datasetname]][indicator]]])) return(data.env[[data.env[[datasetname]][indicator]]])
	do.call('data', list(data.env[[datasetname]][indicator], package=wpp.package, envir=data.env))
  	data <- data.env[[data.env[[datasetname]][indicator]]]
  	year.cols.idx <- grep('^[0-9]{4}', colnames(data))
  	dupl.year <- duplicated(substr(colnames(data)[year.cols.idx], 1,4), fromLast=TRUE)
 	if(any(dupl.year)) year.cols.idx <- year.cols.idx[-which(dupl.year)]
  	year.cols <- colnames(data)[year.cols.idx]
	data <- merge(data.env$iso3166[,c('uncode', 'name', 'charcode')], data, by.x='uncode', by.y='country_code')
  	data <- data[,-which(colnames(data)=='uncode')]
  	data <- melt(data,
               id.vars = 'charcode',
               measure.vars = year.cols,
               variable.name = 'Year',
               na.rm=TRUE)
  data$Year <- as.numeric(substr(as.character(data$Year),1,4))
  data.env[[data.env[[datasetname]][indicator]]] <- data
  data
}


filterByYear <- function(data, year) {
  data <- data[data$Year == year,]
  data$Year <- NULL
  data
}

filterByCountry <- function(data, country) {
  data <- data[data$charcode == country,]
  data$charcode <- NULL
  data
}

filterByMultipleCountryNames <- function(data, countries) {
  data <- data[data$charcode %in% countries,]
  data
}

preserveStructure <- function(dataFrame) {
  structure(
    lapply(names(dataFrame), function(name) {I(dataFrame[[name]])}),
    names=names(dataFrame)
  )
}
