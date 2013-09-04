utils::globalVariables(c("wpp.package", "data.env"))

wpp.explore <- function(wpp.year=2012) {
	allowed.wpps <- c(2008, 2010, 2012)
	if (!wpp.year %in% allowed.wpps)
		stop('wpp.year must be one of ', allowed.wpps)
	assign('wpp.package', paste('wpp', wpp.year, sep=''), envir = .GlobalEnv)
	shiny::runApp(system.file('explore', package='wppExplorer'))
}

wpp.indicator <- function(what, ...) {
	data <- do.call(what, list(...))
	if(is.null(data)) return(NULL)
	merge.with.un.and.melt(data)
}

wpp.by.year <- function(data, year) {
  data <- data[data$Year == year,]
  data$Year <- NULL
  data
}

wpp.by.country <- function(data, country) {
  data <- data[data$charcode == country,]
  data$charcode <- NULL
  data
}

wpp.by.countries <- function(data, countries) {
  data <- data[data$charcode %in% countries,]
  data
}

tpop <- function(...) {
	# Create a dataset of total population
	if.not.exists.load('popM')
	if.not.exists.load('popF')
	tpop <- sumMFbycountry(data.env$popM, data.env$popF)
	if(wpp.package == 'wpp2012') { #projection stored separately from observations
		if.not.exists.load('popMprojMed')
		if.not.exists.load('popFprojMed')
		tpopp <- sumMFbycountry(data.env$popMprojMed, data.env$popFprojMed)
		tpop <- merge(tpop, tpopp, by='country_code')
	}
	tpop
}

tpopF <- function(...) return(tpop.sex('F'))
tpopM <- function(...) return(tpop.sex('M'))

tpop.sex <- function(sex) {
	# Create a dataset of total population
	dataset <- paste('pop', sex, sep='')
	pop <- load.dataset.and.sum.by.country(dataset)
	if(wpp.package == 'wpp2012') { #projection stored separately from observations
		dataset <- paste('pop', sex, 'projMed', sep='')
		popp <- load.dataset.and.sum.by.country(dataset)
		pop <- merge(pop, popp, by='country_code')
	}
	pop
}

mig <- function(...) {
	# Create a dataset of net migration
	if.not.exists.load('migrationM')
	if.not.exists.load('migrationF')
	sumMFbycountry(data.env$migrationM, data.env$migrationF)
}

popagesex <- function(sexm, agem, ...){
	age <- agem
	sex <- sexm
	if(is.null(age)) age <- '0-4'
	if(is.null(sex)) sex <- 'F'
	if(length(sex)==0 || length(age)==0) return(NULL)
	tpop <- tpopp <- NULL			
	for(s in sex) {
		dataset.name <- paste('pop',s, sep='')
		if.not.exists.load(dataset.name)
		pop <- sum.by.country.subset.age(data.env[[dataset.name]], age)
		if(!is.null(tpop)){
			tpop <- cbind(country_code=tpop[,'country_code'], tpop[,2:ncol(tpop)] + pop[,2:ncol(pop)])
		} else tpop<-pop
		if(wpp.package == 'wpp2012') { #projection stored separately from observations
			dataset.name <- paste('pop', s, 'projMed', sep='')
			if.not.exists.load(dataset.name)
			popp <- sum.by.country.subset.age(data.env[[dataset.name]], age)
			if(!is.null(tpopp)){
				tpopp <- cbind(country_code=tpopp[,'country_code'], tpopp[,2:ncol(tpopp)] + popp[,2:ncol(popp)])
			} else tpopp<-popp
		}
	}
	if(!is.null(tpopp)) tpop <- merge(tpop, tpopp, by='country_code')
	tpop
}

mortagesex <- function(sex, age, ...){
	if(is.null(age)) age <- '0'
	if(is.null(sex)) sex <- 'F'
	dataset.name <- paste('mx',sex, sep='')
	if.not.exists.load(dataset.name)
	sum.by.country.subset.age(data.env[[dataset.name]], age)
}

fertage <- function(age, ...){
	if(is.null(age)) age <- '15-19'
	if.not.exists.load('percentASFR')
	tfert <- fert()
	tfert <- cbind(country_code=tfert$country_code, tfert[,.get.year.cols.idx(tfert)])
	asfr <- sum.by.country.subset.age(data.env[['percentASFR']], age)
	tfert <- tfert[tfert$country_code %in% asfr$country_code,]
	o <- order(asfr$country_code)
	#browser()
	cbind(country_code=tfert[o,'country_code'], tfert[o,2:ncol(tfert)] * asfr[o,2:ncol(asfr)] / 100.)
}

fert <- function(...) {
	name.pred <- if(wpp.package=='wpp2008') NULL else 'tfrprojMed'
	return(load.and.merge.datasets('tfr', name.pred))
}

leF <- function(...) {
	name.pred <- if(wpp.package=='wpp2008') NULL else 'e0Fproj'
	return(load.and.merge.datasets('e0F', name.pred))
}

leM <- function(...) {
	name.pred <- if(wpp.package=='wpp2008') NULL else 'e0Mproj'
	return(load.and.merge.datasets('e0M', name.pred))
}

fert.ci <- function(ci, ...) {
	# ci is 'low' or 'high'
	if(wpp.package=='wpp2008') return(NULL)
	load.and.merge.datasets(paste('tfrproj', capitalize(ci), sep=''), NULL)
}

popagesex.ci <- function(ci, sexm, agem, ...) {
	# ci is 'low' or 'high'
	if((wpp.package != 'wpp2012') || (length(sexm) > 1) || (length(agem) > 1)) return(NULL)
	#browser()
	dataset.name <- paste('pop', sexm, 'proj', capitalize(ci), sep='')
	if.not.exists.load(dataset.name)
	sum.by.country.subset.age(data.env[[dataset.name]], agem)
}

load.dataset.and.sum.by.country<-function(dataset){
	if.not.exists.load(dataset)
	pop <- sum.by.country(data.env[[dataset]])
}

if.not.exists.load <- function(name) {
	if(!exists(name, where=data.env, inherits=FALSE))
		do.call('data', list(name, package=wpp.package, envir=data.env))
}
load.and.merge.datasets <- function(name.obs, name.pred=NULL){
	do.call('data', list(name.obs, package=wpp.package, envir=data.env))
  	data <- data.env[[name.obs]]
  	if(!is.null(name.pred)){
  		# load predictions
  		do.call('data', list(name.pred, package=wpp.package, envir=data.env))
  		data.pred <- data.env[[name.pred]]
  		data <- merge(data, data.pred, by='country_code')
  	}
	data
}

lookupByIndicator <- function(indicator, sex.mult=c(), sex=c(), age.mult=c(), age=c()) {
	indicator <- as.numeric(indicator)
	fun <- attr(data.env$indicators, 'fun')[indicator]
	#print(c('ind:', age))
	# load observed data
	#browser()
	if(!is.null(data.env[[fun]])) return(data.env[[fun]])
	data <- wpp.indicator(fun, sexm=sex.mult, sex=sex, agem=age.mult, age=age)
	if(!attr(data.env$indicators, 'by.age')[indicator])
		data.env[[fun]] <- data
	data
}


getUncertainty <- function(indicator, what='low', sex.mult=c(), sex=c(), age.mult=c(), age=c()) {
	indicator <- as.numeric(indicator)
	if(!attr(data.env$indicators, 'low.high')[indicator]) return(NULL)
	fun <- paste(attr(data.env$indicators, 'fun')[indicator], 'ci', sep='.')
	lookup.name <- paste(fun, what, sep='.')
	if(!is.null(data.env[[lookup.name]])) return(data.env[[lookup.name]])
	data <- wpp.indicator(fun, ci=what, sexm=sex.mult, sex=sex, agem=age.mult, age=age)
	if(!attr(data.env$indicators, 'by.age')[indicator])
  		data.env[[lookup.name]] <- data
	data
}

.get.year.col.names <- function(col.names) {
	col.names <- gsub('.y', '', col.names, fixed=TRUE)
	l <- nchar(col.names)
	substr(col.names, l-3, l)
}

.get.year.cols.idx <- function(data, remove.duplicate.columns=TRUE) {
	year.cols.idx <- grep('[0-9]{4}$|[0-9]{4}.y$', colnames(data))
	# if(remove.duplicate.columns) {
  		# dupl.year <- duplicated(.get.year.col.names(colnames(data)[year.cols.idx]), fromLast=TRUE)
 		# if(any(dupl.year)) year.cols.idx <- year.cols.idx[-which(dupl.year)]
 	# }
 	year.cols.idx
}

merge.with.un.and.melt <- function(data) {
	year.cols.idx <- .get.year.cols.idx(data)
  	year.cols <- colnames(data)[year.cols.idx]
	data <- merge(data.env$iso3166[,c('uncode', 'name', 'charcode')], data, by.x='uncode', by.y='country_code')
  	data <- data[,-which(colnames(data)=='uncode')] 
  	data <- melt(data,
               id.vars = 'charcode', 
               measure.vars = year.cols,
               variable.name = 'Year',
               na.rm=TRUE)
	data$Year <- as.numeric(.get.year.col.names(as.character(data$Year)))
	data	
}

sum.by.country <- function(dataset) {
	year.cols.idx <- grep('^[0-9]{4}', colnames(dataset))
	ddply(dataset[,c(which(colnames(dataset)=='country_code'), year.cols.idx)], .(country_code), .fun=colwise(sum))
}

sumMFbycountry <- function(datasetM, datasetF) {
	tpopM <- sum.by.country(datasetM)
	tpopF <- sum.by.country(datasetF)
	cbind(country_code=tpopM[,'country_code'], tpopM[,2:ncol(tpopM)] + tpopF[,2:ncol(tpopF)])
}

sum.by.country.subset.age <- function(dataset, ages) {
	#browser()
	sum.by.country(with(dataset, dataset[gsub("^\\s+|\\s+$", "", age) %in% ages,]))
}



preserveStructure <- function(dataFrame) {
  structure(
    lapply(names(dataFrame), function(name) {I(dataFrame[[name]])}),
    names=names(dataFrame)
  )
}

get.indicator.choices <- function() {
	ind.names <- c('Total Fertility Rate', 'Female Life Expectancy', 'Male Life Expectancy', 
					'Total Population', 'Female Population', 'Male Population', 'Net migration', 
					'Population by sex and age', 'Mortality rate by sex and age', 'Age-specific Fertility')
	funcs <- c('fert', 'leF', 'leM', 'tpop', 'tpopF', 'tpopM', 'mig', 'popagesex', 'mortagesex', 'fertage')
	structure(
		as.character(1:length(ind.names)),
		names = ind.names,
		fun = funcs,
		by.age = c(rep(FALSE, 7), rep(TRUE, 3)),
		no.sum = c(rep(FALSE, 8), rep(TRUE, 2)),
		sum.in.table = c(rep(FALSE, 3), rep(TRUE, 5), rep(FALSE,2)),
		low.high = c(TRUE, rep(FALSE, 6), TRUE, rep(FALSE, 2))
	)
}
