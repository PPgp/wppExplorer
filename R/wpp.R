utils::globalVariables("wpp.data.env")

wpp.explore <- function(wpp.year=NULL) {
	if(!is.null(wpp.year)) set.wpp.year(wpp.year)
	shiny::runApp(system.file('explore', package='wppExplorer'))
}

get.available.wpps <- function() c(2008, 2010, 2012)
check.wpp.revision <- function(wpp.year) {
	if (!wpp.year %in% get.available.wpps())
		stop('wpp.year must be one of ', get.available.wpps())
}

wpp.indicator <- function(what, ...) {
	data <- do.call(what, list(...))
	if(is.null(data)) return(NULL)
	merge.with.un.and.melt(data, what=what)
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

set.wpp.year <- function(wpp.year) {
	check.wpp.revision(wpp.year)
	wpp.data.env$package <- paste('wpp', wpp.year, sep='')
	cat('\nDefault WPP package set to', wpp.data.env$package,'.\n')
}

get.wpp.year <- function() as.integer(substr(wpp.data.env$package, 4,8))
 
tpop <- function(...) {
	# Create a dataset of total population
	if.not.exists.load('popM')
	if.not.exists.load('popF')
	tpop <- sumMFbycountry(wpp.data.env$popM, wpp.data.env$popF)
	if(wpp.data.env$package == 'wpp2012') { #projection stored separately from observations
		if.not.exists.load('popMprojMed')
		if.not.exists.load('popFprojMed')
		tpopp <- sumMFbycountry(wpp.data.env$popMprojMed, wpp.data.env$popFprojMed)
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
	if(wpp.data.env$package == 'wpp2012') { #projection stored separately from observations
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
	sumMFbycountry(wpp.data.env$migrationM, wpp.data.env$migrationF)
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
		pop <- sum.by.country.subset.age(wpp.data.env[[dataset.name]], age)
		if(!is.null(tpop)){
			tpop <- cbind(country_code=tpop[,'country_code'], tpop[,2:ncol(tpop)] + pop[,2:ncol(pop)])
		} else tpop<-pop
		if(wpp.data.env$package == 'wpp2012') { #projection stored separately from observations
			dataset.name <- paste('pop', s, 'projMed', sep='')
			if.not.exists.load(dataset.name)
			popp <- sum.by.country.subset.age(wpp.data.env[[dataset.name]], age)
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
	sum.by.country.subset.age(wpp.data.env[[dataset.name]], age)
}

fertage <- function(age, ...){
	if(is.null(age)) age <- '15-19'
	if.not.exists.load('percentASFR')
	tfert <- fert()
	tfert <- cbind(country_code=tfert$country_code, tfert[,.get.year.cols.idx(tfert)])
	asfr <- sum.by.country.subset.age(wpp.data.env[['percentASFR']], age)
	tfert <- tfert[tfert$country_code %in% asfr$country_code,]
	o <- order(asfr$country_code)
	#browser()
	cbind(country_code=tfert[o,'country_code'], tfert[o,2:ncol(tfert)] * asfr[o,2:ncol(asfr)] / 100.)
}

fert <- function(...) {
	name.pred <- if(wpp.data.env$package=='wpp2008') NULL else 'tfrprojMed'
	return(load.and.merge.datasets('tfr', name.pred))
}

leF <- function(...) {
	name.pred <- if(wpp.data.env$package=='wpp2008') NULL else 'e0Fproj'
	return(load.and.merge.datasets('e0F', name.pred))
}

leM <- function(...) {
	name.pred <- if(wpp.data.env$package=='wpp2008') NULL else 'e0Mproj'
	return(load.and.merge.datasets('e0M', name.pred))
}

sexratio <- function(...) {
	return(load.and.merge.datasets('sexRatio', NULL))
}

.sum.popFM.keep.age <- function() {
	name.preds <- if(wpp.data.env$package!='wpp2012') c(NULL, NULL) else c('popFprojMed', 'popMprojMed')
	pF <- load.and.merge.datasets('popF', name.preds[1], by=c('country_code', 'age'), remove.cols=c('country', 'name'))
	pM <- load.and.merge.datasets('popM', name.preds[2], by=c('country_code', 'age'), remove.cols=c('country', 'name'))
	cbind(country_code=pF[,1], pF[,-c(1,2)] + pM[,-c(1,2)])
}

medage <- function(...) {
	ddply(.sum.popFM.keep.age(), "country_code", .fun=colwise(gmedian))
}

tdratio <- function(...) {
	ddply(.sum.popFM.keep.age(), "country_code", .fun=colwise(dependency.ratio, which='total'))
}

psratio <- function(...) {
	ddply(.sum.popFM.keep.age(), "country_code", .fun=colwise(function(x) 1/dependency.ratio(x, which='old')))
}

chdratio <- function(...) {
	ddply(.sum.popFM.keep.age(), "country_code", .fun=colwise(dependency.ratio, which='child'))
}

oadratio <- function(...) {
	ddply(.sum.popFM.keep.age(), "country_code", .fun=colwise(dependency.ratio, which='old'))
}

.pi.suffix <- function(x) c(low='l', high='u')[x]

fert.ci <- function(which.pi, bound, ...) {
	# which.pi is for '80', '95' or 'half.child'
	# bound is 'low' or 'high'
	if(wpp.data.env$package=='wpp2008') return(NULL)
	if(wpp.data.env$package=='wpp2010' && which.pi != 'half.child') return(NULL)
	dataset.name <- if(which.pi == 'half.child') paste('tfrproj', capitalize(bound), sep='')
					else paste('tfrproj', which.pi, .pi.suffix(bound), sep='')
	load.and.merge.datasets(dataset.name, NULL)
}

leF.ci <- function(which.pi, bound, ...) {
	e0.ci('F', which.pi, bound)
}

leM.ci <- function(which.pi, bound, ...) {
	e0.ci('M', which.pi, bound)
}

e0.ci <- function(sex, which.pi, bound) {
	if(wpp.data.env$package!='wpp2012' || which.pi == 'half.child') return(NULL)
	load.and.merge.datasets(paste('e0', sex, 'proj', which.pi, .pi.suffix(bound), sep=''), NULL)
}

tpop.ci <- function(which.pi, bound, ...) {
	# which.pi is for '80', '95' or 'half.child'
	# bound is 'low' or 'high'
	if(wpp.data.env$package!='wpp2012' || which.pi != 'half.child') return(NULL)
	load.and.merge.datasets(paste('popproj', capitalize(bound), sep=''), NULL)
}

popagesex.ci <- function(which.pi, bound, sexm, agem, ...) {
	# bound is 'low' or 'high'
	if((wpp.data.env$package != 'wpp2012') || (length(sexm) > 1) || (length(agem) > 1) || (which.pi != 'half.child')) 
		return(NULL)
	dataset.name <- paste('pop', sexm, 'proj', capitalize(bound), sep='')
	if.not.exists.load(dataset.name)
	sum.by.country.subset.age(wpp.data.env[[dataset.name]], agem)
}

load.dataset.and.sum.by.country<-function(dataset){
	if.not.exists.load(dataset)
	pop <- sum.by.country(wpp.data.env[[dataset]])
}

if.not.exists.load <- function(name) {
	if(!exists(name, where=wpp.data.env, inherits=FALSE))
		do.call('data', list(name, package=wpp.data.env$package, envir=wpp.data.env))
}
load.and.merge.datasets <- function(name.obs, name.pred=NULL, by='country_code', remove.cols=c('country', 'name')){
	if.not.exists.load(name.obs)
  	data <- wpp.data.env[[name.obs]]
  	if(length(remove.cols) > 0) data <- data[,-which(colnames(data)%in%remove.cols)]
  	if(!is.null(name.pred)){
  		# load predictions
  		if.not.exists.load(name.pred)
  		data.pred <- wpp.data.env[[name.pred]]
  		if(length(remove.cols) > 0) data.pred <- data.pred[,-which(colnames(data.pred)%in%remove.cols)]
  		data <- merge(data, data.pred, by=by, sort=FALSE)
  	}
	data
}

lookupByIndicator <- function(indicator, sex.mult=c(), sex=c(), age.mult=c(), age=c()) {
	indicator <- as.numeric(indicator)
	fun <- ind.fun(indicator)
	# load observed data
	#browser()
	if(!is.null(wpp.data.env[[fun]])) return(wpp.data.env[[fun]])
	data <- wpp.indicator(fun, sexm=sex.mult, sex=sex, agem=age.mult, age=age)
	if(!ind.is.by.age(indicator))
		wpp.data.env[[fun]] <- data
	data
}

.get.pi.name <- function(x) c('80', '95', 'half.child')[x]

getUncertainty <- function(indicator, which.pi, bound='low', sex.mult=c(), sex=c(), age.mult=c(), age=c()) {
	indicator <- as.numeric(indicator)
	if(!ind.is.low.high(indicator)) return(NULL)
	fun <- paste(ind.fun(indicator), 'ci', sep='.')
	pi.name <-.get.pi.name(as.integer(which.pi))
	lookup.name <- paste(fun, pi.name, bound, sep='.')
	if(!is.null(wpp.data.env[[lookup.name]])) return(wpp.data.env[[lookup.name]])
	data <- wpp.indicator(fun, pi.name, bound=bound, sexm=sex.mult, sex=sex, agem=age.mult, age=age)
	if(!ind.is.by.age(indicator))
  		wpp.data.env[[lookup.name]] <- data
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

merge.with.un.and.melt <- function(data, id.vars='charcode', what=NULL) {
	year.cols.idx <- .get.year.cols.idx(data)
  	year.cols <- colnames(data)[year.cols.idx]
	data <- merge(wpp.data.env$iso3166[,c('uncode', 'name', 'charcode')], data, 
					by.x='uncode', by.y='country_code', sort=FALSE)
  	data <- data[,-which(colnames(data)=='uncode')] 
  	data <- melt(data,
               id.vars = id.vars, 
               measure.vars = year.cols,
               variable.name = 'Year',
               na.rm=TRUE)
	data$Year <- as.numeric(.get.year.col.names(as.character(data$Year)))
	#if(!is.null(what) && ind.mid.years(what))
	#	data$Year <- data$Year - 2
	#browser()
	data	
}

sum.by.country <- function(dataset) {
	year.cols.idx <- grep('^[0-9]{4}', colnames(dataset))
	ddply(dataset[,c(which(colnames(dataset)=='country_code'), year.cols.idx)], "country_code", .fun=colwise(sum))
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

ind.settings <- function() attr(wpp.data.env$indicators, 'settings')
ind.fun <- function(indicator) rownames(ind.settings())[indicator]
ind.is.by.age <- function(indicator) ind.settings()[indicator, 'by.age']
ind.is.low.high <- function(indicator) ind.settings()[indicator, 'low.high']
ind.no.age.sum <- function(indicator) ind.settings()[indicator, 'no.age.sum']
ind.sum.in.table <- function(indicator) ind.settings()[indicator, 'sum.in.table']
ind.mid.years <- function(indicator) ind.settings()[indicator, 'mid.years']

set.data.env <- function(name, value) wpp.data.env[[name]] <- value

gmedian <- function(f, cats=NULL, age) {
	# group median
	if(is.null(cats)) cats <- seq(0, by=5, length=length(f)+1)
	nhalf <- sum(f)/2.
	cumsumf <- cumsum(f)
	medcat <- findInterval(nhalf, cumsumf) + 1
	med <- cats[medcat] + ((nhalf-cumsumf[medcat-1])/f[medcat])*(cats[medcat+1]-cats[medcat])
	return(med)
}

dependency.ratio <- function(counts, which='total'){
	nom <- 0
	if(which %in% c('total', 'child')) nom <- nom + sum(counts[1:3])
	if(which %in% c('total', 'old')) nom <- nom + sum(counts[14:21])
	nom/sum(counts[4:13])	
}

get.pyramid.data <- function(year, countries, which.pi=NULL, bound=NULL) {
	name.preds <- name.obs <- c(NULL, NULL)
	if(is.null(which.pi)) {
		name.obs <- c('popF', 'popM')
		if(wpp.data.env$package=='wpp2012') name.preds <- c('popFprojMed', 'popMprojMed')
	} else { #PIs
		if(wpp.data.env$package=='wpp2012' && .get.pi.name(as.integer(which.pi)) == 'half.child') 
			name.obs <- paste('pop', c('F','M'), 'proj', capitalize(bound), sep='')
	}
	if(all(is.null(c(name.preds, name.obs)))) return(NULL)
	pF <- load.and.merge.datasets(name.obs[1], name.preds[1], by=c('country_code', 'age'), remove.cols=c('country', 'name'))
	pM <- load.and.merge.datasets(name.obs[2], name.preds[2], by=c('country_code', 'age'), remove.cols=c('country', 'name'))
	dataF <- merge.with.un.and.melt(cbind(pF, age.num=rep(1:21, nrow(pF)/21)), id.vars=c('charcode', 'age', 'age.num'),
				what="popF")
	dataF <- cbind(dataF, sex='F')
	dataM <- merge.with.un.and.melt(cbind(pM, age.num=rep(1:21, nrow(pM)/21)), id.vars=c('charcode', 'age', 'age.num'),
				what="popM")
	dataM <- cbind(dataM, sex='M')
	data <- wpp.by.year(rbind(dataF, dataM), year)
	#browser()
	wpp.by.countries(data, countries)
}