utils::globalVariables("wpp.data.env")
get.indicator.choices <- function() {
	ind.names <- c('Total Fertility Rate', 'Female Life Expectancy', 'Male Life Expectancy', 
					'Total Population', 'Female Population', 'Male Population', 'Net Migration', 
					'Sex Ratio at Birth', 'Median Age', 'Mean Age of Childbearing',
					'Total Dependancy Ratio', 'Child Dependency Ratio', 'Old-age Dependency Ratio','Potential Support Ratio',
					'Mean Annual Population Growth',
					'Population by sex and age', 'Mortality Rate by sex and age', 'Age-specific Fertility')
	funcs <- c('fert', 'leF', 'leM', 'tpop', 'tpopF', 'tpopM', 'mig', 
				'sexratio', 'medage', 'meanagechbear',
				'tdratio', 'chdratio', 'oadratio', 'psratio',
				'popgrowth',
				'popagesex', 'mortagesex', 'fertage')
	# if a new indicator is added, change also the condition in ui.R for displaying age-specific stuff
	l <- length(ind.names)
	ini <- rep(FALSE, l)
	ind.df <- data.frame(by.age=ini, no.age.sum=ini, sum.in.table=ini, low.high=ini, prob.ci=ini, mid.years=ini,
							digits=rep(2, l)) 
	rownames(ind.df) <- funcs
	ind.df[c('popagesex', 'mortagesex', 'fertage'), 'by.age'] <- TRUE              # display sex and age menu
	ind.df[c('mortagesex','fertage'), 'no.age.sum'] <- TRUE                        # don't allow multiple age- and sex-selection
	ind.df[c('tpop', 'tpopF', 'tpopM', 'mig','popagesex'), 'sum.in.table'] <- TRUE # show sum in the trend table
	ind.df[c('fert', 'leF', 'leM', 'tpop', 'popagesex'), 'low.high'] <- TRUE       # has uncertainty
	ind.df[c('fert', 'leF', 'leM', 'mig', 'sexratio', 'mortagesex', 'fertage'), 'mid.years'] <- TRUE # use mid years in slider (not implemented)
	ind.df[c('tpop', 'tpopF', 'tpopM','popagesex'), 'digits'] <- 0                 # number of digits in trend table (not implemented)
	ind.df['mortagesex', 'digits'] <- 4
	ind.df['fertage', 'digits'] <- 3
	
	structure(
		as.character(1:length(ind.names)),
		names = ind.names,
		settings = ind.df
	)
}


assign("wpp.data.env", new.env(), envir=parent.env(environment())
	#envir = .GlobalEnv
	)
data('iso3166', envir=wpp.data.env)
wpp.data.env$indicators <- get.indicator.choices()
wpp.data.env$package <- "wpp2012"
# Filter out non-used countries
do.call('data', list("popM", package=wpp.data.env$package, envir=wpp.data.env))
wpp.data.env$iso3166 <- wpp.data.env$iso3166[is.element(wpp.data.env$iso3166$uncode, wpp.data.env$popM$country_code),]
