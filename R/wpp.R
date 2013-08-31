utils::globalVariables("wpp.package")

show.maps <- function(wpp.year=2012) {
	allowed.wpps <- c(2008, 2010, 2012)
	if (!wpp.year %in% allowed.wpps)
		stop('wpp.year must be one of ', allowed.wpps)
	wpp.package <<- paste('wpp', wpp.year, sep='')
	shiny::runApp(system.file('maps', package='wppExplorer'))
}
