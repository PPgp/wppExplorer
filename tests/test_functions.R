start.test <- function(name) cat('\n<=== Starting test of', name,'====\n')
test.ok <- function(name) cat('\n==== Test of', name, 'OK.===>\n')

test.fert.ageprofile <- function(){
	test.name <- 'Fertility age profile'
    start.test(test.name)
	prof <- wppExplorer:::get.age.profile.fert(2000, c('FR', 'AF'))
	stopifnot(all(dim(prof) == c(2*7, 4)))
	prof <- wppExplorer:::get.age.profile.fert(2020, 'GE')
	stopifnot(all(dim(prof) == c(7, 4)))
	stopifnot(all(prof$charcode == 'GE'))
	test.ok(test.name)
}

test.wpp.indicators <- function(wpp.year=2012) {
	test.name <- 'WPP indicators'
    start.test(test.name)
    if(!get.wpp.year()==wpp.year)
		set.wpp.year(wpp.year)
	data <- wpp.by.country(wpp.indicator('meanagechbear'), 'US')
	stopifnot(all(dim(data) == c(31, 2)))
	stopifnot(all(is.element(c(1955, 2000, 2020, 2100), data$Year)))
	test.ok(test.name)
}