context('Indicators')

test_that('fertility age profile has the right dimension', {
	prof <- wppExplorer:::get.age.profile.fert(2000, c('FR', 'AF'))
	expect_true(all(dim(prof) == c(2*7, 4))) # 2 countries by 7 age groups
	prof <- wppExplorer:::get.age.profile.fert(2020, 'GE')
	expect_true(all(dim(prof) == c(7, 4)))
	expect_true(all(prof$charcode == 'GE'))
})

test_that('mean age of women in childbearing age is put into the right format', {
	wpp.year <- 2012
	if(!get.wpp.year()==wpp.year)
		set.wpp.year(wpp.year)
	data <- wpp.by.country(wpp.indicator('meanageinchbearage'), 'US')
	expect_true(all(dim(data) == c(31, 2))) # 1 country for all years
	expect_true(setequal(data$Year, seq(1950, 2100, by=5)))
})