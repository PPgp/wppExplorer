context('Indicators')

test_that('fertility age profile has the right dimension', {
	prof <- wppExplorer:::get.age.profile.fert(2000, c('FR', 'AF'))
	expect_true(all(dim(prof) == c(2*7, 4))) # 2 countries by 7 age groups
	prof <- wppExplorer:::get.age.profile.fert(2020, 'GE')
	expect_true(all(dim(prof) == c(7, 4)))
	expect_true(all(prof$charcode == 'GE'))
})

set.year <- function(wpp.year) {
	if(!get.wpp.year()==wpp.year)
		set.wpp.year(wpp.year)
}

test_that('mean age of women in childbearing age is put into the right format', {
	set.year(2012)	
	data <- wpp.by.country(wpp.indicator('meanageinchbearage'), 'US')
	expect_true(all(dim(data) == c(31, 2))) # 1 country for all years
	expect_true(setequal(data$Year, seq(1950, 2100, by=5)))
})

test_that('mortality values for high ages come out correctly', {
	set.year(2015)
	mx <- wpp.by.year(wpp.by.country(wpp.indicator('mortagesex', sex="M", age="100+"), 'FR'), 2015)$value
	expect_true(mx > 0.51)
	mx <- wpp.by.year(wpp.by.country(wpp.indicator('mortagesex', sex="M", age="100+"), 'FI'), 2015)$value
	expect_true(mx > 0.5 & mx < 0.51)
	mx2 <- wpp.by.year(wpp.by.country(wpp.indicator('mortagesex', sex="M", age="100"), 'FI'), 2015)$value
	expect_true(all.equal(mx, mx2))
	mx <- wpp.by.year(wpp.by.country(wpp.indicator('mortagesex', sex="F", age="110"), 'FI'), 2015)$value
	expect_true(mx > 1.2)
})
