context('Setting WPP year')

test_that('WPP year changes correctly', {
	set.wpp.year(2008)
	data <- wpp.indicator('leF') # contains only observed data
	expect_true(setequal(unique(data$Year), seq(1955, 2010, by=5)))
	expect_equal(length(unique(data$charcode)), 194) # 194 countries
	
	set.wpp.year(2012)
	expect_false('leF' %in% ls(wppExplorer:::wpp.data.env)) # leF should be deleted from the environment
	data <- wpp.indicator('leF') # contains also predictions and aggregations
	expect_true(setequal(unique(data$Year), seq(1955, 2100, by=5)))
	expect_equal(length(unique(data$charcode)), 235) 
})