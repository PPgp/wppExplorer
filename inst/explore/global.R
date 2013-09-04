local({
  	data.env <<- new.env()
  	data('iso3166', envir=data.env)
  	data.env$indicators <- wppExplorer:::get.indicator.choices()
})