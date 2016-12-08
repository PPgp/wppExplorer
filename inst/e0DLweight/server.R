library(bayesLife)

shinyServer(function(input, output, session) {

  #sim.dir <- '/mnt/shinydata/apps/wppExplorer/e0simulation'
  sim.dir <- "~/talks/UN16/e0/converged/e0/sim03092016/"
  e0.pred <<- get.e0.prediction(sim.dir)
  DLx <<- seq(20, 90, length=100) 
  dl.world <- e0.world.dlcurves(DLx, e0.pred)
  me.world <<- apply(dl.world, 2, median)
  
  country.int <- reactive({as.integer(input$country)})
  refcountry.int <- reactive({as.integer(input$refcountry)})
  
  observe({
    if(is.null(input$country)) return(NULL)
    if(! country.int() %in% get.countries.table(e0.pred)$code) return(NULL)
    w <- weights()
    updateSliderInput(session, "weight", value=w)
  })
  
  plotDL <- function(country, refcountry){
    e0.DLcurve.plot(e0.pred, country=country.int(), nr.curves=10)
    refcobj <- get.country.object(refcountry, e0.pred$mcmc.set$meta)
    if(!is.null(refcobj$code)) {
      refdl <- apply(e0.country.dlcurves(DLx, e0.pred, country=refcobj$code), 2, median)
      lines(DLx, refdl, col='orange', lwd=2)
      legend("topleft", legend=c("country", "world", "weighted", refcobj$name), col=c("red", "blue", "black", "orange"), lwd=2, lty=1, bty='n')
    } else legend("topleft", legend=c("country", "world", "weighted"), col=c("red", "blue", "black"), lwd=2, lty=1, bty='n')
    lines(DLx, me.world, col='blue', lwd=2)
  }
  
  weights <- reactive({
    country <- country.int()
    t <- e0.trajectories.table(e0.pred, country)
    xct <- t[1:12,'median']
    yct <- diff(t[1:13,'median'])
    is.pos <- yct >= 0
    yct <- yct[is.pos]
    act <- apply(e0.country.dlcurves(xct, e0.pred, country=country), 2, median)[is.pos]
    bct <- apply(e0.world.dlcurves(xct, e0.pred), 2, median)[is.pos]
    w <- - sum((act-bct)^2)/sum((act-bct)*(bct-yct))
    if(w <= 0 || w > 1) w <- 1
    w
  })
  
  median.country <- reactive({
    dl.country <- e0.country.dlcurves(DLx, e0.pred, country=country.int())
    me.ctry <- apply(dl.country, 2, median)
    w <- weights()
    return((me.ctry - (1-w)*me.world)/w)
  })
  
  output$DLPlot <- renderPlot({
    if(is.null(input$country)) return(NULL)
    country <- country.int()
    if(! country %in% get.countries.table(e0.pred)$code) return(NULL)
    plotDL(country, refcountry.int())
    cscurve <- median.country()
    new.w <- input$weight
    tscurve <- new.w * cscurve + (1-new.w)*me.world
    lines(DLx, tscurve, col='black', lwd=2)
  })

  plote0 <- function(country, refcountry){
    e0.trajectories.plot(e0.pred, country=country, nr.traj=10, pi=80)
    refcobj <- get.country.object(refcountry, e0.pred$mcmc.set$meta)
    if(!is.null(refcobj$code)) 
      e0.trajectories.plot(e0.pred, country=refcobj$code, add=TRUE, nr.traj=0, pi=80,
                          show.legend=FALSE, col=c(rep('orange',5)))
  }
  
  get.one.dlvalue <- function(x, country, new.w) {
    dlmed <- median(e0.country.dlcurves(x, e0.pred, country=country))
    w <- weights()
    world <- median(e0.world.dlcurves(x, e0.pred))
    ctryll <- (dlmed - (1-w)*world)/w
    return(new.w * ctryll + (1-new.w)*world)
  }
  
output$e0Plot <- renderPlot({
  if(is.null(input$country)) return(NULL)
  country <- country.int()
  if(! country %in% get.countries.table(e0.pred)$code) return(NULL)
  plote0(country, refcountry.int())
  time <- seq(2013, 2098, by=5)
  e0 <- e0.trajectories.table(e0.pred, country)["2013",1]
  for(i in 2:length(time)) {
    newdl <- get.one.dlvalue(e0[length(e0)], country, input$weight)
    e0 <- c(e0, e0[length(e0)]+newdl)
  }
  #browser()
  lines(time, e0, col='black', lwd=2)
})

output$cselection <- renderUI({
  ct <- get.countries.table(e0.pred)
  o <- order(ct[,'name'])
  codes <- as.character(ct[o,'code'])
  names <- as.character(ct[o,'name'])
  countries <- structure(
    codes,
    names = names
  )
  do.call('selectInput', list('country', 'Country:', countries, multiple=FALSE, selectize = FALSE,
                              selected=countries[codes==68]
  ))
})

output$cselection.ref <- renderUI({
  ct <- get.countries.table(e0.pred)
  o <- order(ct[,'name'])
  codes <- as.character(ct[o,'code'])
  names <- as.character(ct[o,'name'])
  countries <- structure(
    codes,
    names = names
  )
  do.call('selectInput', list('refcountry', 'Reference country:', countries, multiple=FALSE, selectize = FALSE,
                              selected=countries[codes==600]
  ))
})
})

