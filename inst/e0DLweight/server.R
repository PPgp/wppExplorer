library(bayesLife)

shinyServer(function(input, output, session) {
  DLx <<- seq(20, 90, length=100)
  sim.dirs <- c('/mnt/shinydata/apps/wppExplorer/e0simulationSumDelta83',
                '/mnt/shinydata/apps/wppExplorer/e0simulation')
  #sim.dirs <<- c("~/bayespop/R/LE/crossovers/DLmeans/sim20161216",
  #               "~/talks/UN16/e0/converged/e0/sim03092016")
  sim.dir <- sim.dirs[1]
  react <- reactiveValues(e0.pred = get.e0.prediction(sim.dir), 
                          me.world = NULL)

  ct <- get.countries.table(get.e0.prediction(sim.dir))
  o <- order(ct[,'name'])
  list.of.countries <<- structure(
    as.character(ct[o,'code']),
    names = as.character(ct[o,'name'])
  )
  
  country.int <- reactive({as.integer(input$country)})
  refcountry.int <- reactive({as.integer(input$refcountry)})
  simulation.int <- reactive({as.integer(input$simulation)})
  
  observe({
    dl.world <- e0.world.dlcurves(DLx, react$e0.pred)
    react$me.world <- apply(dl.world, 2, median)
  })
  
  observe({
    if(is.null(input$country)) return(NULL)
    if(! country.int() %in% get.countries.table(react$e0.pred)$code) return(NULL)
    w <- weights()
    updateSliderInput(session, "weight", value=w)
  })
  
   observe({
     if(is.null(input$simulation)) return(NULL)
    if(sim.dirs[simulation.int()] != react$e0.pred$output.dir) {
      #browser()
      react$e0.pred <- get.e0.prediction(sim.dirs[simulation.int()])
    }
   })
  
  plotDL <- function(country, refcountry){
    e0.DLcurve.plot(react$e0.pred, country=country.int(), nr.curves=10)
    refcobj <- get.country.object(refcountry, react$e0.pred$mcmc.set$meta)
    if(!is.null(refcobj$code)) {
      refdl <- apply(e0.country.dlcurves(DLx, react$e0.pred, country=refcobj$code), 2, median)
      lines(DLx, refdl, col='orange', lwd=2)
      legend("topleft", legend=c("country", "world", "weighted", refcobj$name), col=c("red", "blue", "black", "orange"), lwd=2, lty=1, bty='n')
    } else legend("topleft", legend=c("country", "world", "weighted"), col=c("red", "blue", "black"), lwd=2, lty=1, bty='n')
    lines(DLx, react$me.world, col='blue', lwd=2)
  }
  
  weights <- reactive({
    country <- country.int()
    t <- e0.trajectories.table(react$e0.pred, country)
    xct <- t[1:12,'median']
    yct <- diff(t[1:13,'median'])
    is.pos <- yct >= 0
    yct <- yct[is.pos]
    act <- apply(e0.country.dlcurves(xct, react$e0.pred, country=country), 2, median)[is.pos]
    bct <- apply(e0.world.dlcurves(xct, react$e0.pred), 2, median)[is.pos]
    w <- - sum((act-bct)^2)/sum((act-bct)*(bct-yct))
    if(w <= 0 || w > 1) w <- 1
    w
  })
  
  median.country <- reactive({
    dl.country <- e0.country.dlcurves(DLx, react$e0.pred, country=country.int())
    me.ctry <- apply(dl.country, 2, median)
    w <- weights()
    return((me.ctry - (1-w)*react$me.world)/w)
  })
  
  output$DLPlot <- renderPlot({
    if(is.null(input$country)) return(NULL)
    country <- country.int()
    if(! country %in% get.countries.table(react$e0.pred)$code) return(NULL)
    plotDL(country, refcountry.int())
    cscurve <- median.country()
    new.w <- input$weight
    tscurve <- new.w * cscurve + (1-new.w)*react$me.world
    lines(DLx, tscurve, col='black', lwd=2)
  })

  plote0 <- function(country, refcountry){
    e0.trajectories.plot(react$e0.pred, country=country, nr.traj=10, pi=80)
    refcobj <- get.country.object(refcountry, react$e0.pred$mcmc.set$meta)
    if(!is.null(refcobj$code)) 
      e0.trajectories.plot(react$e0.pred, country=refcobj$code, add=TRUE, nr.traj=0, pi=80,
                          show.legend=FALSE, col=c(rep('orange',5)))
  }
  
  get.one.dlvalue <- function(x, country, new.w) {
    dlmed <- median(e0.country.dlcurves(x, react$e0.pred, country=country))
    w <- weights()
    world <- median(e0.world.dlcurves(x, react$e0.pred))
    ctryll <- (dlmed - (1-w)*world)/w
    return(new.w * ctryll + (1-new.w)*world)
  }
  
output$e0Plot <- renderPlot({
  if(is.null(input$country)) return(NULL)
  country <- country.int()
  if(! country %in% get.countries.table(react$e0.pred)$code) return(NULL)
  plote0(country, refcountry.int())
  time <- seq(2013, 2098, by=5)
  e0 <- e0.trajectories.table(react$e0.pred, country)["2013",1]
  for(i in 2:length(time)) {
    newdl <- get.one.dlvalue(e0[length(e0)], country, input$weight)
    e0 <- c(e0, e0[length(e0)]+newdl)
  }
  #browser()
  lines(time, e0, col='black', lwd=2)
})

output$cselection <- renderUI({
  do.call('selectInput', list('country', 'Country:', list.of.countries, multiple=FALSE, selectize = FALSE,
                              selected=list.of.countries[list.of.countries==68]
  ))
})

output$cselection.ref <- renderUI({
  do.call('selectInput', list('refcountry', 'Reference country:', list.of.countries, multiple=FALSE, selectize = FALSE,
                              selected=list.of.countries[list.of.countries==600]
  ))
})
})

