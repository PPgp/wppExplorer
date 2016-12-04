library(bayesLife)

shinyServer(function(input, output, session) {

  sim.dir <- '/mnt/shinydata/apps/wppExplorer/e0simulation'
  #sim.dir <- "~/talks/UN16/e0/converged/e0/sim03092016/"
  e0.pred <<- get.e0.prediction(sim.dir)
  DLx <<- seq(20, 90, length=100) 
  dl.world <- e0.world.dlcurves(DLx, e0.pred)
  me.world <<- apply(dl.world, 2, median)
  
  observe({
    if(is.null(input$country)) return(NULL)
    if(! input$country %in% get.countries.table(e0.pred)$code) return(NULL)
    w <- weights()
    updateSliderInput(session, "weight", value=w)
  })
  
  plotDL <- function(country){
    e0.DLcurve.plot(e0.pred, country=input$country, nr.curves=10)
    legend("topleft", legend=c("country", "world", "weighted"), col=c("red", "blue", "black"), lwd=2, lty=1, bty='n')
    lines(DLx, me.world, col='blue', lwd=2)
  }
  
  weights <- reactive({
    country <- input$country
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
    dl.country <- e0.country.dlcurves(DLx, e0.pred, country=input$country)
    me.ctry <- apply(dl.country, 2, median)
    w <- weights()
    return((me.ctry - (1-w)*me.world)/w)
  })
  
  output$DLPlot <- renderPlot({
    if(is.null(input$country)) return(NULL)
    country <- input$country
    if(! country %in% get.countries.table(e0.pred)$code) return(NULL)
    plotDL(country)
    cscurve <- median.country()
    new.w <- input$weight
    tscurve <- new.w * cscurve + (1-new.w)*me.world
    lines(DLx, tscurve, col='black', lwd=2)
  })
})


