library(bayesLife)
server <- function(input, output) {
  output$DLPlot <- renderPlot({
    #dir <- "~/talks/UN16/e0/converged/e0/sim03092016/"
    dir <- '/mnt/shinydata/apps/wppExplorer/e0simulation'
    e0.pred <- get.e0.prediction(dir)
    country <- input$country
    t <- e0.trajectories.table(e0.pred, country)
    xct <- t[1:12,'median']
    act <- apply(e0.country.dlcurves(xct, e0.pred, country=country), 2, median)
    bct <- apply(e0.world.dlcurves(xct, e0.pred), 2, median)
    yct <- diff(t[1:13,'median'])
    w <- - sum((act-bct)*(bct-yct))/sum((bct-yct)^2)
    e0.DLcurve.plot(e0.pred, country=country, nr.curves=10)
    legend("topleft", legend=c("country", "world", "weighted"), col=c("red", "blue", "black"), lwd=2, lty=1, bty='n')
    x <- seq(40, 90, length=100) 
    dl.country <- e0.country.dlcurves(x, e0.pred, country=country)
    me.ctry <- apply(dl.country, 2, median)
    dl.world <- e0.world.dlcurves(x, e0.pred)
    me.world <- apply(dl.world, 2, median)
    lines(x, me.world, col='blue', lwd=2)
    cscurve <- (me.ctry - (1-w)*me.world)/w
    new.w <- input$weight
    tscurve <- new.w * cscurve + (1-new.w)*me.world
    lines(x, tscurve, col='black', lwd=2)
  })
}


