library(bayesLife)
sim.dir <- "~/talks/UN16/e0/converged/e0/sim03092016/"
#sim.dir <- '/mnt/shinydata/apps/wppExplorer/e0simulation'
e0.pred <<- get.e0.prediction(sim.dir)
DLx <<- seq(20, 90, length=100) 
dl.world <- e0.world.dlcurves(DLx, e0.pred)
me.world <<- apply(dl.world, 2, median)