rm(list = ls())
gc()
set.seed(100)
start.time <- Sys.time()

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDSourcer.R", sep = ''))

init.model = init.CWD()

pop = init.model[[1]]

landscape.prions = init.model[[2]]

centroids = init.model[[3]]

onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = FALSE)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

# plot.landscape.meta(onerun, grid.xmax, grid.ymax, save = TRUE)
CWD.plots.static(onerun, grid.xmax, grid.ymax, thyme)

