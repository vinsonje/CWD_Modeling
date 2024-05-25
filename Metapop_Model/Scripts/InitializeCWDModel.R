#The purpose of this function is to initialize the CWD model
#by creating the landscape and population (families)
#and puts the infected individuals in the middle of the landscape
#Run this before running SimulateOneRun.R

init.CWD = function(){
######################
####Source Functions
#####################
#running this will load in all the parameters for the CWD 
#(overkill most likely already read in)
source(paste(getwd(), "/CWDParms.R", sep = ''))

######################
####Create grid
######################
grid = create.grid(grid.xmax, grid.ymax, cell.x.size, cell.y.size)

centroids = grid[,6:7]
cells = nrow(grid)

######################
####Initialize Population
#####################
pop = InitializeFamilies(N0, fs, cells, centroids, 0, 0)

######################################
######## Initialize Infection ######## 
######################################
if(I0>0){
#find the midpoint of the grid
midpoint = c(max(centroids[,1]/2),max(centroids[,2]/2))
mid.cell = which(centroids[, 1] >= midpoint[1] & centroids[, 2] >= midpoint[2])[1] #location on grid closest to midpoint

infected = InitializeFamilies(N0, I0, cells, centroids, "inf", mid.cell)

#combine infected with pop matrix
pop = rbind(pop,infected)
}
######################################
######## Initialize Infection ######## 
######################################
landscape.prions = data.frame(centroids, prions = rep(0, dim(centroids)[1]))

return(list(pop, landscape.prions, centroids, cells))
}