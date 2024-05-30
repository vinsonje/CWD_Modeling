#The purpose of this script is to initialize the deer population dataframe
#This will replace the various state variable matrices (ie S, I)
#instead, values will be assigned to indicate statuses and location

#N0-population size, using given density and area of grid
#fs-average family size, setting manually
#centroids-center coordinates of each cell
#type- if 0, initialize population for start of population; else, use init_locs and total number to initialize an infected individual
#init_locs- used for births
#n-number of infected individuals

InitializeFamilies <- function(N0, fs, cells, centroids, type, init_locs){
  if(type==0){
    
    fn_i = N0/fs #Get the initializing number of families
    if(fn_i > cells){fn_i = cells} #if there are more families than cells, need to constrain to only have any many families as cells
    assigns = rbinom(cells, 1, fn_i/cells) #randomly assign families to cells
    fn = sum(assigns) #generated pop size from random assignment of families to cells
    init_locs = which(assigns == 1) #get the locations where families have been initialized
    
    #Initialize the family dataframe
    #each row is a family
    
    fam.size = rpois(fn,fs) #family size with avg as lambda in a poisson
    dis.status = rep(0, fn) #
    grid.loc = init_locs #this will be grid location (row number)
    move.dis = rep(0, fn) #this will be assigned movement distance
    x.now = centroids[grid.loc, 1] #present location X 
    y.now = centroids[grid.loc, 2] #present location Y
    prev.loc = init_locs #previous location (grid row number)
    S.num = fam.size #number of S status in family (susceptible)
    E.num = rep(0, fn) #number of E status in family (exposed)
    I.num = rep(0, fn) #number of I status in family (infectious)
    Z.num = rep(0, fn) #number of Z status in family (dead)
    fam.id = seq(1:fn)
    
    pop =  matrix(c(fam.size, dis.status, grid.loc, move.dis, x.now, y.now, prev.loc, S.num, E.num, I.num, Z.num, fam.id), byrow = FALSE, ncol = 12)
    
  } else{ 
    
    fam.size = fs #family size with avg as lambda in a poisson
    dis.status = 1 #this will be disease status
    grid.loc = init_locs #this will be grid location (row number)
    move.dis = 0 #this will be assigned movement distance
    x.now = centroids[grid.loc,1] #present location X 
    y.now = centroids[grid.loc,2] #present location Y
    prev.loc = init_locs #previous location (grid row number)	
    S.num = 0 #number of S status in family (susceptible)
    E.num = 0 #number of E status in family (exposed)
    I.num = fs #number of I status in family (infectious)
    Z.num = 0 #number of Z status in family (dead)
    fam.id = -999
    
    pop =  matrix(c(fam.size, dis.status, grid.loc, move.dis, x.now, y.now, prev.loc, S.num, E.num, I.num, Z.num, fam.id), byrow = FALSE, ncol = 12)
    
  }
  
  pop=pop[pop[,1]>0,,drop=FALSE]
  
  return(pop)
}



