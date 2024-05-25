###########################################
#Corpse "Burst" Function
###########################################
#The purpose of this function is to simulate
#the "burst" of prions that are added from a dead infectious individual
#pop = population matrix
#prions = landscape.prions matrix
#burst.prions = avg. number of prions that a dead inf. ind. produces in a burst (Poisson)
#########################################

corpse_burstCWD = function(pop, landscape.prions, burst.prions){
  landscape.prions.out = landscape.prions
  if(sum(pop[,11])>0){
    
  Z.mat = pop[which(pop[,11]>0), , drop = FALSE]
  
  for(i in 1:dim(Z.mat)[1]){
    prions.from.Z = rpois(Z.mat[i,11], burst.prions)
    
    landscape.prions.out[Z.mat[i, 3], 3] = landscape.prions.out[Z.mat[i,3], 3] + sum(prions.from.Z)
  }
  
  pop[, 11] = 0
  }
  return(list(pop, landscape.prions.out))
  
}