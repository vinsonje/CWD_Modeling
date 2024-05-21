FastMovementCWD = function(pop,centroids,shift,inc){

#get distances from gamma distribution
pop[,4] = rgamma(nrow(pop), shape=shift[1], scale=shift[2])

#set those less than inc to 0
pop[pop[,4] < inc,][,4]=0 

#set present locations to previous locations
pop[,7] = pop[,3]

# m1 = parallelMovementRcpp_portion(pop,pop[,1,drop=FALSE],pop[,3,drop=FALSE],centroids,20) #this determines the cell id of where the family moved to. 

m1 = movement_fun(pop,centroids, 10, move.strat = "maxden") #this determines the cell id of where the family moved to. 

pop[,3] = m1 #assign new cell id 

pop[,5:6] = centroids[pop[,3],] #need something to update the x.now, ynow I think. 

#if stop function here.. if no cells to move to
#any(pop[,3]==nrow(centroids)+1000)
if(any(pop[,3] == nrow(centroids)+1000)) {
    stop("No cells to move to! This shouldn't happen")
  }

#if stop function here.. 
#if all sounders with dist equals zero NOT contained in rows for which prev locs=present locs
if(all(!(which(pop[,4] == 0)%in%which(pop[,3]==pop[,7])))){
	  stop("All sounders with distance=0 should have same prev. and present locations")
}

return(pop)
}
