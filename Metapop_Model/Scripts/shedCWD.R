#Shedding 

shedCWD = function(pop, landscape.prions, shed){
  
Imat = pop[pop[,10] > 0, , drop = FALSE]

if(dim(Imat)[1]>0){
  
  for(k in 1:dim(Imat)[1]){
    
    prions.shed = rpois(Imat[k,10], shed)
    # print(paste(prions.shed, "prions"))
    land.index = intersect(which(landscape.prions[,1] == Imat[k,5]), which(landscape.prions[,2] == Imat[k,6]))
    
    landscape.prions[land.index,]$prions = landscape.prions[land.index, ]$prions + sum(prions.shed)
  }
  
}#end of if statement to test if there are any Is

return(landscape.prions)
}#end function