########################################
#Harvesting function
########################################
#The purpose of this function is to simulate
#harvesting (hunting) on a landscape. It has inputs of:
#pop = population matrix
#centroids = centroids matrix
#h.loc = vector of grid ids where SS happens
#h.time = vector of times when SS happens
#h.radius = the radius around the cells centroid where SS is effective
#ss.eff = the effectiveness of sharpshooting, the proportion of the population that sharpshooting can remove 
#thyme = the current time of the simulation
#########################################

#I might need to think about this some more. Would it make more sense to simualte individual hunters on the landscape? 
#Right now it is really is just a "less effective" SS. 
harvestingCWD = function(pop, centroids, h.loc, h.time, h.radius, h.num, thyme){
  
  pop.out = pop
  # print(dim(pop.out)[1])
  
  if(thyme %in% h.time){
    for(q in 1:length(h.loc)){
      h.loc.id.now = h.loc[q]
      h.loc.xloc.now = centroids[h.loc.id.now, 1]
      h.loc.yloc.now = centroids[h.loc.id.now, 2]
      
      cells.in.h.radius = NULL
      for(w in 1:dim(centroids)[1]){
        potential.cell.x = centroids[w,1]
        potential.cell.y = centroids[w,2]
        
        distance = sqrt((h.loc.xloc.now - potential.cell.x)^2 + (h.loc.yloc.now - potential.cell.y)^2)
        
        if(distance <= h.radius){
          # print("true")
          cells.in.h.radius = append(cells.in.h.radius, w)
        }
      }#end of distance calc loop
      
      fams.in.radius = intersect(pop.out[,3], cells.in.h.radius)
      # print(paste("removing", length(fams.in.radius), "families"))
      
      fams.rem.index = which(pop[,3] %in% fams.in.radius)
      
      if(ss.eff == 1.0){pop.out = pop.out[-fams.rem.index,]}else{
        
        for(f in 1:length(fams.rem.index)){
          pop.now = pop[fams.rem.index[f],]
          fam.mems = c(rep("S",pop.now[8]), rep("E", pop.now[9]), rep("I", pop.now[10]))
          
          rem.mems = sample(fam.mems, h.num, replace = FALSE)
          S.rem = length(which(rem.mems == "S"))
          E.rem = length(which(rem.mems == "E"))
          I.rem = length(which(rem.mems == "I"))
          
          pop[fams.rem.index[f], 8] = pop[fams.rem.index[f], 8] - S.rem
          pop[fams.rem.index[f], 9] = pop[fams.rem.index[f], 9] - E.rem
          pop[fams.rem.index[f], 10] = pop[fams.rem.index[f], 10] - I.rem
          
          pop[fams.rem.index[f], 1] = sum(pop[fams.rem.index[f],8:10])
        } #end for loop
      } #end else
      # print(dim(pop.out)[1])
      
    }#end of h.loc loop
  }#end of h.time if
  
  return(pop.out)
}#end of function


# h.loc = c(11, 265)
# h.time = 10
# h.radius = 1.0
# 
# thyme = 12
# 
# test.pop = pop
# 
# ss.test = sharpshootingCWD(test.pop, centroids, h.loc, h.time, h.radius, thyme)

