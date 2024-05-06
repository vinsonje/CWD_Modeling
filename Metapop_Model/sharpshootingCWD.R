########################################
#Sharpshooting function
########################################
#The purpose of this function is to simulate
#sharpshooting on a landscape. It has inputs of:
#pop = population matrix
#centroids = centroids matrix
#ss.loc = vector of grid ids where SS happens
#ss.time = vector of times when SS happens
#ss.radius = the radius around the cells centroid where SS is effective
#ss.eff = the effectiveness of sharpshooting, the proportion of the population that sharpshooting can remove 
#thyme = the current time of the simulation
#########################################

sharpshootingCWD = function(pop, centroids, ss.loc, ss.time, ss.radius, ss.eff, thyme){
  
  pop.out = pop
  # print(dim(pop.out)[1])
  
  if(thyme %in% ss.time){
  for(q in 1:length(ss.loc)){
    ss.loc.id.now = ss.loc[q]
    ss.loc.xloc.now = centroids[ss.loc.id.now, 1]
    ss.loc.yloc.now = centroids[ss.loc.id.now, 2]
    
    cells.in.ss.radius = NULL
    for(w in 1:dim(centroids)[1]){
      potential.cell.x = centroids[w,1]
      potential.cell.y = centroids[w,2]
      
      distance = sqrt((ss.loc.xloc.now - potential.cell.x)^2 + (ss.loc.yloc.now - potential.cell.y)^2)
      
      if(distance <= ss.radius){
        # print("true")
        cells.in.ss.radius = append(cells.in.ss.radius, w)
        }
    }#end of distance calc loop
    
    fams.in.radius = intersect(pop.out[,3], cells.in.ss.radius)
    # print(paste("removing", length(fams.in.radius), "families"))
   
    fams.rem.index = which(pop[,3] %in% fams.in.radius)
    
    if(ss.eff == 1.0){pop.out = pop.out[-fams.rem.index,]}else{
      
      #Something you might need to do here is basically ungroup the families sample from the population within the radius.
      #Without doing that it, I think it may inflate the number of ind. ss'ed out
      for(f in 1:length(fams.rem.index)){
        pop.now = pop[fams.rem.index[f],]
        fam.mems = c(rep("S",pop.now[8]), rep("E", pop.now[9]), rep("I", pop.now[10]))
        num.rem = ceiling(pop.now[1]*ss.eff)
        
        rem.mems = sample(fam.mems, num.rem, replace = FALSE)
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
    
  }#end of ss.loc loop
  }#end of ss.time if
  
  return(pop.out)
}#end of function


# ss.loc = c(11, 265)
# ss.time = 10
# ss.radius = 1.0
# 
# thyme = 12
# 
# test.pop = pop
# 
# ss.test = sharpshootingCWD(test.pop, centroids, ss.loc, ss.time, ss.radius, thyme)

