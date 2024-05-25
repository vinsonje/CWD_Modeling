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

sharpshootingCWD = function(pop, centroids, surv.data, ss.shooters, ss.time, ss.radius, ss.eff, ss.strat, thyme){
  
  surv.out = surv.data
  pop.out = pop
  SS.out = NULL
  
  if(thyme %in% ss.time){
    
    num.rem.out = NULL
    
    surv.counts = aggregate(num ~ loc, surv.data, sum, simplify = TRUE, drop = TRUE)
    surv.counts[,1] = as.numeric(as.character(surv.counts[,1]))
    ss.poss.locs = unique(surv.counts[,1])
    
    if(length(ss.poss.locs) < ss.shooters){ss.shooters = length(ss.poss.locs)}
    
    if(ss.strat == "random"){ss.loc = sample(ss.poss.locs, ss.shooters, replace = FALSE)}
    if(ss.strat == "priority"){
      ss.poss.loc.order = surv.counts[order(surv.counts[,2], decreasing = TRUE),]
      
      ss.loc = head(ss.poss.loc.order[,1], ss.shooters)
    }
    
    
  for(q in 1:length(ss.loc)){
    
    num.rem.temp = 0
    
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
   
    fams.rem.index = which(pop.out[,3] %in% fams.in.radius)
    
    if(ss.eff == 1.0){
      
      num.rem.temp = sum(pop.out[fams.rem.index, 8:10])
      
      pop.out = pop.out[-fams.rem.index,]
    
    }else{
      
    
      for(f in 1:length(fams.rem.index)){
        pop.now = pop.out[fams.rem.index[f],]
        fam.mems = c(rep("S",pop.now[8]), rep("E", pop.now[9]), rep("I", pop.now[10]))
        num.rem = ceiling(pop.now[1]*ss.eff)
        num.rem.temp = num.rem.temp + num.rem
        
        rem.mems = sample(fam.mems, num.rem, replace = FALSE)
        S.rem = length(which(rem.mems == "S"))
        E.rem = length(which(rem.mems == "E"))
        I.rem = length(which(rem.mems == "I"))
        
        pop.out[fams.rem.index[f], 8] = pop.out[fams.rem.index[f], 8] - S.rem
        pop.out[fams.rem.index[f], 9] = pop.out[fams.rem.index[f], 9] - E.rem
        pop.out[fams.rem.index[f], 10] = pop.out[fams.rem.index[f], 10] - I.rem
        
        pop.out[fams.rem.index[f], 1] = sum(pop.out[fams.rem.index[f],8:10])
      } #end for loop
    } #end else
    
    num.rem.out = c(num.rem.out, num.rem.temp)
    surv.out = surv.out[-which(surv.out[,1] == ss.loc[q]),]  
  }#end of ss.loc loop
    
    pop.out = pop.out[which(pop.out[,1]>0),]
    
    SS.out = data.frame(time = thyme, loc = ss.loc, num.rem = num.rem.out)
    
  }#end of ss.time if
  
  return(list(pop.out, surv.out, SS.out))
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

