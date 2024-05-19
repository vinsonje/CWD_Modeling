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
#h.num = the number of individuals removed at each harvesting event
#thyme = the current time of the simulation
#########################################

#I need to save the individuals and their information to be used in a surveillance

harvestingCWD = function(pop, centroids, h.loc, h.time, h.radius, h.num, thyme){
  
  pop.out = pop
  harvest.out = matrix(c(0, 0, 0, 0, 0), nrow = 1)
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
      
      pop.sub = pop[fams.rem.index, , drop = FALSE]
      
      pop.classes = colSums(pop.sub[,8:10, drop = FALSE])
      
      deer.in.sub = rep(c("S", "E", "I"), pop.classes)
      
      S.in.sub = rep(pop.sub[,3], pop.sub[,8])
      E.in.sub = rep(pop.sub[,3], pop.sub[,9])
      I.in.sub = rep(pop.sub[,3], pop.sub[,10])
      
      if(h.num > length(deer.in.sub)){h.num = length(deer.in.sub)}
      rem.mems = sample(deer.in.sub, h.num, replace = FALSE)
      
      S.rem = length(which(rem.mems == "S"))
      E.rem = length(which(rem.mems == "E"))
      I.rem = length(which(rem.mems == "I"))
      
      S.rem.loc = sample(S.in.sub, S.rem, replace = FALSE)
      E.rem.loc = sample(E.in.sub, E.rem, replace = FALSE)
      I.rem.loc = sample(I.in.sub, I.rem, replace = FALSE)
      
      S.rem.ct = data.frame(table(S.rem.loc))
      E.rem.ct = data.frame(table(E.rem.loc))
      I.rem.ct = data.frame(table(I.rem.loc))
      
      for(f in 1:length(fams.rem.index)){
        
        S.rem = 0
        E.rem = 0
        I.rem = 0
        
        loc = pop[fams.rem.index[f], 3]
        
        if(loc %in% S.rem.ct$S.rem.loc){S.rem = S.rem.ct$Freq[which(S.rem.ct$S.rem.loc==loc)]}
        
        if(loc %in% E.rem.ct$E.rem.loc){E.rem = E.rem.ct$Freq[which(E.rem.ct$E.rem.loc==loc)]}
        
        if(loc %in% I.rem.ct$I.rem.loc){S.rem = I.rem.ct$Freq[which(I.rem.ct$I.rem.loc==loc)]}
        
        pop.out[fams.rem.index[f], 8] = pop.out[fams.rem.index[f], 8] - S.rem
        pop.out[fams.rem.index[f], 9] = pop.out[fams.rem.index[f], 9] - E.rem
        pop.out[fams.rem.index[f], 10] = pop.out[fams.rem.index[f], 10] - I.rem
        
        pop.out[fams.rem.index[f], 1] = sum(pop.out[fams.rem.index[f],8:10])
        
        harvest.out.temp = c(time = thyme, loc = loc, S = S.rem, E = E.rem, I = I.rem)
        harvest.out = rbind(harvest.out, harvest.out.temp)
        
      } #end for loop to remove individuals from each cell
      
    }#end of h.loc loop
  }#end of h.time if
  
  pop.out = pop.out[which(pop.out[,1]>0),]
  harvest.out = harvest.out[which(rowSums(harvest.out[,3:5, drop = FALSE])>0),]
  rownames(harvest.out) = NULL
  
  return(list(pop.out, harvest.out))
}#end of function

#testing
# h.loc = c(11, 265)
# h.time = 10
# h.radius = 3.0
# h.num = 5
# 
# thyme = 10
# 
# test.pop = pop
# 
# harvest.test = harvestingCWD(test.pop, centroids, h.loc, h.time, h.radius, h.num, thyme)
# harvest.test[[2]]
