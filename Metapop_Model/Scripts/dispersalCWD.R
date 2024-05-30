########################################
#Disperal
########################################

dispersalCWD = function(pop, dispersal, disp.dist, disp.times, thyme){
  
  pop.out = pop
  
  p.disp = 1 - exp(-dispersal)
  
  if(thyme %in% disp.times){
    
    for(z in 1:dim(pop.out)[1]){
      
      S.disp.ind = rbinom(1, pop.out[z, 8], p.disp)
      E.disp.ind = rbinom(1, pop.out[z, 9], p.disp)
      I.disp.ind = rbinom(1, pop.out[z, 10], p.disp)
      tot.disp.ind = S.disp.ind + E.disp.ind + I.disp.ind
      
      dist =  sqrt((centroids[, 1] - pop.out[z, 5])^2 + (centroids[, 2] - pop.out[z, 6])^2)
      
      poss.cells = which(dist < disp.dist)
      fams.in.range = subset(pop.out, pop.out[,3] %in% poss.cells)
      
      if(dim(fams.in.range)[1] > 0){
        
      pop.out[z, 1] = pop.out[z, 1] - tot.disp.ind
      pop.out[z, 8] = pop.out[z, 8] - S.disp.ind
      pop.out[z, 9] = pop.out[z, 9] - E.disp.ind
      pop.out[z, 10] = pop.out[z, 10] - I.disp.ind
      
      S.cells.moved = sample(fams.in.range[,3], S.disp.ind, replace = TRUE)
      E.cells.moved = sample(fams.in.range[,3], E.disp.ind, replace = TRUE)
      I.cells.moved = sample(fams.in.range[,3], I.disp.ind, replace = TRUE)
      
      if(length(S.cells.moved)>0){
        for(S in 1:length(S.cells.moved)){
          fam.ind = which(pop.out[,3] == S.cells.moved[S])
          pop.out[fam.ind, 1] = pop.out[fam.ind, 1] + 1
          pop.out[fam.ind, 8] = pop.out[fam.ind, 8] + 1
        } #end S move loop
      } #end S if 
      
      if(length(E.cells.moved)>0){
        for(E in 1:length(E.cells.moved)){
          fam.ind = which(pop.out[,3] == E.cells.moved[E])
          pop.out[fam.ind, 1] = pop.out[fam.ind, 1] + 1
          pop.out[fam.ind, 9] = pop.out[fam.ind, 9] + 1
        } #end E move loop
      } #end E if 
      
      if(length(I.cells.moved)>0){
        for(I in 1:length(I.cells.moved)){
          fam.ind = which(pop.out[,3] == I.cells.moved[I])
          pop.out[fam.ind, 1] = pop.out[fam.ind, 1] + 1
          pop.out[fam.ind, 10] = pop.out[fam.ind, 10] + 1
        } #end I move loop
      } #end I if 
      
      } #end if fams.in.range
      
  } #end for loop
} #end thyme if
  
  return(pop.out)
}