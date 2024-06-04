########################################
#Dispersal
########################################

dispersalCWD = function(pop, dispersal, disp.dist, disp.times, thyme){
  
  pop.out = pop
  disp.out = data.frame()
  
  p.disp = 1 - exp(-dispersal)
  
  if(thyme %in% disp.times){
    
    for(z in 1:dim(pop.out)[1]){
      
      S.disp.ind = rbinom(1, pop.out[z, 8], p.disp)
      E.disp.ind = rbinom(1, pop.out[z, 9], p.disp)
      I.disp.ind = rbinom(1, pop.out[z, 10], p.disp)
      tot.disp.ind = S.disp.ind + E.disp.ind + I.disp.ind
      
      pop.out[z, 1] = pop.out[z, 1] - tot.disp.ind
      pop.out[z, 8] = pop.out[z, 8] - S.disp.ind
      pop.out[z, 9] = pop.out[z, 9] - E.disp.ind
      pop.out[z, 10] = pop.out[z, 10] - I.disp.ind
      
      dist =  sqrt((centroids[, 1] - pop.out[z, 5])^2 + (centroids[, 2] - pop.out[z, 6])^2)
      
      if(S.disp.ind != 0){
        for(S in 1:length(S.disp.ind)){
          disp.move = rgamma(1, shape = disp.dist[1], scale = disp.dist[2])
          poss.cells = which(dist < disp.move)
          poss.fids = pop[which(pop[,3] %in% poss.cells), 12]
          poss.fids = poss.fids[!poss.fids == pop.out[z, 12]]
          if(length(poss.fids) > 0){
            S.cell.moved = sample(poss.fids, 1, replace = FALSE)
          }else{S.cell.moved = pop.out[z,12]}
          fam.ind = which(pop.out[,12] == S.cell.moved)
          pop.out[fam.ind, 1] = pop.out[fam.ind, 1] + 1
          pop.out[fam.ind, 8] = pop.out[fam.ind, 8] + 1
          disp.out = rbind(disp.out, c(time = thyme, ind = "S", old.loc = pop.out[z,3], new.loc = pop.out[fam.ind,3]))
        } #end S move loop
      } #end S if 
      
      if(E.disp.ind != 0){
        for(E in 1:length(E.disp.ind)){
          disp.move = rgamma(1, shape = disp.dist[1], scale = disp.dist[2])
          poss.cells = which(dist < disp.move)
          poss.fids = pop[which(pop[,3] %in% poss.cells), 12]
          poss.fids = poss.fids[!poss.fids == pop.out[z, 12]]
          if(length(poss.fids) > 0){
            E.cell.moved = sample(poss.fids, 1, replace = FALSE)
          }else{E.cell.moved = pop.out[z,12]}
          fam.ind = which(pop.out[,12] == E.cell.moved[E])
          pop.out[fam.ind, 1] = pop.out[fam.ind, 1] + 1
          pop.out[fam.ind, 9] = pop.out[fam.ind, 9] + 1
          disp.out = rbind(disp.out, c(time = thyme, ind = "E", old.loc = pop.out[z,3], new.loc = pop.out[fam.ind,3]))
        } #end E move loop
      } #end E if 
      
      if(I.disp.ind != 0){
        for(I in 1:length(I.disp.ind)){
          disp.move = rgamma(1, shape = disp.dist[1], scale = disp.dist[2])
          poss.cells = which(dist < disp.move)
          poss.fids = pop[which(pop[, 3] %in% poss.cells), 12]
          poss.fids = poss.fids[!poss.fids == pop.out[z, 12]]
          if(length(poss.fids) > 0){
            I.cell.moved = sample(poss.fids, 1, replace = FALSE)
          }else{I.cell.moved = pop.out[z, 12]}
          fam.ind = which(pop.out[,12] == I.cell.moved[I])
          pop.out[fam.ind, 1] = pop.out[fam.ind, 1] + 1
          pop.out[fam.ind, 10] = pop.out[fam.ind, 10] + 1
          disp.out = rbind(disp.out, c(time = thyme, ind = "I", old.loc = pop.out[z,3], new.loc = pop.out[fam.ind,3]))
        } #end I move loop
      } #end I if 

      
  } #end for loop
    
    names(disp.out) = c("time", "ind", "old.loc", "new.loc")
} #end thyme if

  return(list(pop.out, disp.out))
}