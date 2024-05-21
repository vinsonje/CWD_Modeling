movement_fun = function(pop, centroids, max.den, move.strat){
  
  move.loc.out = pop[,3]
  
  which.move = which(pop[,4]>0)
  if(length(which.move) > 0){
  
  move.mat = pop[which.move, , drop = FALSE]
  
  for(i in 1:dim(move.mat)[1]){

  dist =  sqrt((centroids[, 1] - move.mat[i, 5])^2 + (centroids[, 2] - move.mat[i, 6])^2)
  
  poss.cells = which(dist < move.mat[i,4])
  poss.cells = poss.cells[!poss.cells == move.mat[i,3]]
  
  if(move.strat == "random"){move.loc = sample(poss.cells, 1, replace = FALSE)}
  
  if(move.strat == "avoid"){
    
    poss.cells.abun = matrix(0, nrow = length(poss.cells), ncol = 2)
    poss.cells.abun[,1] = poss.cells
    
    for(j in 1:length(poss.cells)){
     poss.cells.abun[j,2] = sum(pop[which(pop[,3] == poss.cells[j]),1])
    }
    
    min.abun = min(poss.cells.abun[,2])
    
    poss.cells.min = poss.cells.abun[which(poss.cells.abun[,2] == min.abun),1]
    
    if(length(poss.cells.min) == 0){
      move.loc = sample(poss.cells, 1, replace = FALSE)
    }else{
      if(length(poss.cells.min) == 1){move.loc = poss.cells.min
      }else{move.loc = sample(poss.cells.min, 1, replace = FALSE)}
    }
  }
  
  if(move.strat == "maxden"){
    
    poss.cells.abun = matrix(0, nrow = length(poss.cells), ncol = 2)
    poss.cells.abun[,1] = poss.cells
    
    for(j in 1:length(poss.cells)){
      poss.cells.abun[j,2] = sum(pop[which(pop[,3] == poss.cells[j]),1])
    }
    
    poss.cells.maxden = poss.cells.abun[which(poss.cells.abun[,2] < max.den),1]
    
    if(length(poss.cells.maxden) == 0){
      
      move.loc = sample(poss.cells, 1, replace = FALSE)
      
    }else{
       if(length(poss.cells.maxden) == 1){
         move.loc = poss.cells.maxden
       }else{
         move.loc = sample(poss.cells.maxden, 1, replace = FALSE)}
        }
  }

  move.loc.out[which.move[i]] = move.loc
  
  }#end for loop
  }#end if statement
  
  return(move.loc.out)
  
}