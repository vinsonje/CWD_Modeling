summarizeCWD = function(pop, track.pop = FALSE, thyme){
  
  #####################################
  ######## Track SEI numbers  ######### 
  #####################################
  
  Ssums.temp = sum(pop[, 8])
  Esums.temp = sum(pop[, 9])
  Isums.temp = sum(pop[, 10])
  
  #####################################
  ######## Track SEI numbers  ######### 
  #####################################
  
  if(nrow(pop[pop[, 8] > 0, ,drop = FALSE]) > 0){
    Scells.temp = nrow(pop[pop[, 8] > 0, , drop = FALSE])
  } else{Scells.temp = 0}
  
  if(nrow(pop[pop[, 9] > 0, ,drop = FALSE]) > 0){
    Ecells.temp = nrow(pop[pop[, 9] > 0, , drop = FALSE])
  } else{Ecells.temp = 0}
  
  if(nrow(pop[pop[, 10] > 0, ,drop = FALSE]) > 0){
    Icells.temp = nrow(pop[pop[, 10] > 0, , drop = FALSE])
  } else{Icells.temp = 0}

  #####################################
  ######## Track I locs  ############## 
  #####################################
  if(any(pop[, 10] > 0)){		
    I.locs.temp = rep(pop[pop[, 10] > 0, 3], pop[pop[, 10] > 0, 10])
  } else{
    I.locs.temp = pop[pop[, 10] > 0, 3]
  }
  
  #############################
  ####Track true spatial spread
  #############################
  #if any infected individuals
  if(nrow(pop[pop[, 9, drop = FALSE] > 0 | pop[ , 10, drop = FALSE] > 0, , drop = FALSE]) > 0){
    
    Spread.temp = areaOfinfectionCWD(pop, centroids, inc)
    
  } else{Spread.temp = c(0,0, 0)}
  
  #############################
  ####Summarize total population size
  #############################
  Nall.temp = sum(pop[,1])
  
  #############################
  ####Track total population###
  #############################
  if(track.pop == TRUE){
    pop.temp = cbind(pop, time = thyme)
  }else{pop.temp = NULL}
  
  
  return(list(
    Ssums.temp, Esums.temp, Isums.temp,
    Scells.temp, Ecells.temp, Icells.temp,
    I.locs.temp, 
    Spread.temp,
    Nall.temp, pop.temp))
}