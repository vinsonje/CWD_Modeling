prion.removal = function(landscape.prions, prion.lifespan){
  landscape.prions.out = landscape.prions
  P.rem.prob = 1 - exp(-1/prion.lifespan)
  
  if(sum(landscape.prions.out[,3])>0){
  P.index = which(landscape.prions.out[,3]>0)
  
  for(i in 1:length(P.index)){
    P.rem = rbinom(1, landscape.prions.out[P.index[i],3], P.rem.prob)
    
    landscape.prions.out[P.index[i], 3] = landscape.prions.out[P.index[i], 3] - P.rem
  }
  }
  
  return(landscape.prions.out)
}