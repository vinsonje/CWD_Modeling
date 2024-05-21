#outputs: pop,Incidence,BB
StateChangesCWD = function(pop, centroids, cells,
                      Pbd,
                      B1, F1, F2,
                      B1P.m, B1P.inter,
                      K, death, lat.period, inf.period,
                      Incidence, BB, i, 
                      landscape.prions){
  ####################################################################
  ########### Initialize state change probability matrices ########### 
  ####################################################################
  
  #births
  Sdpb = matrix(nrow = nrow(pop), ncol=1)
  Sdpb[, 1] = 0
  
  #natural deaths
  Sdpd = matrix(nrow = nrow(pop), ncol=1)
  Edpd = matrix(nrow = nrow(pop), ncol=1)
  Idpd = matrix(nrow = nrow(pop), ncol=1)
  Sdpd[, 1] = 0
  Edpd[, 1] = 0
  Idpd[, 1] = 0 

  #disease state change recording
  Eep = matrix(nrow = nrow(pop), ncol=1)
  Eep[, 1] = 0
  Iep = matrix(nrow = nrow(pop), ncol=1)
  Iep[, 1] = 0

  #dead recording
  Zdpd = matrix(nrow = nrow(pop), ncol=1)
  Zdpd[, 1] = 0
  
  ########################################
  ######## Determine Death Prob ########## 
  ########################################  
  Pdeath = 1 - exp(-death)
  
  ########################################
  ########### Determine Births ########### 
  ########################################
  
  #subset family sets with live individuals
  # idN = pop[pop[, 8, drop = FALSE] > 0 | pop[, 9, drop = FALSE] > 0 | pop[, 10, drop=FALSE] > 0 | pop[, 11, drop=FALSE] > 0, ]
  
  #Number of live individuals
  liveind = sum(colSums(pop)[8:10])
  
  #get row indices of live individuals
  liverows = which(pop[, 8,drop = FALSE] > 0 | pop[, 9, drop = FALSE] > 0 | pop[, 10, drop = FALSE] > 0) #rownums with live indiv
  
  #density-dependent birth rate
  Brate = max(Pbd*liveind*(1-liveind/K),  0)
  
  #get total births, using Brate as mean in a poisson
  Tbirths = rpois(1, Brate)
  
  #record total births this time step
  BB[i] = Tbirths
  
  #Pick out enough numbers that sum to Tbirths - this will determine how many cells get births
  id=0
  n=1
  while (sum(id) < Tbirths) {
    birthset_i = round(runif(1, min = 0, max = min(Tbirths,10)))
    id[n] = birthset_i
    n = n+1
  }
  
  #if there are some births
  if(length(id) > 1){
    
    #if there are more live families than births needed	
    if(length(id) <= nrow(pop)){
      
      #pick which cells with individuals will get the births
      id2 = sample(1:nrow(pop), length(id))
      
    } else {
      #%if there are more births than cells only add births cells where the individuals are (so fewer births will be happening)
      id2 = sample(1:length(liverows), length(liverows))
    }
  
  #this here assigns the new individuals to families and returns the vector that should be added to the pop df later. 
  for(j in 1:length(id)){
    Sdpb[id2[j], 1] = id[j]
  }
  }
  
  ##############################################################
  ######## Determine disease state change probabilities ######## 
  ##############################################################
  
  Pse = FOICWD(pop, centroids, cells,
               B1, F1, F2,
               B1P.m, B1P.inter,
               landscape.prions) #force of infection #R version
  #Pse is the probability of becoming infected for each S individual in each family; 
  #it is organized as a vector with the the probability of each cell on the landscape

  Pei = 1 - exp(-lat.period) #transitions exposure to infected
  Pid = 1 - exp(-inf.period) #transitions infected to either dead 
  
  
  ###############################################
  ######## Conduct the State Transitions ######## 
  ###############################################
  
  for(k in 1:nrow(pop)){
    
    #operations on Susceptible individuals
    if(pop[k, 8]>0){

      Sdpd[k] = sum(rbinom(pop[k, 8], 1, Pdeath)) #determine how many S die
      Eep[k] = sum(rbinom(pop[k, 8], 1, Pse[pop[k, 3]])) #Exposure (S -> E) infection based on probability using their location
      
    }	
    
    #operations on Exposed individuals
    if(pop[k,9]>0){ 
      
      Edpd[k] = sum(rbinom(pop[k,9], 1, Pdeath)) #determine how many E die
      Iep[k] = sum(rbinom(pop[k,9], 1, Pei)) #determine how many transition to I
      
    }
    
    #operations on Infected individuals	
    if(pop[k, 10] > 0){
      Idpd[k] = sum(rbinom(pop[k, 10], 1, Pdeath)) #determine how many I die to natural causes
      Zdpd[k] = sum(rbinom(pop[k, 10], 1, Pid)) #determine how many die from infection
     }	
    

  }
  
  Incidence[i] = Incidence[i]+sum(Eep)
  
  
  ###################################
  ######## Update pop matrix ######## 
  ###################################
    #update states in pop matrix
  ###################################
  pop[, 8] = pop[, 8] - Eep + Sdpb - Sdpd #Snew = Sold - S->E + births - deaths
  pop[, 9] = pop[, 9] - Iep + Eep - Edpd #Enew = Eold - E->I + S->E - deaths
  pop[, 10] = pop[, 10] - Zdpd + Iep - Idpd #Inew = Iold - death via inf. + E->I - deaths
  pop[, 11] = pop[, 11] + Zdpd  #Znew = Zold + I deaths
  #this last one I think I should just have as the new dead from infection to do the corpse prion burst. 
  
  #sometimes end up with negative numbers 
  #(i.e. all individuals in families chosen for natural mort and disease mort)
  #just set anything below zero to zero
  pop[which(pop[, 8]<0), 8] = 0
  pop[which(pop[, 9]<0), 9] = 0
  pop[which(pop[, 10]<0), 10] = 0
  pop[which(pop[, 11]<0), 11] = 0
  
  #move dead individuals (Z) into their own rows
  deadguys = pop[pop[, 11] > 0, , drop = FALSE]
  
  #if there are deadguys....
  if(dim(deadguys)[1] != 0){
    #remove abundance and all live guy counts from deadguy set
    deadguys[, 1] = 0
    deadguys[, 8] = 0
    deadguys[, 9] = 0
    deadguys[, 10] = 0
    deadguys[, 11] = 0
    
    #set all deadguys in pop rows to zero
    pop[which(pop[, 11] > 0), 11] = 0
    
    #add deadguys to pop matrix
    pop = rbind(pop, deadguys)
    
  }
  
  #Update abundance numbers (live individuals only count in abundance)
  pop[, 1] = rowSums(pop[, 8:10])
  
  #remove rows that don't have any pigs
  pop = pop[which(rowSums(pop[, 8:11]) != 0), ]
  
  return(list(pop, Incidence, BB))

}
