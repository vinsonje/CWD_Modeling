##The purpose of this script is to run a single rep of the CWD model

SimulateOneRunCWD = function(Pbd, death, shed,
                         F1, F2_int, F2i_B, B1, B2,
                         thyme, cells, N0, K,
                         shift, centroids, inc, fs,
                         midpoint, pop, I0, 
                         ss.loc, ss.time, ss.radius){

  ###########################################
  ######## Initialize Output Objects ######## 
  ###########################################
  Nall = matrix(nrow = thyme) #track total abundance
  BB = matrix(nrow = thyme) #track births
  
  POSlive_locs = as.list(rep(0,thyme))

  Spread = matrix(0, nrow = thyme, ncol = 3) #number of infectious individuals, area of infection, max distance between any two cases
  Incidence = matrix(0, nrow = thyme) #store new cases for each time step
  
  I_locs = vector("list", thyme)
  I_locs[1:thyme] = NA
  
  Itrue = matrix(0, nrow = thyme, ncol = 1)

  Isums = matrix(0, nrow = thyme)

  out = matrix(c(0, 0, 0), nrow=thyme, ncol=3)
  
  landscape.prions = data.frame(centroids, prions = rep(0, dim(centroids)[1]))
  landscape.prions.out = data.frame(landscape.prions, time = rep(0, dim(landscape.prions)[1]))
  
  ######################################
  ######## Initialize Infection ######## 
  ######################################
  num_inf_0 = I0 #how many individuals to infect starting off
  
  #find the midpoint of the grid
  id = which(centroids[, 1] >= midpoint[1] & centroids[, 2] >= midpoint[2])[1] #location on grid closest to midpoint
  
  infected = InitializeFamilies(N0,ss,cells,centroids,num_inf_0,id,1)
  infected[,8] = 0
  infected[,10] = 1
  
  #combine infected pig with pop matrix
  pop = rbind(pop,infected)
  
  #track first infection in Incidence matrix
  Incidence[1] = num_inf_0
  
  Nall[1] = sum(pop[,1])
  
  pop.out = cbind(pop, rep(1,dim(pop)[1]))
  
  ##################################
  ######## Start simulation ######## 
  ##################################
  #start the timestep loop

  i = 2
  
  for(i in 2:thyme){
    # if (any(pop[, 9, drop=FALSE]!=0|pop[, 10, drop=FALSE]!=0)){
    if (any(pop[, 9, drop=FALSE]>-999|pop[, 10, drop=FALSE]>-999)){
        
      print(i)
      # print(pop)
      
      #####################################
      ######## Track I locations ######## 
      #####################################
      print("tracking I locations")
      if(nrow(pop[pop[, 10] > 0, ,drop = FALSE]) > 0){
        Isums[i] = nrow(pop[pop[, 10] > 0, , drop = FALSE])
      } else{Isums[i] = 0}

      if(any(pop[, 10] > 0)){		
        I_locs[[i]] = rep(pop[pop[, 10] > 0, 3], pop[pop[, 10] > 0, 10])
      } else{
        I_locs[[i]] = pop[pop[, 10] > 0, 3]
      }
  
      ##########################
      ######## Movement ######## 
      ##########################
      print("starting movement")
      pop = FastMovementCWD(pop, centroids, shift, inc)
      
      ###############################
      ######## State Changes ######## 
      ###############################
      print("starting state changes")
      #births, natural deaths, disease state changes (exposure, infection, recovery, death), carcass decay
      st.list = StateChangesCWD(pop, centroids, cells,
                                Pbd,
                                B1, B2, F1, F2_int, F2_B,
                                F1P, F2P_int, F2P_B,
                                K, death,
                                Incidence, BB, i, landscape.prions) 
      pop = st.list[[1]]
      Incidence = st.list[[2]]
      BB = st.list[[3]]
      
      ################################
      ####### Shedding ###############
      ################################
      print("starting shedding")
      Imat = pop[pop[,10] > 0, , drop = FALSE]
      
      if(dim(Imat)[1]>0){
        
      for(k in 1:dim(Imat)[1]){
        
        prions.shed = rpois(Imat[k,9], shed)
        
        land.index = intersect(which(landscape.prions$cent.x == Imat[k,5]), which(landscape.prions$cent.y == Imat[k,6]))
        
        landscape.prions[land.index,]$prions = landscape.prions[land.index, ]$prions + sum(prions.shed)
      }
        
      }#end of if statement to test if there are any Is
      
      ################################
      ####### Removal of prions#######
      ################################
      print("starting prions removal")
      for(l in 1:dim(landscape.prions)[1]){
        landscape.prions$prions[l] = floor(landscape.prions$prions[l] * 0.25) #this should be a parameter in the model
      }
      
      landscape.prions.temp = data.frame(landscape.prions, time = rep(i, dim(landscape.prions)[1]))
      landscape.prions.out = rbind(landscape.prions.out, landscape.prions.temp)
      
      ###############################
      #### Sharpshooting ############
      ###############################
      print("starting sharpshooting")
      pop = sharpshootingCWD(pop, centroids, ss.loc, ss.time, ss.radius, thyme)
      
      #############################
      ####Track true spatial spread
      #############################
      print("starting true spatial spread")
      #if any infected individuals
      if(nrow(pop[pop[, 9, drop = FALSE] > 0 | pop[ , 10, drop = FALSE] > 0, , drop = FALSE]) > 0){
        
        out[i,] = areaOfinfectionCWD(pop, centroids, inc)
        
      } else{out[i,] = c(0,0, 0)}
      
      #############################
      ####Summarize infections
      #############################
      print("summarizing infections")
      #sum all infectious cases (I,C,E) at each timestep
      #Itrue = sum(I + C,2); sum of all infectious cases over time
      if(i==1){Itrue[i] = num_inf_0}
     else{
       Itrue[i] = sum(colSums(pop)[c(9, 10)])
      }

      #############################
      ####Summarize total population size
      #############################
      print("summarizing total population size")
      Nall[i] = sum(pop[,1])
      pop.temp = cbind(pop, time = i)
      pop.out = rbind(pop.out, pop.temp)
      
      #comment brackets below for manual testing
    }else{print("Exiting loop, no infections")} #if any infected closing bracket/else
    
  } #for timestep closing bracket
  
  #############################
  #############################
  
  
  print("calculating outputs")
  out.list = GetOutputsCWD(pop, Incidence, out,
                       I_locs, 
                       POSlive_locs, Isums,
                       landscape.prions.out, Nall,
                       pop.out)

  return(out.list)

} #function closing bracket



