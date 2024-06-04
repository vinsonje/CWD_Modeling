##The purpose of this script is to run a single rep of the CWD model

SimulateOneRunCWD = function(pop, landscape.prions, centroids, track.pop = TRUE){
  
  #load in parmeters
  source(paste(getwd(), "/CWDParms.R", sep = ''))
  
  #determine cells
  cells = dim(centroids)[1]
  
  ###########################################
  ####### Initialize Tracked Objects ########
  ###########################################
  
  #track total abundance
  Nall = matrix(0, nrow = thyme) 
  Nall[1] = sum(pop[,1])
  
  #track births
  BB = matrix(0, nrow = thyme) 
  
  #number of infected cells, area of infection, max distance between any two cases
  Spread = matrix(0, nrow = thyme, ncol = 3) 
  
  #store new cases for each time step
  Incidence = matrix(0, nrow = thyme) 
  Incidence[1] = I0
  
  #track the locations of cells with I
  I_locs = vector("list", thyme)
  I_locs[1:thyme] = NA
  
  #track the number of SEI individuals
  Ssums = matrix(0, nrow = thyme, ncol = 1)
  Esums = matrix(0, nrow = thyme, ncol = 1)
  Isums = matrix(0, nrow = thyme)
  
  #track the number of cells with SEI individuals
  Scells = matrix(0, nrow = thyme, ncol = 1)
  Ecells = matrix(0, nrow = thyme, ncol = 1)
  Icells = matrix(0, nrow = thyme)
  
  #initialize the prions
  landscape.prions.out = data.frame(landscape.prions, time = rep(0, dim(landscape.prions)[1]))
  
  #harvest
  harvest.out = matrix(0, ncol = 5)
  harvest.yearly = matrix(0, ncol = 5)
  
  #total pop data
  pop.out = cbind(pop, time = 0)
  
  #surveillence data
  surv.out = NULL
  surv.yearly = NULL
 
  #SS data
  SS.out = NULL
  
  #dispersal data
  disp.data.out = NULL 
  
  ##################################
  ######## Start simulation ######## 
  ##################################
  
  #start the timestep loop
  for(i in 1:thyme){
    # if (any(pop[, 9, drop=FALSE]!=0|pop[, 10, drop=FALSE]!=0)){
    if (any(pop[, 9, drop=FALSE]>-999|pop[, 10, drop=FALSE]>-999|pop[, 8, drop=FALSE]>-999)){
        
      print(paste("timestep:",i, sep = " "))
  
      ##########################
      ######## Movement ######## 
      ##########################
      print("starting movement")
      pop = FastMovementCWD(pop, centroids, shift, inc, max.den, move.strat)
      
      ##########################
      ##### Dispersal ##########
      ##########################
      print("starting dispersal")
      dispersal.out = dispersalCWD(pop, dispersal, disp.dist, disp.times, i)
      pop = dispersal.out[[1]]
      disp.data.out = rbind(disp.data.out, dispersal.out[[2]])
      
      ###############################
      ######## State Changes ######## 
      ###############################
      print("starting state changes")
      #births, natural deaths, disease state changes (exposure, infection, recovery, death), carcass decay
      st.list = StateChangesCWD(pop, centroids, cells,
                                Pbd, birth.times,
                                B1, F1, F2,
                                B1P.m, B1P.inter, F2i,
                                K, lifespan, lat.period, inf.period,
                                Incidence, BB, i, 
                                landscape.prions,
                                i) 
      pop = st.list[[1]]
      Incidence = st.list[[2]]
      BB = st.list[[3]]
      
      ################################
      ####### Shedding ###############
      ################################
      print("starting shedding")
      landscape.prions = shedCWD(pop, landscape.prions, shed)

      ################################
      ####### Corpse Burst ###########
      ################################
      print("starting corpse burst")
      corpse.burst.out = corpse_burstCWD(pop, landscape.prions, corpse.burst)
      pop = corpse.burst.out[[1]]
      landscape.prions = corpse.burst.out[[2]]
      
      
      ################################
      ####### Removal of prions #######
      ################################
      print("starting prions removal")
      landscape.prions = prion.removal(landscape.prions, prion.lifespan)
      
      ##############################
      ######### Harvesting #########
      ##############################
      print("starting harvesting")
      harvest.event.out = harvestingCWD(pop, centroids, h.permits, h.times, h.radius, h.num, i)
      pop = harvest.event.out[[1]]
      harvest.out = rbind(harvest.out, harvest.event.out[[2]])
      harvest.yearly = rbind(harvest.yearly, harvest.event.out[[2]])
      
      ##############################
      ######### Surveillance #######
      ##############################
      print("starting surveillance")
      surv.event.out = surveillance.fun(harvest.yearly, test.rate, true.pos.E, true.pos.I, true.neg, sur.start, i)
      
      surv.yearly = rbind(surv.yearly, surv.event.out[[1]])
      harvest.yearly = surv.event.out[[2]]
      if(is.null(surv.event.out[[1]]) == FALSE){surv.out = rbind(surv.out, cbind(surv.event.out[[1]], time = i))}
    
      ###############################
      #### Sharpshooting ############
      ###############################
      print("starting sharpshooting")
      SS.event.out = sharpshootingCWD(pop, centroids, surv.yearly, ss.shooters, ss.times, ss.radius, ss.eff, ss.strat, 
                                      ss.laccess, ss.laccess.dist, i)
      
      pop = SS.event.out[[1]]
      surv.yearly = SS.event.out[[2]]
      SS.out = rbind(SS.out, SS.event.out[[3]])
      
      ################################
      ###########Summarize############
      ################################
      print("Summarizing")
      summary.pop = summarizeCWD(pop, track.pop, i)

      Ssums[i,] = summary.pop[[1]]
      Esums[i,] = summary.pop[[2]]
      Isums[i,] = summary.pop[[3]]
      Scells[i,] = summary.pop[[4]]
      Ecells[i,] = summary.pop[[5]]
      Icells[i,] = summary.pop[[6]]
      
      I_locs[[i]] = summary.pop[[7]]
      
      Spread[i,] = summary.pop[[8]]
      
      Nall[i,] = summary.pop[[9]]
      pop.out = rbind(pop.out, summary.pop[[10]])
      
      landscape.prions.temp = data.frame(landscape.prions, time = rep(i, dim(landscape.prions)[1]))
      landscape.prions.out = rbind(landscape.prions.out, landscape.prions.temp)
      
    }else{print("Exiting loop, no infections")} #if any infected closing bracket/else
    
  } #for timestep closing bracket
  
    #############################
    #############################
  harvest.out = harvest.out[-which(harvest.out[,1] == 0), , drop = FALSE]
  
  pop.out = as.data.frame(pop.out)
  names(pop.out) = c("fam.size", "dis.status", "grid.loc", "move.dis", "x.now", "y.now", 
                     "prev.loc", "S.num", "E.num", "I.num", "Z.num", "fam.id", "time")
  
  out.list = list(Nall, 
           Ssums, Esums, Isums,
           Scells, Ecells, Icells,
           I_locs, Spread,
           BB, Incidence,
           harvest.out, surv.out, SS.out,
           pop.out, landscape.prions.out,
           disp.data.out)

  return(out.list)

} #function closing bracket

#[[1]] Total pop size vec
#[[2]] S num vec
#[[3]] E num vec
#[[4]] I num vec
#[[5]] S cell num vec
#[[6]] E cell num vec
#[[7]] I cell num vec
#[[8]] I locs list
#[[9]] Spread matrix
#[[10]] Births vec
#[[11]] Incidence vec
#[[12]] Harvest df
#[[13]] Surveillance df
#[[14]] Sharpshooting df
#[[15]] Pop df (if track.pop == TRUE, all timesteps)
#[[16]] landscape prions df
#[[17]] dispersal data




