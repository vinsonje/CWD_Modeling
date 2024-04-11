GetOutputsCWD = function(pop, Incidence, out, 
										 I_locs, POSlive_locs, 
										 Isums, landscape.prions, Nall,
										 pop.out){
	
#List of outputs created here:	
	#Tinc #sum of all exposures over simulation 
	#sum(Tculled)
	#idT #last day there is an infectious individual
	#Mspread #max spread of infection
	#I_locs
	#C_locs
	#POSlive_locs
	#POSdead_locs


#Tinc, this just sums all of the exposures
Tinc = sum(Incidence)

#Find last day there was an infectious individual
idT = which(Isums != 0)[length(which(Isums != 0))]

#Find max spread of infection
Mspread = max(out[ ,2])

#ICatEnd #number of I,C,E on last day
IatEnd = Isums[idT]

#DET #total number of detections
DET = sum(unlist(POSlive_locs))

#total population
total.pop = data.frame(time = 1:length(Nall), pop = Nall)

pop.out = as.data.frame(pop.out)
names(pop.out) = c("fam.size", "dis.status", "grid.loc", "move.dis", "x.now", "y.now", "prev.loc", "S.num", "E.num", "I.num", "Z.num", "time")

#send to list
list.all = list("Tinc" = Tinc,
							"idT" = idT,
							"Mspread" = Mspread,
							"IatEnd" = IatEnd,
							"DET" = DET,
							"I_locs" = I_locs,
							"POSlive_locs" = POSlive_locs,
							"Isums" = Isums, 
							"land.prions" = landscape.prions,
							"tot.abun" = total.pop,
							"pop.out" = pop.out)

return(list.all)

}

#output is a list with 
# [1] Total infected/incidence
# [2] Time of last infected
# [3] #Maximum distance that it spread
# [4] Number of infected at end
# [5] Total number of detections
# [6] Locations of Infected across time series
# [7] Locations of positive IDs
# [8] Total number of infected at each time point