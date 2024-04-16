Nall = matrix(nrow = thyme) #track total abundance
BB = matrix(nrow = thyme) #track births

POSlive_locs = as.list(rep(0,thyme))

Spread = matrix(0, nrow = thyme, ncol = 3) #number of infectious individuals, area of infection, max distance between any two cases
Incidence = matrix(0, nrow = thyme) #store new cases for each time step

I_locs = vector("list", thyme)
I_locs[1:thyme] = 0

Itrue = matrix(0, nrow = thyme, ncol = 1)

Isums = matrix(0, nrow = thyme)

out = matrix(c(0, 0, 0), nrow=thyme, ncol=3)

landscape.prions = data.frame(centroids, prions = rep(0, dim(centroids)[1]))
landscape.prions[211, 3] = 34
landscape.prions.out = data.frame(landscape.prions, time = rep(0, dim(landscape.prions)[1]))

#####################################
####### Initialize Infection ########
#####################################
num_inf_0 = I0 #how many individuals to infect starting off

#find the midpoint of the grid
id = which(centroids[, 1] >= midpoint[1] & centroids[, 2] >= midpoint[2])[1] #location on grid closest to midpoint

infected = InitializeFamilies(N0,ss,cells,centroids,num_inf_0,id,1)
infected[,8] = 0
infected[,10] = 1

#combine infected pig with pop matrix
pop = rbind(pop,infected)





F1P = F1
F2P_int = F2_int
F2P_B = F2_B

FOIParallelFullCWD(pop,as.matrix(landscape.prions),centroids,cells,B1,B2,
                   F1,F2_int,F2_B,F1P,F2P_int,F2P_B)
