#The purpose of this script is to set all the parameters 
#and initialize all state variables needed to run the CWD metapopulation Model
#It will also do one run of the simulation using SimulateOneRun.R
#Run this before running SimulateOneRun.R

######################
####Clear the environment
#####################
rm(list = ls())

######################
####Set directories
#####################
# setwd("~/Desktop/IBM_files/Metapop Model") #for my Mac
setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Metapop_Model") #for my PC

######################
####Set libraries
#####################
library(Rcpp)
library(profvis)
library(R.matlab)
library(pracma)
library(rdist)
library(tidyverse)
library(microbenchmark)
library(RcppArmadillo)
library(RcppParallel)

######################
####Source Functions
#####################
#running this will load in all functions needed for the CWD Metapopulation model
source(paste(getwd(), "/CWDSourcer.R", sep = ''))


######################
####Define Parameters
###################
thyme = 70

#grid/landscape parameters
grid.xmax = 10
grid.ymax = 10
cell.x.size = 0.5
cell.y.size = 0.5
density = 10 #density per X
area = grid.xmax * grid.ymax #total area of the grid

#host demographic parameters
N0 = density*area #initial population size
K = floor(N0*1.5) #carrying capacity for whole population
fs = 12 #average family size

death = 7/(365*3) #assume pop growth rate of 1.5 so make death rate = birth rate*(1/1.5); 1/(365*3); % natural death rate for S and R
mc_time=0.0027 #this is just a rounding of 1/365
Pbd = 7*mc_time #; %repmat(mean(c_time(1:364/7)),time,1).*1; % constant birth rate for S; rescale trend as needed to produce realistic pop dynamics

#host relocation parameters
shift = c(2.0,0.3550) #shape and scale of the gamma distribution that defines how far they relocate on the landscape
inc = 0.5 #home range size of the population (basically, if they are drawn to relocate lower than this number they don't relocate)

#disease parameters
shed = 20 #the shedding rate of infected deer (mean of poisson distribution)


######################
####Create grid
######################
grid = create.grid(grid.xmax, grid.ymax, cell.x.size, cell.y.size)
# grid = readMat("Grid_80x80_0pt4km.mat")$grid

centroids = grid[,6:7]
cells = nrow(grid)

midpoint = c(max(centroids[,1]/2),max(centroids[,2]/2))

#########################
####Define Contact Rules
#########################
F1 = 0.7381

B1 = 0.4
B2 = 0.5*0.2

#########################
####Movement model for population
#########################
#define movement characteristics of the population
#THIS JUST SET THE PARAMETERS B1, F1, F2, F2i, B2 which were from GLM?
#F2 is a glm model. You need this model to do the predict function in StateChanges.R

# set.seed(2)            # for replicability

n_sim = 10000           # for the initial dataset

xx = runif(n_sim)     # predictor values
coefficients = c(0.98, 1.9128) # my assumption
prob = 1/(1 + exp(-(coefficients[1] + coefficients[2] * xx)))

yy = runif(n_sim) < prob

F2 = glm(yy ~ xx, family = "binomial")

F2_int = F2$coef[[1]]
F2_B = F2$coef[[2]]

##########################
#Prion transmission parameters
########################
F1P = F1
F2P_int = F2_int
F2P_B = F2_B

######################
####Initialize Population
#####################
pop = InitializeFamilies(N0, fs, cells, centroids, 0, 0, 0)
I0 = 1


######################
#Sharpshooting parameters
######################
ss.locs = c(12, 20)
ss.times = c(30, 60)
ss.radius = 5.0

all = matrix(nrow = thyme) #track total abundance
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

pop.out = cbind(pop, rep(1,dim(pop)[1]))

Pse = FOIParallelFullCWD(pop, as.matrix(landscape.prions), centroids, cells, B1, B2, F1, F2_int, F2_B, F1P, F2P_int, F2P_B) #C++ version
