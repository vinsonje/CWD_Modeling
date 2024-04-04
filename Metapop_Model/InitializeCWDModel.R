#The purpose of this script is to set all the parameters 
#and initialize all state variables needed to run the CWD metapopulation Model
#Run this before running SimulateOneRun.R

rm(list = ls())
######################
####Set directories
#####################
# setwd("~/Desktop/IBM_files/Metapop Model") #for my Mac
setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Metapop Model") #for my PC

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
#####################
thyme = 70

#grid/landscape parameters
grid.xmax = 20
grid.ymax = 20
cell.x.size = 0.5
cell.y.size = 0.5
density = 6 #density per X
area = grid.xmax * grid.ymax #total area of the grid

#host demographic parameters
N0 = density*area #initial population size
K = N0*1.5 #carrying capacity for whole population
fs = 4
death = 7/(365*3) #assume pop growth rate of 1.5 so make death rate = birth rate*(1/1.5); 1/(365*3); % natural death rate for S and R
mc_time=0.0027
Pbd = 7*mc_time #; %repmat(mean(c_time(1:364/7)),time,1).*1; % constant birth rate for S; rescale trend as needed to produce realistic pop dynamics
shift = c(0.7515,0.3550) #you pulled this from the other file. I think it has something to do with gamma distribution. Maybe mean and sd?
inc = 0.5

shed = 20


######################
####Create grid
######################
grid = create.grid(grid.xmax, grid.ymax, cell.x.size, cell.y.size)

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
####Choose State Rules
#########################
#define movement characteristics of the population
#THIS JUST SET THE PARAMETERS B1, F1, F2, F2i, B2 which were from GLM?
#F2 is a glm model. You need this model to do the predict function in StateChanges.R

# set.seed(2)            # for replicability

n_sim = 10000           # for the initial dataset

xx = runif(n_sim)     # predictor values
coefficients = c(-0.80094,-1.9128) # my assumption
prob = 1/(1 + exp(-(coefficients[1] + coefficients[2] * xx)))

yy = runif(n_sim) < prob

F2 = glm(yy ~ xx, family = "binomial")

F2_int = F2$coef[[1]]
F2_B = F2$coef[[2]]

######################
####Initialize Population
#####################
pop = InitializeFamilies(N0, fs, cells, centroids, 0, 0, 0)
I0 = 1

######################
####RunModel
#####################
for(z in 1:1){
  print("starting new simulation")
sim_output = SimulateOneRunCWD(Pbd, death, shed, #this are parameters realated to population and epi model
                           F1, F2_int, F2_B, B1, B2,
                           thyme, cells,
                           N0, K, #population parameters
                           shift, centroids, inc, fs,
                           gridlen,midpoint,
                           pop, I0)
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


plot.landscape.meta(sim_output, grid.xmax = grid.xmax, grid.ymax = grid.ymax)

