######################
####Define Parameters
######################
thyme = 12*6

track.pop = TRUE

###########################
#grid/landscape parameters
###########################
grid.xmax = 20
grid.ymax = 20
cell.x.size = 1.0
cell.y.size = 1.0
density = 200 #density per X
area = grid.xmax * grid.ymax #total area of the grid

###########################
#host demographic parameters
###########################
N0 = density*area #initial population size
K = N0 #carrying capacity for whole population
fs = 20 #average family size

lifespan = 12*6
Pbd = 2.5
birth.times = seq(6, 72, 12)

###########################
#host relocation parameters
###########################
shift = c(1.0, 0.3550) #shape and scale of the gamma distribution that defines how far they relocate on the landscape
inc = 1.0 #home range size of the population (basically, if they are drawn to relocate lower than this number they don't relocate)
max.den = 100
move.strat = "maxden"

##########################
#host dispersal parameters
##########################
dispersal = 0.05
disp.dist = c(4, 1) 

disp.times = c(1, 2, 4, 11, 12)
# disp.times = c(10:12, 22:24, 34:36,
#                46:48, 58:60, 70:72)

###########################
#Epidemiological parameters
###########################
I0 = 1

shed = 10 #the shedding rate of infected deer (mean of poisson distribution)
lat.period = 16
inf.period = 6

#########################
#Direct Transmission Parameters
#########################
F1 = 0.07381
B1 = 0.04

# F1 = 0
# B1 = 0

##########################
#Prion transmission parameters
########################
B1P.m = -1
B1P.inter = 200

# B1P.m = 0
# B1P.inter = 0

corpse.burst = 300

prion.lifespan = 20

############################
####Define direct contacts
############################
n_sim = 10000           # for the initial dataset

xx = runif(n_sim)*grid.xmax     # predictor values
coefficients = c(0.98, -1.9128) # my assumption
prob = 1/(1 + exp(-(coefficients[1] + coefficients[2] * xx)))

yy = (runif(n_sim) < prob)*1

F2 = glm(yy ~ xx, family = "binomial")

F2_int = F2$coef[[1]]
F2_B = F2$coef[[2]]

#############################
####Define indirect contacts
#############################
xxi = runif(n_sim)*grid.xmax     # predictor values
coefficientsi = c(0.98, -2.0) # my assumption
probi = 1/(1 + exp(-(coefficientsi[1] + coefficientsi[2] * xxi)))

yyi = (runif(n_sim) < probi)*1

F2i = glm(yyi ~ xxi, family = "binomial")

F2i_int = F2i$coef[[1]]
F2i_B = F2i$coef[[2]]


######################
#Sharpshooting parameters
######################
ss.times = c(25, 45)
# ss.times = 0
ss.shooters = 10
ss.radius = 1.0
ss.eff = 0.05
ss.strat = "priority"
ss.laccess = 0.8
ss.laccess.dist = 0.5

######################
#Harvesting parameters
######################
h.permits = 5
# h.times = 0
h.times = c(10:19, 30:40)
h.radius = 2.0
h.num = 3

######################
#Surveillance parameters
######################
test.rate = 0.95
true.pos.E = 0.85
true.pos.I = 0.90
true.neg = 0.95
# sur.start = 0
sur.start = c(20, 42)