######################
####Define Parameters
######################
thyme = 12*6

track.pop = TRUE

###########################
#grid/landscape parameters
###########################
grid.xmax = 100
grid.ymax = 100
cell.x.size = 1.0
cell.y.size = 1.0
density = 5 #density per X
area = grid.xmax * grid.ymax #total area of the grid

###########################
#host demographic parameters
###########################
N0 = density*area #initial population size
K = N0 #carrying capacity for whole population
fs = 12 #average family size

lifespan = 12*6
Pbd = 1.5 

###########################
#host relocation parameters
###########################
shift = c(1.0, 0.3550) #shape and scale of the gamma distribution that defines how far they relocate on the landscape
inc = 1.0 #home range size of the population (basically, if they are drawn to relocate lower than this number they don't relocate)
max.den = 100
move.strat = "maxden"

###########################
#Epidemiological parameters
###########################
I0 = 1

shed = 10 #the shedding rate of infected deer (mean of poisson distribution)
lat.period = 3
inf.period = 6

#########################
#Direct Tranmission Parameters
#########################
F1 = 0.7381
B1 = 0.4

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
ss.shooters = 10
ss.radius = 5.0
ss.eff = 0.5
ss.strat = "priority"

######################
#Harvesting parameters
######################
h.permits = 100
h.times = c(10:19, 30:40)
h.radius = 5.0
h.num = 2

######################
#Surveillance parameters
######################
test.rate = 0.95
true.pos.E = 0.85
true.pos.I = 0.90
true.neg = 0.95
sur.start = c(20, 42)