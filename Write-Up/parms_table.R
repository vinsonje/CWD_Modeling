#Parameter Table Data
#################################################################
#Names
#################################################################

LS.parms.names = c(
  "thyme",
  "grid.xmax",
  "grid.ymax",
  "cell.x.size",
  "cell.y.size"
)

HDemo.parms.names = c(
  "density",
  "N0",
  "K",
  "fs",
  "lifespan",
  "Pbd"
)

HMove.parms.names = c(
  "Shift",
  "inc",
  "max.den",
  "move.strat"
)

HContact.parms.names = c(
  "F2",
  "F2i"
)

HEpiDir.parms.names = c(
  "B1",
  "F1"
)

HEpiEnv.parms.names = c(
  "B1P.m",
  "B1P.inter",
  "shed",
  "corpse.burst",
  "lifespan_P"
)

Harvest.parms.names = c(
  "h.permits",
  "h.radius",
  "h.time",
  "h.num"
)

Surveil.parms.names = c(
  "test.rate",
  "true.pos.E",
  "true.pos.I",
  "true.neg", 
  "sur.start"
)

SS.parms.names = c(
  "ss.time", 
  "ss.shooters",
  "ss.radius", 
  "ss.eff",
  "ss.strat"
  )

#####################################################################
#Meaning
#####################################################################

LS.parms.mean = c(
  "max time for simulation to run",
  "max grid size in X dim",
  "max grid size in Y dim",
  "cell size in X dim",
  "cell size in Y dim"
)

HDemo.parms.mean = c(
  "number of individuals per unit area",
  "(roughly) the initial number of individuals on landscape",
  "carrying capacity (max number) of individuals",
  "average family size",
  "lifespan of individiuals",
  "birth rate"
)

HMove.parms.mean = c(
  "shape and scale of gamma distribution that defines how far individuals relocate on the landscape",
  "homerange size",
  "maximum density per cell",
  "momenent strategy: do they avoid others or not; random, maxden, avoid"
)

HContact.parms.mean = c(
  "glm object for fitting direct contact probabilities",
  "glm object for fitting indirect contact probabilities"
)

HEpiDir.parms.mean = c(
  "transmission probability given contact for individauls in different cells",
  "transmission probability given contact within same cell"
)

HEpiEnv.parms.mean = c(
  "slope of linear portion of logistic function describing the probability of successful transmission given amount of prions in cell",
  "intercept of linear portion of logistic function describing the probability of successful transmission given amount of prions in cell",
  "average number of prions an infectious individuals sheds per timestep (Poisson)",
  "average number of prions a dead infectious individiual in a single release (Poisson)",
  "lifespan of prions in the environment"
)

Harvest.parms.mean = c(
  "number of permits issued per time step",
  "radius that a harvesting event affects",
  "times that a harvesting event occurs",
  "number of individuals removed per permit"
)

Surveil.parms.mean = c(
  "proportion of individuals that are sent off for CWD testing",
  "true positive rate for Exposed individuals",
  "true positive rate for Infectious individuals",
  "true negative rate for Susceptible individuals", 
  "time when surveillence (when do we want to decide where to sharpshoot)"
)

SS.parms.mean = c(
  "times when sharpshooting events occur", 
  "number of shooters that are out per time step",
  "radius that sharpshooting event affects", 
  "efficiency (proportion of individuals) that sharpshooting removes",
  "sharpshooting strategy; random: randomly choose where to go shootshoot from surveillance locations, 
    priority: go to locations that have the highest number of positive detections"
)


###########################################################
#Values
###########################################################
LS.parms.vals = c(
  thyme,
  grid.xmax,
  grid.ymax,
  cell.x.size,
  cell.y.size
)

HDemo.parms.vals = c(
  density,
  N0,
  K,
  fs,
  lifespan,
  Pbd
)

HMove.parms.vals = c(
  paste(shift, collapse = " "),
  inc,
  max.den,
  move.strat
)

HContact.parms.vals = c(
  "estimated from fake data",
  "estimated from fake data"
)

HEpiDir.parms.vals = c(
  B1,
  F1
)

HEpiEnv.parms.vals = c(
  B1P.m,
  B1P.inter,
  shed,
  corpse.burst, 
  prion.lifespan
)

Harvest.parms.vals = c(
  h.permits,
  h.radius,
  paste(h.times, collapse = " "),
  h.num
)

Surveil.parms.vals = c(
  test.rate,
  true.pos.E,
  true.pos.I,
  true.neg, 
  paste(sur.start, collapse = " ")
)

SS.parms.vals = c(
  paste(ss.times, collapse = " "), 
  ss.shooters,
  ss.radius, 
  ss.eff,
  ss.strat
)



###########################################################
#Combine them all
###########################################################
all.parm.names = c(
  LS.parms.names, HDemo.parms.names, HContact.parms.names, 
  HEpiDir.parms.names, HEpiEnv.parms.names, HMove.parms.names,
  Harvest.parms.names, Surveil.parms.names, SS.parms.names
)

all.parm.mean = c(
  LS.parms.mean, HDemo.parms.mean, HContact.parms.mean, 
  HEpiDir.parms.mean, HEpiEnv.parms.mean, HMove.parms.mean,
  Harvest.parms.mean, Surveil.parms.mean, SS.parms.mean
)

all.parm.vals = c(
  LS.parms.vals, HDemo.parms.vals, HContact.parms.vals, 
  HEpiDir.parms.vals, HEpiEnv.parms.vals, HMove.parms.vals,
  Harvest.parms.vals, Surveil.parms.vals, SS.parms.vals
)