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
  "death",
  "mc_time",
  "Pbd"
)

HMove.parms.names = c(
  "Shift",
  "inc",
  "max.den",
  "move.strat"
)

HContact.parms.names = c(
  "F2_int",
  "F2_B"
)

HEpiDir.parms.names = c(
  "B1",
  "F1"
)

HEpiEnv.parms.names = c(
  "B1P.m",
  "B1P.inter",
  "shed",
  "corpse.burst"
)

Harvest.parms.names = c(
  "h.loc",
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

SS.parms.names = c("ss.loc", 
                   "ss.time", 
                   "ss.radius", 
                   "ss.eff")

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
  "death",
  "mc_time",
  "Pbd"
)

HMove.parms.names = c(
  "Shift",
  "inc",
  "max.den",
  "move.strat"
)

HContact.parms.names = c(
  "F2_int",
  "F2_B"
)

HEpiDir.parms.names = c(
  "B1",
  "F1"
)

HEpiEnv.parms.names = c(
  "B1P.m",
  "B1P.inter",
  "shed",
  "corpse.burst"
)

Harvest.parms.names = c(
  "h.loc",
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

SS.parms.names = c("ss.loc", 
                   "ss.time", 
                   "ss.radius", 
                   "ss.eff")

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
  "number of individuals on landscape",
  "carrying capacity (max number) of individuals",
  "average family size",
  "death rate of individuals",
  "mc_time",
  "birth rate"
)

HMove.parms.mean = c(
  "shape and scale of gamma distribution that defines how far individuals relocate on the landscape",
  "homerange size",
  "maximum density per cell",
  "momenent strategy: do they avoid others or not"
)

HContact.parms.mean = c(
  "intercept of glm describing the contact probabilty given distance between individuals",
  "slope of glm describing the contact probability given distance between individuals"
)

HEpiDir.parms.mean = c(
  "tranmision probability given contact for individauls in different cells",
  "transmission probability "
)

HEpiEnv.parms.mean = c(
  "slope of linear portion of logistic function describing the probability of successful transmission given amount of prions in cell",
  "intercept of linear portion of logistic function describing the probability of successful transmission given amount of prions in cell",
  "average number of prions an infectious individuals sheds per timestep (Poisson)",
  "average number of prions a dead infectious individiual in a single release (Poisson)"
)

Harvest.parms.mean = c(
  "grid locations (cell id) that harvesting events occur",
  "radius that a harvesting event affects",
  "time that a harvesting event occurs",
  "number of individuals that "
)

Surveil.parms.mean = c(
  "proportion of individuals that are sent off for CWD testing",
  "true positive rate for Exposed individuals",
  "true positive rate for Infectious individuals",
  "true negative rate for Susceptible individuals", 
  "time when surveillence (when do we want to decide where to sharpshoot)"
)

SS.parms.mean = c(
  "grid locations (cell id) that sharpshooting occurs", 
  "when sharpshooting events occur", 
  "radius that sharpshooting event affects", 
  "efficiency (proportion of individuals) that sharpshooting removes"
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