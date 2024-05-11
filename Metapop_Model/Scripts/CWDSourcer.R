#Source File
source(paste(getwd(), "/CreateGrid.R", sep = ''))
source(paste(getwd(), "/InitializeFamilies.R", sep = ''))
source(paste(getwd(), "/FastMovementCWD.R", sep = ''))
source(paste(getwd(), "/FOICWD.R", sep = ''))
source(paste(getwd(), "/SimulateOneRunCWD.R", sep = ''))
source(paste(getwd(), "/StateChangesCWD.R", sep = ''))
source(paste(getwd(), "/areaOfInfectionCWD.R", sep = ''))
source(paste(getwd(), "/GetOutputsCWD.R", sep = ''))
source(paste(getwd(), "/CWD_plots.R", sep = ''))
source(paste(getwd(), "/sharpshootingCWD.R", sep = ''))

Rcpp::sourceCpp("./Movement_Parallel_Functionsmall.cpp", verbose=TRUE)
# Rcpp::sourceCpp("./Fast_FOI_ParallelCWD.cpp", verbose=TRUE)
