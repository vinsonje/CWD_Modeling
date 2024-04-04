############Generating Fake F2 GLM. This is used for contact probabilities I believe. 
#This is needed for the non-C++ FOI function. 

set.seed(101)            # for replicability
n_sim <- 10000           # for the initial dataset
xx <- runif(n_sim)     # predictor values
coefficients <- c(-0.80094,-1.9128) # my assumption
prob <- 1/(1+exp(-(coefficients[1]+coefficients[2]*xx)))

yy <- runif(n_sim)<prob

F2 <- glm(yy~xx,family="binomial")