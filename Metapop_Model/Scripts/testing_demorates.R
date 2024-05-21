gamma = 1/100

P.test = 1-exp(-gamma)

# plot(gamma, P.test)

time = 1000
samples = 10000
E.vec = sample(75:100, samples, replace = T)
E.vec.out = matrix(0, nrow = samples, ncol = time+1,)
E.vec.out[,1] = E.vec

E.out = matrix(0, nrow = samples, ncol = time)

for(i in 1:time){
  for(j in 1:length(E.vec)){
  E.out[j,i] = rbinom(1, E.vec[j], P.test)
  
  E.vec[j] = E.vec[j] - E.out[j,i]
  }
  E.vec.out[,i+1] = E.vec
}

out.prop = matrix(0, nrow = samples, ncol = time)
w.prop = matrix(0, nrow = samples, ncol = time)

for(k in 1:time){
out.prop[,k] = E.out[,k]/E.vec.out[,1]
w.prop[,k] = out.prop[,k]*k
}

mean.time = rowSums(w.prop)

hist(mean.time)
