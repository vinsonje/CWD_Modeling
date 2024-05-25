########################################
#Surveillance function
########################################
#The purpose of this function is to simulate
#surveillance of CWD from harvested individuals. 
#It has inputs of:
#harvest.data = dataframe of the harvested individuals
#true.pos = parameter for the true positive accuracy
#true.neg = parameter for the true negative accuracy
#sur.start = time that the surveillance starts
#########################################

surveillance.fun = function(harvest.data, test.rate, true.pos.E, true.pos.I, true.neg, sur.start, thyme){
  
  if(any(harvest.data[,1] == 0)){harvest.data = harvest.data[-which(harvest.data[,1]==0), , drop = FALSE]}
  
  SS.locs = NULL
  harvest.data.out = harvest.data
  surv.data.all = NULL
  
  if(thyme %in% sur.start){
    #testing
    num.classes = colSums(harvest.data[,3:5])
    
    deer.in.samp = rep(c("S", "E", "I"), num.classes)
    
    S.in.samp = rep(harvest.data[,2], harvest.data[,3])
    E.in.samp = rep(harvest.data[,2], harvest.data[,4])
    I.in.samp = rep(harvest.data[,2], harvest.data[,5])
    
    num.tested = ceiling(test.rate*sum(num.classes))
    tested.mems = sample(deer.in.samp, num.tested, replace = FALSE)
    
    S.test = length(which(tested.mems == "S"))
    E.test= length(which(tested.mems == "E"))
    I.test = length(which(tested.mems == "I"))
    
    S.test.loc = sample(S.in.samp, S.test, replace = FALSE)
    E.test.loc = sample(E.in.samp, E.test, replace = FALSE)
    I.test.loc = sample(I.in.samp, I.test, replace = FALSE)
    
    S.test.neg = rbinom(1, S.test, true.neg)
    S.false.pos = S.test - S.test.neg
    
    E.test.pos = rbinom(1, E.test, true.pos.E)
    E.false.neg = E.test - E.test.pos 
    
    I.test.pos = rbinom(1, I.test, true.pos.I)
    I.false.neg = I.test - I.test.pos 
    
    S.FP.loc = sample(S.test.loc, S.false.pos, replace = FALSE)
    E.TP.loc = sample(E.test.loc, E.test.pos, replace = FALSE)
    I.TP.loc = sample(I.test.loc, I.test.pos, replace = FALSE)
    
    if(length(S.FP.loc)<1){S.FP.loc = 0}
    if(length(E.TP.loc)<1){E.TP.loc = 0}
    if(length(I.TP.loc)<1){I.TP.loc = 0}
    
    surv.data.all = data.frame(table(rbind(data.frame(loc = S.FP.loc, result = "FP"), 
                                      data.frame(loc = E.TP.loc, result = "ETP"), 
                                      data.frame(loc = I.TP.loc, result = "ITP"))))
    
    if(any(surv.data.all[,1] == 0)){surv.data.all = surv.data.all[-which(surv.data.all[,1] == 0),]}
    if(any(surv.data.all[,3] == 0)){surv.data.all = surv.data.all[-which(surv.data.all[,3] == 0),]}
    names(surv.data.all) = c("loc", "result", "num")
    
    harvest.data.out = matrix(0, ncol = 5)
}#end if statement for starting surv.
  
  return(list(surv.data.all, harvest.data.out))
  
}#end of function