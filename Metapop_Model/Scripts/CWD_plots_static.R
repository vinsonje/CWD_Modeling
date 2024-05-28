#################################
#CWD staic plots
#################################

CWD.plots.static = function(sim_output, grid.xmax, grid.ymax, thyme){
  
  N.plot = ggplot() + geom_line(aes(x = 1:thyme, sim_output[[1]]), lwd = 2.0) + theme_cowplot() +
    xlab("time") + ylab("total population size")
  
  SEI.plot = ggplot() + geom_line(aes(x = 1:thyme, sim_output[[2]]/sim_output[[1]]), color = "black", lwd = 2.0) +
    geom_line(aes(x = 1:thyme, sim_output[[3]]/sim_output[[1]]), color = "darkgreen", lwd = 2.0) +
    geom_line(aes(x = 1:thyme, sim_output[[4]]/sim_output[[1]]), color = "firebrick", lwd = 2.0) +
    theme_cowplot() + xlab("time") + ylab("proportion of population")
  
  spread.plot = ggplot() + geom_line(aes(x = 1:thyme, sim_output[[9]][,1]), color = "black", lwd = 2.0) + 
    theme_cowplot() + xlab("time") + ylab("spread (KM)")
  
  incidence.plot = ggplot() + geom_line(aes(x = 1:thyme, sim_output[[11]]), color = "black", lwd = 2.0) + 
    theme_cowplot() + xlab("time") + ylab("incidence")
  
  harvest.data = sim_output[[12]]
  harvest.dates = unique(harvest.data[,1])
  h.out = matrix(nrow = length(harvest.dates), ncol = 5)
  for(i in 1:length(harvest.dates)){
    h.sub = subset(harvest.data, harvest.data[,1] == harvest.dates[i])
    h.colsums = colSums(h.sub[,3:5])
    h.sum = sum(h.sub[,3:5])
    h.out[i,] = c(harvest.dates[i], h.colsums, h.sum)
  }
  h.out = as.data.frame(h.out)
  names(h.out) = c("time", "S", "E", "I", "N")
  h.tall = gather(h.out, key = "status", value = "number", S:I)
  
  harvest.plot = ggplot(h.tall) + geom_col(aes(x = time, y = number, fill = status)) + 
    theme_cowplot() + xlim(0, thyme) + 
    ylab("number harvested")

  surv.data = sim_output[[13]]
  surv.counts = aggregate(surv.data, num~result+time, sum)
  surv.plot = ggplot(surv.counts) + geom_col(aes(x = time, y = num, fill = result)) + 
    theme_cowplot() + xlim(0, thyme) + 
    ylab("number detected")
  
  
  ss.data = sim_output[[14]]
  ss.counts = aggregate(ss.data, num.rem~time, sum)
  ss.plot = ggplot(ss.counts) + geom_col(aes(x = time, y = num.rem)) +
    theme_cowplot() + xlim(0, thyme) +
    ylab("sharpshoot removed")
  
  p.list = list(N.plot, SEI.plot,
               spread.plot, incidence.plot,
               harvest.plot, surv.plot,
               ss.plot)
  
  grid.arrange(N.plot, SEI.plot, ncol = 2, nrow = 1)
  grid.arrange(spread.plot, incidence.plot, ncol = 2, nrow = 1)
  grid.arrange(harvest.plot, surv.plot, ncol = 2, nrow = 1)
  grid.arrange(ss.plot, ncol = 2, nrow = 1)
}