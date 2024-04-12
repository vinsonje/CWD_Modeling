require(cowplot)

pop.time = sim_output[[11]]

ggplot() + geom_tile(aes(x = pop[,5], y = pop[,6], fill = pop[,1])) + 
  geom_text(data = grid, aes(x = cent.x, y = cent.y, label=grid.id), alpha=0.5, color="brown") +
  theme_cowplot()

ggplot(pop.time) + geom_histogram(aes(grid.loc), bins = 100) + facet_wrap(~time)
ggplot(pop.time) + geom_histogram(aes(move.dis), bins = 100) + facet_wrap(~time)

pop.final = pop.time[which(pop.time$time==70),]

#Looking at the number of families through time
table(pop.time$time)
