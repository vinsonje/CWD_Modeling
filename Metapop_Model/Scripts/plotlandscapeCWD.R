require(cowplot)

ggplot() + geom_tile(aes(x = pop[,5], y = pop[,6], fill = pop[,1])) + 
  geom_text(aes(x = grid[,6], y = grid[,7], label=grid[,1]), alpha=0.75, color="#997009", size = 3.0) +
  theme_cowplot() + ylab("Y (km)") + xlab("X (km)") + 
  scale_fill_gradient(name = "family size", low = "#923CB5", high = "#301934")



