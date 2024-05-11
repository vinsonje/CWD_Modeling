require(cowplot)

ggplot() + geom_tile(aes(x = pop[,5], y = pop[,6], fill = pop[,1])) + 
  geom_text(aes(x = grid[,6], y = grid[,7], label=grid[,1]), alpha=0.5, color="brown") +
  theme_cowplot()



