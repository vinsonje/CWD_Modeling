require(cowplot)

ggplot() + geom_tile(aes(x = pop[,5], y = pop[,6], fill = pop[,1])) + 
  geom_text(aes(x = grid[,6], y = grid[,7], label=grid[,1]), alpha=0.75, color="#997009", size = 3.0) +
  theme_cowplot() + ylab("Y (km)") + xlab("X (km)") + 
  scale_fill_gradient(name = "family size", low = "#923CB5", high = "#301934")

centroids2 = data.frame(centroids, id = seq(1:dim(centroids)[1]))

ggplot(centroids2) + geom_tile(aes(x = X1, y = X2), fill = "white") + 
  geom_text(aes(x = X1, y = X2, label = id), alpha=0.75, color="#997009", size = 3.0) +
  theme_cowplot() + ylab("Y (km)") + xlab("X (km)") 
