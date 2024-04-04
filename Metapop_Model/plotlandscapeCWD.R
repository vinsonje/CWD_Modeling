require(cowplot)

ggplot() + geom_tile(data = pop, aes(x = x.now, y = y.now, fill = fam.size)) + 
  geom_text(data = grid, aes(x = cent.x, y = cent.y, label=grid.id), alpha=0.5, color="white") +
  theme_cowplot()
