
pop.data = sim_output[[11]]

p.pop = ggplot() + geom_tile(data = pop.data, aes(x = x.now, y = y.now, fill = fam.size)) +
  theme_cowplot() + scale_fill_gradient(low = "white", high = "blue") + xlim(0, grid.xmax) + ylim(0, grid.ymax)

p.ani.pop = p.pop + transition_time(time) + labs(title = "Time: {frame_time}")

p.pop.gif = animate(p.ani.pop, duration = 30, fps = 10, nframes = 30*10, width = 400, height = 400, renderer = gifski_renderer())

p.pop.gif
