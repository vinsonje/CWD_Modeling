#Plotting the simulation
library(cowplot)
library(ggplot2)
library(gganimate)
library(gifski)
library(cowplot)
library(gridExtra)
library(tidyr)
library(magick)

plot.landscape.meta = function(sim_output, grid.xmax, grid.ymax, gif.time=30, gif.fps=20, save=FALSE){
  
  #Prion Heatmap
  p.prions = ggplot() + geom_tile(data = data.frame(sim_output[[9]]), aes(x = X1, y = X2, fill = prions)) + 
    theme_cowplot() + scale_fill_gradient(low = "white", high = "red") + xlim(0, grid.xmax) + ylim(0, grid.ymax)
  
  #Plot where infectious are through time
  I.loc.list = sim_output[[6]]
  I.loc.df = data.frame()
  for(j in 1:length(I.loc.list)){
    I.loc = unlist(I.loc.list[j])
    time = rep(j, length(I.loc))
    I.loc.temp = data.frame(time = as.integer(time), I.loc = I.loc)
    I.loc.df = rbind(I.loc.df, I.loc.temp)
  }
  
  I.loc.table = table(I.loc.df)
  I.loc.df2 = as.data.frame(I.loc.table)
  names(I.loc.df2) = c("time", "grid.id", "num")
  I.loc.df2 = I.loc.df2[-which(I.loc.df2$num==0),]
  
  x.loc.temp = NULL
  y.loc.temp = NULL
  for(i in 1:dim(I.loc.df2)[1]){
    I.loc.grid.id = as.numeric(as.character(I.loc.df2$grid.id[i]))
    x.loc.temp = c(x.loc.temp, centroids[I.loc.grid.id,1])
    y.loc.temp = c(y.loc.temp, centroids[I.loc.grid.id,2])
  }
  I.loc.df.final = data.frame(I.loc.df2, x.loc = x.loc.temp, y.loc = y.loc.temp)
  I.loc.df.final = I.loc.df.final[order(I.loc.df.final$time),]
  I.loc.df.final$time = as.numeric(I.loc.df.final$time)
  
  p.inf = ggplot() + geom_tile(data = I.loc.df.final, aes(x = x.loc, y = y.loc, fill = num)) +
    theme_cowplot() + scale_fill_gradient(low = "white", high = "blue") + xlim(0, grid.xmax) + ylim(0, grid.ymax)

  #Infection Abundance
  I.time = data.frame(time = 1:length(sim_output[8]$Isums), I = sim_output[8]$Isums)
  
  p.inf.line = ggplot(I.time) + geom_line(aes(x = time, y = I), color="black") + theme_cowplot()
  
  #total abundance
  p.pop.line = ggplot(sim_output[[10]]) + geom_line(aes(x = time, y = pop)) + theme_cowplot()
  
  
  p.ani.prions = p.prions + transition_time(time) + labs(title = "Time: {frame_time}")
  p.ani.inf = p.inf + transition_time(time) + labs(title = "Time: {frame_time}")
  p.ani.inf.line = p.inf.line + transition_reveal(time)
  p.ani.pop.line = p.pop.line + transition_reveal(time)
  
  p.prions.gif = animate(p.ani.prions, duration = gif.time, fps = gif.fps, nframes = gif.time*gif.fps, width = 400, height = 400, renderer = gifski_renderer())
  p.inf.gif = animate(p.ani.inf, duration = gif.time, fps = gif.fps, nframes = gif.time*gif.fps, detail = gif.fps,  width = 400, height = 400, renderer = gifski_renderer())
  p.inf.line.gif = animate(p.ani.inf.line, duration = gif.time, fps = gif.fps, nframes = gif.time*gif.fps, detail = gif.fps, width = 400, height = 400, renderer = gifski_renderer())
  p.pop.line.gif = animate(p.ani.pop.line, duration = gif.time, fps = gif.fps, nframes = gif.time*gif.fps, detail = gif.fps, width = 400, height = 400, renderer = gifski_renderer())
  
  p.prions.gif.read = image_read(p.prions.gif)
  p.inf.gif.read = image_read(p.inf.gif)
  p.inf.line.gif.read = image_read(p.inf.line.gif)
  p.pop.line.gif.read = image_read(p.pop.line.gif)
  
  new_gif = image_append(c(p.prions.gif.read[1], p.inf.gif.read[1]))
  for(i in 2:(gif.time*gif.fps)){
    print(i)
    combined = image_append(c(p.prions.gif.read[i], p.inf.gif.read[i]))
    new_gif = c(new_gif, combined)
  }
  
  new_gif2 = image_append(c(p.pop.line.gif.read[1], p.inf.line.gif.read[1]), stack = FALSE)
  for(i in 2:min(length(p.pop.line.gif.read), length(p.inf.line.gif.read))){
    print(i)
    combined = image_append(c(p.pop.line.gif.read[i], p.inf.line.gif.read[i]), stack = FALSE)
    new_gif2 = c(new_gif2, combined)
  }
  
  new_gif3 = image_append(c(new_gif[1], new_gif2[1]), stack = TRUE)
  for(i in 2:min(length(new_gif), length(new_gif2))){
    print(i)
    combined = image_append(c(new_gif[i], new_gif2[i]), stack = TRUE)
    new_gif3 = c(new_gif3, combined)
  }
  
  print(new_gif3)
  
  if(save==TRUE){anim_save("Meta_outbreak.gif", new_gif3)}
  
}

     

