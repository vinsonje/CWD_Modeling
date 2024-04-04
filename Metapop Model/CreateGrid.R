create.grid = function(grid.xmax, grid.ymax, cell.x.size, cell.y.size){
  
  cells.x = grid.xmax/cell.x.size
  cells.y = grid.ymax/cell.x.size
  grid.id.num = cells.x*cells.y
  grid.x.breaks = seq(0, grid.xmax, cell.x.size)
  grid.y.breaks = seq(0, grid.ymax, cell.y.size)
  
  grid.xy.loc = data.frame()
  for(i in 1:(length(grid.x.breaks)-1)){
    for(j in 1:(length(grid.y.breaks)-1)){
      
      centroid.x = mean(c(grid.x.breaks[i], grid.x.breaks[i+1]))
      centroid.y = mean(c(grid.y.breaks[j], grid.y.breaks[j+1]))
      
      grid.xy.temp = data.frame(x1.loc = grid.x.breaks[i], y1.loc = grid.y.breaks[j], x2.loc = grid.x.breaks[i+1], 
                                y2.loc = grid.y.breaks[j+1], cent.x = centroid.x, cent.y = centroid.y)
      
      grid.xy.loc = rbind(grid.xy.loc, grid.xy.temp)
    }
  }
  grid.xy.loc = data.frame(grid.id = seq(1, grid.id.num), grid.xy.loc)
  return(grid.xy.loc)
}
