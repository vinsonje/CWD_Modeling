create.grid = function(grid.xmax, grid.ymax, cell.x.size, cell.y.size){
  
  cells.x = grid.xmax/cell.x.size
  cells.y = grid.ymax/cell.x.size
  grid.id.num = cells.x*cells.y
  grid.x.breaks = seq(0, grid.xmax, cell.x.size)
  grid.y.breaks = seq(0, grid.ymax, cell.y.size)
  
  grid.xy.loc = data.frame()
  grid.xy.loc = NULL
  for(i in 1:(length(grid.y.breaks)-1)){
    for(j in 1:(length(grid.x.breaks)-1)){

      grid.xy.temp = cbind(grid.x.breaks[j], grid.y.breaks[i], grid.x.breaks[j+1], 
                           grid.y.breaks[i+1], 
                           mean(c(grid.x.breaks[j], grid.x.breaks[j+1])), 
                           mean(c(grid.y.breaks[i], grid.y.breaks[i+1])))
      
      grid.xy.loc = rbind(grid.xy.loc, grid.xy.temp)
    }
  }
  
  grid.xy.loc = as.matrix(grid.xy.loc)
  grid.xy.loc2 = matrix(seq(1, grid.id.num), byrow=FALSE, ncol = 1)
  grid.xy.loc3 = cbind(grid.xy.loc2, grid.xy.loc)
  
  return(grid.xy.loc3)
}
