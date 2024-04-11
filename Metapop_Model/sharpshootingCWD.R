########################################
#Sharpshooting
########################################
#The purpose of this function is to simulate
#sharpshooting on a landscape. It has inputs of:
#
#########################################

sharpshootingCWD = function(pop, centroids, ss.loc, ss.time, ss.radius, thyme){
  
  pop.out = pop
  print(dim(pop.out)[1])
  
  if(ss.time == thyme){
  for(q in 1:length(ss.loc)){
    ss.loc.id.now = ss.loc[q]
    ss.loc.xloc.now = centroids[ss.loc.id.now, 1]
    ss.loc.yloc.now = centroids[ss.loc.id.now, 2]
    
    cells.in.ss.radius = NULL
    for(w in 1:dim(centroids)[1]){
      potential.cell.x = centroids[w,1]
      potential.cell.y = centroids[w,2]
      
      distance = sqrt((ss.loc.xloc.now - potential.cell.x)^2 + (ss.loc.yloc.now - potential.cell.y)^2)
      
      if(distance <= ss.radius){
        # print("true")
        cells.in.ss.radius = append(cells.in.ss.radius, w)
        }
    }#end of distance calc loop
    
    fams.in.radius = intersect(pop.out[,3], cells.in.ss.radius)
    print(paste("removing", length(fams.in.radius), "families"))
   
    fams.rem.index = which(pop[,3] %in% fams.in.radius)
    
    pop.out = pop.out[-fams.rem.index,]
    print(dim(pop.out)[1])
    
  }#end of ss.loc loop
  }
  
  return(pop.out)
}#end of function


ss.loc = c(11, 265)
ss.time = 10
ss.radius = 1.0

thyme = 12

test.pop = pop

ss.test = sharpshootingCWD(test.pop, centroids, ss.loc, ss.time, ss.radius, thyme)

