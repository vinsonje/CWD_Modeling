FOICWD = function(pop, centroids, cells,
              B1, F1, F2
              prions){
  
 
  Pse = matrix(nrow=cells,ncol=1)	
  
  #F1 = contact probability at 0 distance
  #F2 = contact probability live deer at other distances (define based on landscape dist matrix)
  #B1 = transmission probability given contact for direct contact (a constant)
  #B2 = transmission probability given contact for indirect contact (a constant)
  
  if((sum(pop[pop[,10] > 0, 10]) + sum(prions[prions[,3] > 0, 3])) > 0) { #if there are any infectious agents (Infected or Prions)
    
    ################
    #within cell transmission
    #W = [F1.*I; F1.*C]; #contact probability at 0 distance, get this for each infectious individual
    #B = zeros(length(id),cells); % creates a matrix with row=num infectious individuals, cols=cells
    
    
    #between cell transmission
    #get the centroid of each infected individual
    #X1 = CT(id(i),1); Y1 = CT(id(i),2); %centroid of infected
    #for one infected individual, one number each for X1 Y1
    #have this already, would be X1=pop[pop[,2]==2,][5]; Y1=X1=pop[pop[,2]==2,][6]
    
    #distance between each infected individual, and all other cells
    #dist = sqrt((CT(:,1)-X1).^2 + (CT(:,2)-Y1).^2); % distance to all other cells
    #is vector with 1 col, 40000 rows (bc 40000 cells)
    ###############
    
    #Direct Transmission
    num_I = sum(pop[pop[,10] > 0, 10])
    
    Imat = pop[pop[,10] > 0, , drop = FALSE]
    
    if(nrow(Imat)>0){
      
      pdI = array(0, dim = c(num_I,cells,2), dimnames = list(paste0("I_", 1:num_I), paste0("cell_", 1:cells), c("dist","prob")))
      
      for(i in 1:nrow(Imat)){
        
        pdI[i, , 1] = sqrt((centroids[, 1] - Imat[i, 5])^2 + (centroids[, 2] - Imat[i, 6])^2) #this is distance calculation; you had to add an i here not sure if that is needed
        
        pdI[i, , 2] = exp(predict(F2, newdata = data.frame(xx = pdI[i, , 1]))) 
      }
    }
    
    #Environmental Transmission
    num_P_cells = length(which(prions[,3] > 0))
    
    Pmat = prions[prions[,3] > 0, , drop = FALSE]
    
    if(nrow(Pmat)>0){
      
      pdP = array(0, dim = c(num_P_cells,cells,2), dimnames = list(paste0("P_", 1:num_I), paste0("cell_", 1:cells), c("dist","prob")))
      
      for(i in 1:nrow(Pmat)){
        
        pdP[i, , 1] = sqrt((centroids[, 1] - Pmat[i, 1])^2 + (centroids[, 2] - Pmat[i, 2])^2) #this is distance calculation; you had to add an i here not sure if that is needed
        
        pdP[i, , 2] = exp(predict(F2, newdata = data.frame(xx = pdI[i, , 1])))  
      }
    }

    #B- FOI matrix from infected to all other cells
    #I- matrix of locations for infected individuals (Imat[,7])
    #id- row number of grid/centroids where there are infected individuals or carcasses (Imat[,7])(Cmat[,7])
    #B1- transmission probability given contact for indirect contact with infected live deer 
    #B- matrix of FOI from all infected cells
    
    #B(i,:) = -B1.*I(id(i)).*prob' - B2.*C(id(i)).*probi'; %FOI from infected cell id(i) to all other cells
    #B=matrix(nrow=num_C/I,ncol=cells)
    
    #force of infection from each infected deer
    #-B1.*I(id(i)).*prob'
    #force of infection from environmental transmission 
    #B2.*C(id(i)).*probi'
    
    if(nrow(Imat) > 0){ 
      
      B_I = matrix(nrow = cells, ncol = dim(pdI)[1])
      B_I[, 1] = 0
      
      for(i in 1:dim(pdI)[1]){
        
        B_I[,i] = (pdI[i,,2] * B1)
        
      }
      
      B_I = rowSums(B_I)
      
      for(i in 1:nrow(Imat)){
        
        I_direct = which(pdI[i, , 1:2][, 1] == 0) #this just says if there is any cells that have a distance of 0
        B_I[I_direct] = (F1) #But the way this is written this looks like the prob of transmission is set to F1
        
      }
    }
    
    if(nrow(Pmat) > 0){ 
      
      B_P = matrix(nrow = cells, ncol = dim(pdP)[1])
      B_P[, 1] = 0
      
      for(i in 1:dim(pdP)[1]){
        
        B_P[,i] = (pdP[i,,2] * B1P)
        
      }
      
      B_P = rowSums(B_P)
      
      for(i in 1:nrow(Pmat)){
        
        P_direct = which(pdP[i, , 1:2][, 1] == 0) #this just says if there is any cells that have a distance of 0
        B_P[P_direct] = (F1P) #This needs to be a function of the number of prions in the cell
        
      }
    }

    if(nrow(Pmat) > 0 & nrow(Imat) > 0){
      Bsum = B_I + B_P
    } else if(nrow(Imat) > 0){
      Bsum = B_I
    } else if(nrow(Pmat) > 0){Bsum = B_P}
    
    Pse = 1 - exp(-Bsum)

  } else {Pse = matrix(0, nrow = cells, ncol=1)} 	#if/else any infectious closing bracket (There are no infected individuals or prions)

  return(Pse)
  
} #function closing bracket
