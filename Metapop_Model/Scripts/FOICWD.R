FOICWD = function(pop, centroids, cells,
              B1, F1, F2,
              B1P.m, B1P.inter,
              prions){
  
  #B1 = transmission probability given contact for direct contact (a constant)
  #F1 = probability of direct transmission at 0 distance (transmission within cell)
  #F2 = **contact** probability live deer at other distances that is a glm object that fits with contact-distance data

  #B1P = parameters for the transmission probability function given "environmental contact from prions, 
  #      used in function that relates the number of prions with probability of successful transmission (a constant)
  #B1P.m = the slope of the linear portion of the function(should be negative)
  #B1P.inter = the intercept of the linear portion of the function
  #F1P = probability of environmental transmission contact event happening at 0 distance (transmission within cell)
 
  Pse = matrix(nrow=cells,ncol=1)	
  #initialized matrix/vector that will have the probability of transmission 
  #for susceptible individuals in each cell (each row is the cell on the landscape)
  
  if((sum(pop[pop[,10] > 0, 10]) + sum(prions[prions[,3] > 0, 3])) > 0) { 
  #if there are any infectious agents (Infected or Prions)
  #need to calculate for each cell in the landscape, 
  #the probability of transmission from both direct and environmental transmission
    
    #####################
    #Direct Transmission
    #####################

    #How many cells have a infected individual in them
    num_I = sum(pop[pop[,10] > 0, 10])
    
    #Subset the population matrix to only have the cells that have infected individuals
    Imat = pop[pop[,10] > 0, , drop = FALSE]
    
    #if there are infected individuals on the landscape
    if(nrow(Imat)>0){
      
      #create an array that has all 0 (for now), each entry in the array (array[i,,]):
      #the cell that has the infected individual ("I_X"), 
      #each of these is a matrix that has the number of rows that is the number of cells in the landscape, row names ("cell_XXX")
        #The columns are:
        #the distance from the infected cell ("dist") 
        #the probability that the deers from the respective cells have a direct contact ("prob")
      pdI = array(0, dim = c(num_I, cells, 2), dimnames = list(paste0("I_", 1:num_I), paste0("cell_", 1:cells), c("dist","prob")))
      
      #to replace all the 0's in the array, we need to loop through each infected cell (array[i,,])
      for(i in 1:nrow(Imat)){
        # print(paste("calculating I distances ", i, " in ", nrow(Imat)))
        #calculate the distance of the infected cell with all the other cells on the landscape
        pdI[i, , 1] = sqrt((centroids[, 1] - Imat[i, 5])^2 + (centroids[, 2] - Imat[i, 6])^2) 
        #calculate the probability of the infected deer coming into contact with all the other cells on the landscape
        #this is from the ("binomial") glm that takes in the distance between the cells and outputs the probability of contact occurring
        pdI[i, , 2] = exp(predict(F2, newdata = data.frame(xx = pdI[i, , 1])))
      } #end of for loop
    } #end of if statement
    
    #if there are any infected infected individuals on the landscape
    if(nrow(Imat) > 0){ 
      
      #create an empty matrix, then put 0's in the first column, where
      #row is the cell on the landscape
      #column is the infected individual cell
      #this will have the force of infection (FOI, prob. of contact*prob of succ. trans. given contact) as each entry
      B_I = matrix(nrow = cells, ncol = dim(pdI)[1])
      B_I[, 1] = 0
      
      #loop through each infected cell/column of the matrix
      for(i in 1:dim(pdI)[1]){
        #calculate the FOI that infected cell i has on each cell on the landscape
        B_I[,i] = (pdI[i,,2] * B1) #prob. of contact * prob. of successful transmission
      } #end for loop
      
      #to calculate each cell on the landscape has coming from all infected cells
      #need to do sum of each row
      B_I = rowSums(B_I)
      
      #loop through each of the infected cells
      for(i in 1:nrow(Imat)){
        
        #if any of the distances are 0, meaning within the same cell
        I_direct = which(pdI[i, , 1:2][, 1] == 0)
        #set that FOI for within the same cell to F1
        B_I[I_direct] = (F1)
        
      }
    }
    
    #####################
    #Environmental Transmission
    #####################
    
    #How many cells have prions on them
    num_P_cells = length(which(prions[,3] > 0))
    
    #Subset the prions matrix to only have the cells that have prions
    Pmat = prions[prions[,3] > 0, , drop = FALSE]
    
    #if there are prions on the landscape
    if(nrow(Pmat)>0){
      #create an array that has all 0 (for now), each entry in the array (array[i,,]):
      #the cell that has the cell that the prions are in  ("P_X"), 
      #each of these is a matrix that has the number of rows that is the number of cells in the landscape, row names ("cell_XXX")
      #The columns are:
      #the distance from the prion cell ("dist") 
      #the probability that the deers from the respective cells have a prion contact, i.e. move into that cell ("prob")
      #right now this will come from the same glm for direct contacts
      #I think it will need to be something with a homerange overlap analysis
      pdP = array(0, dim = c(num_P_cells,cells,2), dimnames = list(paste0("P_", 1:num_P_cells), paste0("cell_", 1:cells), c("dist","prob")))
      
      #to replace all the 0's in the array, we need to loop through each prion cell (array[i,,])
      for(i in 1:nrow(Pmat)){
        # print(paste("calculating P distances ", i, " in ", nrow(Pmat)))
        #calculate the distance of the prion cell with all the other celss on the landscape
        pdP[i, , 1] = sqrt((centroids[, 1] - Pmat[i, 1])^2 + (centroids[, 2] - Pmat[i, 2])^2) #this is distance calculation; you had to add an i here not sure if that is needed
        #calculate the probability of the prion coming into contact with deer from all the other cells on the landscape
        #this is from the ("binomial") glm that takes in the distance between the cells and outputs the probability of indirect contact occurring
        pdP[i, , 2] = predict(F2, newdata = data.frame(xx = pdP[i, , 1]), type = "response")
      }
    }
    #if there are prions on the landscape
    if(nrow(Pmat) > 0){ 
      #create an empty matrix, then put 0's in the first column, where
      #row is the cell on the landscape
      #column is the prion "infected" cell
      #this will have the force of infection (FOI, prob. of contact*prob of succ. trans. given contact) as each entry
      B_P = matrix(nrow = cells, ncol = dim(pdP)[1])
      B_P[, 1] = 0
      
      #loop through each prion "infected" cell 
      for(i in 1:dim(pdP)[1]){
        #calculate the FOI that infected cell i has on each cell on the landscape
        #need to determine the prob. of successful transmission as a function of the number of prions
        B1P.real = 1/(1+exp(B1P.m*Pmat[i,3] + B1P.inter))
        B_P[,i] = (pdP[i,,2] * B1P.real)
      }
      
      #to calculate each cell on the landscape has coming from all infected cells
      #need to do sum of each row
      B_P = rowSums(B_P)
      
      #loop through each prion "infected" cell
      for(i in 1:nrow(Pmat)){
        #if any of the distances are 0, meaning within the same cell
        P_direct = which(pdP[i, , 1:2][, 1] == 0)
        #set that FOI for within the same cell to F1P
        B_P[P_direct] = 1/(1+exp(B1P.m*Pmat[i,3] + B1P.inter)) #This needs to be a function of the number of prions in the cell
        
      }
    }
    
    #Need to get the total FOI from both direct (B_I) and indirect prion (B_P)
    #If there are both infected individual and prions on the landscape
    if(nrow(Pmat) > 0 & nrow(Imat) > 0){
      #Add both direct and prion transmission together
      Bsum = B_I + B_P
      #if there are only infected individuals
    } else if(nrow(Imat) > 0){
      Bsum = B_I
      #if there are only prions
    } else if(nrow(Pmat) > 0){Bsum = B_P}
    
    #This scales the probability of infection to be between 0 and 1. 
    Pse = 1 - exp(-Bsum)

  } else {Pse = matrix(0, nrow = cells, ncol=1)} 	#if/else any infectious closing bracket (There are no infected individuals or prions)

  return(Pse)
  
} #function closing bracket
