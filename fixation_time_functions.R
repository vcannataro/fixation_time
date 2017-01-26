###
#Simulation of dynamics within the crypt base
#
#Cells exist in a ring
#When a cell divides, the new cell replaces a neighbor of the parent cell
#
#Need to record time new cell exists, and whether or not it fixes
#
#Run either a certain number of experiments or until a certain number of fixations occur. 
#
### 

population.size <- 6


###Function to randomly pick a neighbor--- outputs the neighbor to replace given the parent 
Neighbor_replace <- function(n){
  parent <- n
  if(parent!=population.size & parent!=1){
    if(rbinom(1,1,0.5)==0){
     return(parent-1) 
    }else{
      return(parent+1)
    }
  }else{
    if(parent==1){
      if(rbinom(1,1,0.5)==0){
        return(population.size)
      }else{
        return(2)
      }
    }else{
      if(rbinom(1,1,0.5)==0){
        return(population.size-1)
      }else{
        return(1)
      }
    }
  }
}


#testing if this worked. 
# test.vec <- rep(NA,10000)
# for(i in 1:length(test.vec)){
#   test.vec[i] <- Neighbor_replace(5)
# }
# hist(test.vec)
# mean(test.vec)




