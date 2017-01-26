########Functions script
#Author: Vincent L. Cannataro
#Date: Jan 25 2017

###
#Simulation of dynamics within the crypt base
#
#Cells exist in a ring
#When a cell divides, the new cell replaces a neighbor of the parent cell
#
#Need to record time new cell exists, and whether or not it fixes, and 
#total number of divisions (so we can calculate expected mutation number)
#
#Run either a certain number of experiments or until a certain number of fixations occur. 
#
### 

# population.size <- 6
# population.size <- 20
# 
# background.division.rate <- 0.2 #mouse
# background.division.rate <- 0.143 #human 

# mutation.rate <- 1.26e-4


###Function to randomly pick a neighbor--- outputs the neighbor to replace given the parent 
Neighbor_replace <- function(n,population.size){
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

# new_mut_selection_coefficient <- 1

fixation_simulation <- function(division_rate=0.2,
                                pop.size=6,
                                new_mut_selection_coefficient=1,
                                fixed_mutations_max=100,
                                count_out_loud=T){
  
  background.division.rate <- division_rate
  new.lineage.division.rate <- background.division.rate*new_mut_selection_coefficient
  population.size <- pop.size
  
  
  fixed.division.number <- rep(0,fixed_mutations_max)
  fixed.time <- rep(0,fixed_mutations_max)
  fixed.total <- 0
  while(fixed.total<fixed_mutations_max){
    
    
    crypt.ring <- rep(0,population.size) #Have background lineage of 0s
    crypt.ring[sample(1:population.size,size = 1)] <- 1 #Randomly pick a single cell to be new lineage
    
    rates.ring <- rep(0,population.size) #vector to hold rates for the Gillespie algorithm 
    
    division.number <- 0 #counter_for_division
    total.time <- 0
    while(mean(crypt.ring)!=0 & mean(crypt.ring)!=1){  #While a single 1 (new lineage) exists in the ring
      
      rates.ring[which(crypt.ring==0)] <- background.division.rate
      rates.ring[which(crypt.ring==1)] <- new.lineage.division.rate
      
      new_rate <- sum(rates.ring)
      
      new_wait <- rexp(1,new_rate)
      
      parent.choice <- sample(x = 1:population.size,size = 1,replace = T,prob = rates.ring/new_rate)
      child_lineage <- crypt.ring[parent.choice]
      unlucky_neighbor <- Neighbor_replace(parent.choice,population.size = population.size)
      crypt.ring[unlucky_neighbor] <- child_lineage
      if(child_lineage==0){
        rates.ring[unlucky_neighbor] <- background.division.rate
      }else{
        rates.ring[unlucky_neighbor] <- new.lineage.division.rate
      }
      
      total.time <- total.time+new_wait
      division.number <- division.number+1
    }
    if(mean(crypt.ring)==1){
      fixed.total <- fixed.total+1
      fixed.division.number[fixed.total] <- division.number
      fixed.time[fixed.total] <- total.time
      if(count_out_loud){message(paste("Mutation fixed! Percent of mutations fixed before simulation ends: ",round(fixed.total/fixed_mutations_max,4)*100,"%",sep=""))} 
    }
  }
  output <- list()
  output[[1]] <- fixed.division.number
  output[[2]] <- fixed.time
  return(output)
}

# simulation_output <- fixation_simulation(pop.size = 20,division_rate = 0.2)
# simulation_output[[1]]
# simulation_output[[2]]
# 
# mean(simulation_output[[1]])*mutation.rate

