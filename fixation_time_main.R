###Script to impliment functions and plot results. 

setwd("~/GitHub/fixation_time/")
source("fixation_time_functions.R")

mutation.rate <- 1.26e-4

crypt.sizes <- seq(from=1,to=20,by=1)
sample.size <- 1000

data.matrix <- matrix(data=NA,nrow=sample.size,ncol=length(crypt.sizes))

for(i in 1:length(crypt.sizes)){
  output.from.sim <- fixation_simulation(division_rate = 0.2,pop.size = crypt.sizes[i],new_mut_selection_coefficient = 1,fixed_mutations_max = sample.size,count_out_loud = F)
  data.matrix[,i] <- output.from.sim[[1]]
  print(i)
}

data.matrix <- as.data.frame(data.matrix)
colnames(data.matrix) <- paste("Crypt_size_",crypt.sizes,sep="")

write.table(data.matrix,file = "fixation_simulations_number_of_divisions.txt",quote = F,sep="\t",row.names = F)

library("ggplot2")

p <- ggplot(data.matrix, aes(Crypt_size_1))
p + geom_violin()


library(vioplot)
x1 <- data.matrix$Crypt_size_3
x2 <- data.matrix$Crypt_size_4
x3 <- data.matrix$Crypt_size_5
x4 <- data.matrix$Crypt_size_6
x5 <- data.matrix$Crypt_size_7
x6 <- data.matrix$Crypt_size_8
x7 <- data.matrix$Crypt_size_9
x8 <- data.matrix$Crypt_size_10
x9 <- data.matrix$Crypt_size_11
x10 <- data.matrix$Crypt_size_12
x11 <- data.matrix$Crypt_size_13
x12 <- data.matrix$Crypt_size_14
x13 <- data.matrix$Crypt_size_15
x14 <- data.matrix$Crypt_size_16
x15 <- data.matrix$Crypt_size_17
x16 <- data.matrix$Crypt_size_18
x17 <- data.matrix$Crypt_size_19
x18 <- data.matrix$Crypt_size_20



vioplot(x1, x2, x3,
        x4,
        x5,
        x6,
        x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,
        names=paste("Crypt size\n",3:20), 
        col="gold")

