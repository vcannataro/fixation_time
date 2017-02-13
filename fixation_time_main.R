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
# 
# library("ggplot2")
# 
# p <- ggplot(data.matrix, aes(Crypt_size_1))
# p + geom_violin()


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

#Thanks Ben Bolker for the stackexchange tips
par(las=1,bty="l")  ## my preferred setting
## set up empty plot
plot(0:1,0:1,type="n",xlim=c(0.5,18.5),ylim=range((c(x1, x2, x3,
                                                     x4,
                                                     x5,
                                                     x6,
                                                     x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18))),
     axes=F,ann=FALSE)
vioplot(x1, x2, x3,
        x4,
        x5,
        x6,
        x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,
        names=paste("",3:20), 
        col="gold",add=T)
axis(side=1,at=1:18,labels=paste("",3:20,sep=""))
axis(side=2,at=seq(0,7000,1000),labels=seq(0,7000,1000))
title(ylab="Number of divisions until fixation",xlab="Stem cell compartment population size")
title("Divisions until fixation among 1000 \nfixation events per population size")

mean.vec <- NULL
for(i in 1:ncol(data.matrix)){
  mean.vec[i] <- mean(data.matrix[,i])
}

points(mean.vec[3:20],pch=8,col="red",cex=2)

plot(mean.vec[3:20]*mutation.rate,pch=8,col="red",cex=2,ylab="Mean divisions until fixation 
times mutation rate",xlab="Stem cell compartment population size")






####Saving plots

png(file="violin_plots_fixation_divisions.png",res=200,units = "in",height = 4,width = 12)
#Thanks Ben Bolker for the stackexchange tips
par(las=1,bty="l")  ## my preferred setting
## set up empty plot
plot(0:1,0:1,type="n",xlim=c(0.5,18.5),ylim=range((c(x1, x2, x3,
                                                     x4,
                                                     x5,
                                                     x6,
                                                     x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18))),
     axes=F,ann=FALSE)
vioplot(x1, x2, x3,
        x4,
        x5,
        x6,
        x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,
        names=paste("",3:20), 
        col="gold",add=T)
axis(side=1,at=1:18,labels=paste("",3:20,sep=""))
axis(side=2,at=seq(0,7000,1000),labels=seq(0,7000,1000))
title(ylab="Number of divisions until fixation",xlab="Stem cell compartment population size")
title("Divisions until fixation among 1000 \nfixation events per population size")
dev.off()

mean.vec <- NULL
for(i in 1:ncol(data.matrix)){
  mean.vec[i] <- mean(data.matrix[,i])
}

points(mean.vec[3:20],pch=8,col="red",cex=2)

png(file="expected_mutations.png",res=200,units = "in",height = 4,width = 8)
plot(mean.vec*mutation.rate,pch=8,col="red",cex=2,ylab="Mean divisions until fixation times mutation rate",xlab="Stem cell compartment population size")
dev.off()




