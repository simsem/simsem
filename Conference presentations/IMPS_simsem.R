##IMPS Simulation Examples

library(simsem)

##Power analysis example
##Continously varying sample size
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.1)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
ContN <- simUnif(100, 2000)
Output.pow <- simResult(3000, SimData, SimModel, n=ContN, multicore=F)
summary(Output.pow, digits=5)
pow<-continuousPower(Output.pow, powerParam = 'PS2_1')
pow[pow[,2]>.8 & pow[,2]<.84,][1,] #power of .80004 is sample size of 1436

#plot power
plot(pow[,1], pow[,2], type='l',ylab="Power", xlab="Sample", lwd=2, ylim=c(0,1), cex.lab=1.5, cex.axis=1.25)

##Planned missing data example

#Specify factor loadings.
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

#Specify latent variances and covariances
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.1)

#Specify measurement errors
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

#Bind matrices together into a CFA object
CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
#Create data generation template with n=500
SimData <- simData(CFA.Model, 500)
#Create analysis template
SimModel <- simModel(CFA.Model)

#itemGroups specify which items go into which forms, X, form1, form2, form3
itemGroups <- list(c(1,4), c(2,5), 3, 6)
#Create missing object to contain missing data specification
SimMissing <- simMissing(nforms=3, itemGroups=itemGroups, numImps=5)
#Run simulation for 1000 replications using multiple processors
Output <- simResult(1000, SimData, SimModel, SimMissing, multicore=TRUE)

#Run complete data simulations
OutputC <- simResult(1000, SimData, SimModel, multicore=TRUE)


