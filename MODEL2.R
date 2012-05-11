### Models need for simulations. MODEL 2

#set working directory for use with HPC
##NOT NEEDED WITH HPC
setwd('D:\\Users\\Fun Police\\Documents\\simSEM\trunk')

##Need to run the line below interactively in R before using scripts...
install.packages("simsem_0.0-5.tar.gz", repos=NULL, type="source")

#Set up cluster to run different conditions on different nodes

library(snow)

cl <- makeCluster(11, type="MPI")

## runModel2 = Function to put together model and run simualation
## Function arguments are parameters in the simulation
## Params = vector of parameters in model with order, fl wtc, ar1, cl1
## groups = list of item columns to go in x, a, b and c block
#require(simsem)

runModel2 <- function(params = c(.7, .1, .4, 0), MISseed=1234562) {
require(simsem)

fl <- params[1]
wtc <- params[2]
ar1 <- params[3]
cl1 <- params[4]

#Time specific variances need to make each one equal 1. 
t2xvar <- 1 - ar1^2
t2mvar <- 1 - cl1^2 - ar1^2 - 2*(cl1*wtc*ar1)
t2yvar <- 1 - cl1^2 - ar1^2 - 2*(cl1*wtc*ar1)

#t3xvar <- 1 - ar1^4 - ar1^2*t2xvar
#t3mvar <- 1 - cl1^2*t2xvar - cl1^2*ar1^2 - (cl1*ar1)^2 - 2*(ar1^3*cl1*wtc) - ar1^4 - (ar1*cl1)^2 - 2*(cl1*ar1^3*wtc) - ar1^2*t2mvar - 2*(ar1*wtc*cl1)
#t3yvar <- 1 - (cl1^2)*t2mvar - cl1^4 - (ar1^2)*2*t2yvar - ar1^4 - (ar1^2)*(cl1^2) - 2*((cl1^3)*wtc*ar1) - 2*((cl1^3)*wtc*ar1) - 2*(cl1^2*wtc*(ar1^2)) - 2*((ar1^3)*cl1*wtc) - 2*(cl1^3*ar1*wtc) - (cl1*ar1)^2 - (ar1*cl1)^2 - 2*(cl1*wtc*ar1*sqrt(t2mvar)*sqrt(t2yvar)) - 2*(ar1^3)*cl1*wtc

if(t2xvar <0 || t2mvar < 0  || t2yvar  < 0) {
results <- NULL
} else {

#Residual variance
resvar <- fl^2

loading<-matrix(0,27,9)
loading[1:3,1]<-NA
loading[4:6,2]<-NA
loading[7:9,3]<-NA
loading[10:12,4]<-NA
loading[13:15,5]<-NA
loading[16:18,6]<-NA
loading[19:21,7]<-NA
loading[22:24,8]<-NA
loading[25:27,9]<-NA
loading

load.val<-matrix(0,27,9)
load.val[1:3,1]<-fl
load.val[4:6,2]<-fl
load.val[7:9,3]<-fl
load.val[10:12,4]<-fl
load.val[13:15,5]<-fl
load.val[16:18,6]<-fl
load.val[19:21,7]<-fl
load.val[22:24,8]<-fl
load.val[25:27,9]<-fl
load.val

LY<-simMatrix(loading,load.val)
#summary(LY)

LYcons1 <- matrix(0, 3, 2)
LYcons1[1,] <- c(1, 1)
LYcons1[2,] <- c(10, 4)
LYcons1[3,] <- c(19, 7)
rownames(LYcons1) <- rep("LY", 3)

LYcons2 <- matrix(0, 3, 2)
LYcons2[1,] <- c(2, 1)
LYcons2[2,] <- c(11, 4)
LYcons2[3,] <- c(20, 7)
rownames(LYcons2) <- rep("LY", 3)

LYcons3 <- matrix(0, 3, 2)
LYcons3[1,] <- c(3, 1)
LYcons3[2,] <- c(12, 4)
LYcons3[3,] <- c(21, 7)
rownames(LYcons3) <- rep("LY", 3)

LYcons4 <- matrix(0, 3, 2)
LYcons4[1,] <- c(4, 2)
LYcons4[2,] <- c(13, 5)
LYcons4[3,] <- c(22, 8)
rownames(LYcons4) <- rep("LY", 3)

LYcons5 <- matrix(0, 3, 2)
LYcons5[1,] <- c(5, 2)
LYcons5[2,] <- c(14, 5)
LYcons5[3,] <- c(23, 8)
rownames(LYcons5) <- rep("LY", 3)

LYcons6 <- matrix(0, 3, 2)
LYcons6[1,] <- c(6, 2)
LYcons6[2,] <- c(15, 5)
LYcons6[3,] <- c(24, 8)
rownames(LYcons6) <- rep("LY", 3)

LYcons7 <- matrix(0, 3, 2)
LYcons7[1,] <- c(7, 3)
LYcons7[2,] <- c(16, 6)
LYcons7[3,] <- c(25, 9)
rownames(LYcons7) <- rep("LY", 3)

LYcons8 <- matrix(0, 3, 2)
LYcons8[1,] <- c(8, 3)
LYcons8[2,] <- c(17, 6)
LYcons8[3,] <- c(26, 9)
rownames(LYcons8) <- rep("LY", 3)

LYcons9 <- matrix(0, 3, 2)
LYcons9[1,] <- c(9, 3)
LYcons9[2,] <- c(18, 6)
LYcons9[3,] <- c(27, 9)
rownames(LYcons9) <- rep("LY", 3)

equal.loading <- simEqualCon(LYcons1, LYcons2, LYcons3, LYcons4, LYcons5, LYcons6, LYcons7, LYcons8, LYcons9, modelType="SEM")


error.na<-matrix(0,27,27)
diag(error.na)<-NA
error.na[1,10]<-NA
error.na[10,1]<-NA
error.na[2,11]<-NA
error.na[11,2]<-NA
error.na[3,12]<-NA
error.na[12,3]<-NA
error.na[4,13]<-NA
error.na[13,4]<-NA
error.na[5,14]<-NA
error.na[14,5]<-NA
error.na[6,15]<-NA
error.na[15,6]<-NA
error.na[7,16]<-NA
error.na[16,7]<-NA
error.na[8,17]<-NA
error.na[17,8]<-NA
error.na[9,18]<-NA
error.na[18,9]<-NA
error.na[10,19]<-NA
error.na[19,10]<-NA
error.na[11,20]<-NA
error.na[20,11]<-NA
error.na[12,21]<-NA
error.na[21,12]<-NA
error.na[13,22]<-NA
error.na[22,13]<-NA
error.na[14,23]<-NA
error.na[23,14]<-NA
error.na[15,24]<-NA
error.na[24,15]<-NA
error.na[16,25]<-NA
error.na[25,16]<-NA
error.na[17,26]<-NA
error.na[26,17]<-NA
error.na[18,27]<-NA
error.na[27,18]<-NA
error.na[1,19]<-NA
error.na[19,1]<-NA
error.na[2,20]<-NA
error.na[20,2]<-NA
error.na[3,21]<-NA
error.na[21,3]<-NA
error.na[4,22]<-NA
error.na[22,4]<-NA
error.na[5,23]<-NA
error.na[23,5]<-NA
error.na[6,24]<-NA
error.na[24,6]<-NA
error.na[7,25]<-NA
error.na[25,7]<-NA
error.na[8,26]<-NA
error.na[26,8]<-NA
error.na[9,27]<-NA
error.na[27,9]<-NA
error.na

error.cor<-matrix(0,27,27)
diag(error.cor)<-1
error.cor[1,10]<-0.2
error.cor[10,1]<-0.2
error.cor[2,11]<-0.2
error.cor[11,2]<-0.2
error.cor[3,12]<-0.2
error.cor[12,3]<-0.2
error.cor[4,13]<-0.2
error.cor[13,4]<-0.2
error.cor[5,14]<-0.2
error.cor[14,5]<-0.2
error.cor[6,15]<-0.2
error.cor[15,6]<-0.2
error.cor[7,16]<-0.2
error.cor[16,7]<-0.2
error.cor[8,17]<-0.2
error.cor[17,8]<-0.2
error.cor[9,18]<-0.2
error.cor[18,9]<-0.2
error.cor[10,19]<-0.2
error.cor[19,10]<-0.2
error.cor[11,20]<-0.2
error.cor[20,11]<-0.2
error.cor[12,21]<-0.2
error.cor[21,12]<-0.2
error.cor[13,22]<-0.2
error.cor[22,13]<-0.2
error.cor[14,23]<-0.2
error.cor[23,14]<-0.2
error.cor[15,24]<-0.2
error.cor[24,15]<-0.2
error.cor[16,25]<-0.2
error.cor[25,16]<-0.2
error.cor[17,26]<-0.2
error.cor[26,17]<-0.2
error.cor[18,27]<-0.2
error.cor[27,18]<-0.2
error.cor[1,19]<-0.04
error.cor[19,1]<-0.04
error.cor[2,20]<-0.04
error.cor[20,2]<-0.04
error.cor[3,21]<-0.04
error.cor[21,3]<-0.04
error.cor[4,22]<-0.04
error.cor[22,4]<-0.04
error.cor[5,23]<-0.04
error.cor[23,5]<-0.04
error.cor[6,24]<-0.04
error.cor[24,6]<-0.04
error.cor[7,25]<-0.04
error.cor[25,7]<-0.04
error.cor[8,26]<-0.04
error.cor[26,8]<-0.04
error.cor[9,27]<-0.04
error.cor[27,9]<-0.04
error.cor

TE<-symMatrix(error.na,error.cor)
#summary(TE)

factor.na<-matrix(0,9,9)
diag(factor.na)<-NA
factor.na[2,1]<-NA
factor.na[3,1]<-NA
factor.na[3,2]<-NA
factor.na[5,4]<-NA
factor.na[6,4]<-NA
factor.na[6,5]<-NA
factor.na[8,7]<-NA
factor.na[9,7]<-NA
factor.na[9,8]<-NA
factor.na[1,2]<-NA
factor.na[1,3]<-NA
factor.na[2,3]<-NA
factor.na[4,5]<-NA
factor.na[4,6]<-NA
factor.na[5,6]<-NA
factor.na[7,8]<-NA
factor.na[7,9]<-NA
factor.na[8,9]<-NA
factor.na

factor.cor<-matrix(0,9,9)
diag(factor.cor)<-1
factor.cor[2,1]<-wtc
factor.cor[3,1]<-wtc
factor.cor[3,2]<-wtc
factor.cor[5,4]<-wtc
factor.cor[6,4]<-wtc
factor.cor[6,5]<-wtc
factor.cor[8,7]<-wtc
factor.cor[9,7]<-wtc
factor.cor[9,8]<-wtc
factor.cor[1,2]<-wtc
factor.cor[1,3]<-wtc
factor.cor[2,3]<-wtc
factor.cor[4,5]<-wtc
factor.cor[4,6]<-wtc
factor.cor[5,6]<-wtc
factor.cor[7,8]<-wtc
factor.cor[7,9]<-wtc
factor.cor[8,9]<-wtc
factor.cor


PS<-symMatrix(factor.na,factor.cor)
#summary(PS)

path.na<-matrix(0,9,9)
path.na[4,1]<-NA
path.na[5,2]<-NA
path.na[6,3]<-NA
path.na[7,4]<-NA
path.na[8,5]<-NA
path.na[9,6]<-NA
path.na[5,1]<-NA
path.na[6,2]<-NA
path.na[8,4]<-NA
path.na[9,5]<-NA
path.na

path.st<-matrix(0,9,9)

path.st[4,1]<-ar1
path.st[5,2]<-ar1
path.st[6,3]<-ar1
path.st[7,4]<-ar1
path.st[8,5]<-ar1
path.st[9,6]<-ar1
path.st[5,1]<-cl1
path.st[6,2]<-cl1
path.st[8,4]<-cl1
path.st[9,5]<-cl1
path.st

BE<-simMatrix(path.na,path.st)
#summary(BE)

#Need factor variance to be 1
VPS<-simVector(c(1, 1, 1, t2xvar, t2mvar, t2yvar, t2xvar, t2mvar, t2yvar))
#summary(VPS)


VTE<-simVector(rep(resvar,27))
#summary(VTE)

#Bind matrices together to specify model
SEM.model2<-simSetSEM(BE=BE,LY=LY,PS=PS,TE=TE,VPS=VPS,VTE=VTE)

#Set up data and model objects given to the simulation
Data.model2<-simData(500,SEM.model2)
SimModel2<-simModel(SEM.model2)
SimModel2@equalCon <- equal.loading 
SimModel2@param@PS[4,4] <- NA
SimModel2@param@PS[5,5] <- NA
SimModel2@param@PS[6,6] <- NA
SimModel2@param@PS[7,7] <- NA
SimModel2@param@PS[8,8] <- NA
SimModel2@param@PS[9,9] <- NA

#Put groups in the function to make things go smoother....
groups<- list(c(1, 4, 5, 7, 8), c(3), c(2, 6), c(9))

#Set up missing object, specify the parameters for missing data
#Start with 3 form design, always do 100 imps? Lets start with that.
missing1 <- simMissing(nforms=3, itemGroups=groups, timePoints=3, numImps=0)

#Run simulation. simResult generates data, runs model and saes results. Lets start by running 200 reps per conditions
results<-simResult(Data.model2,SimModel2, missing1, nRep=200, seed=MISseed, multicore=FALSE)
}
return(results)
}


#Factor loadings vary from .7 to .85 by .05 (e.g, .7, .75, .8, .85)
fl <- seq(0.70, .85, .05)
#Within time covariances vary from .2-.8 by .1 (e.g. .3, .4, .5, .6, .7, .8)
wtc <- seq(.2, .8, .1)

## Structural parameters. Currently relationships are the same for both variables

#First order autoregressive paths vary from .4 to .9 by .1
ar1 <- seq(.4, .9, .1)
#First order cross lagged paths vary from 0 to .5 by .1
cl1 <- seq(0, .5, .1)

##Create matrix of conditions
conds <- expand.grid(fl, wtc, ar1, cl1)
#conds <- matrix(c(.7,.8,.9,.4,.7,.2,.4,.3), byrow=TRUE, nrow=2)
#item groups determined which variables are in the x, a, b, and c blocks
#Need to vary this from 1 item in a b and c, to 1 item in X block.
groups<- list(c(1, 4, 5, 7, 8), c(3), c(2, 6), c(9))

##Set seed for this run
#MOD1seed <- 1234567

conds <- conds[312:325,]
runtime<- system.time(
testOUT <- apply(conds, MARGIN=1,runModel2)
)

#Run function over values of model parameters
runtime<- system.time(
output2 <- parRapply(cl, conds, runModel2) 
)

#Save results to an Rdata file name contains parameters in the simulation
save(list=c("output2", "groups", "conds", "runtime"), file=paste("tp3.var3", length(groups[[1]]), "RData", sep="."))

#Correctly exits R so cluster doesn't keep running forever!
stopCluster(cl)
mpi.quit()