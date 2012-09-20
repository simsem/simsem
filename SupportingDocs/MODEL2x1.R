### Models needes for simulations. MODEL 2 (3 timepoints, 3 variables)

#set working directory for use with HPC
#setwd('/crmda/workgroups/MDSimFiles/NSFGrant')

#simSEM must be installed before use!
#install.packages("/crmda/workgroups/MDSimFiles/NSFGrant/simsem_0.1-0.tar.gz", repos=NULL, type="source")


library(snow)

cl <- makeCluster(23, type="MPI")

## runModel2 = Function to put together model and run simualation
## Function arguments are parameters in the simulation
## Params = vector of parameters in model with order, fl wtc, ar1, cl1
## groups = list of item columns to go in x, a, b and c block
#require(simsem)

runModel2 <- function(params = c(.7, .1, .4, 0), MISseed=1234564) {
require(simsem)

fl <- as.numeric(params[1])
wtc <- as.numeric(params[2])
ar1 <- as.numeric(params[3])
cl1 <- as.numeric(params[4])

#Residual variances set them equal to 1
resvar <- 1 - fl^2

#Time specific variances: need to make each one equal 1.
t2xvar <- 1 - (ar1^2)
t2mvar <- 1 - (cl1^2 + ar1^2 + 2*(ar1 * wtc * cl1))
t2yvar <- 1 - (cl1^2 + ar1^2 + 2*(ar1 * wtc * cl1))

t3xvar <- 1 - (ar1^2)
t3mvar <- 1 - (cl1^2 + ar1^2 + 2*(ar1 * wtc * cl1))
t3yvar <- 1 - (cl1^2 + ar1^2 + 2*(ar1 * wtc * cl1))

##Return a NULL value if model specified is NPD
if(t2xvar < 0 || t2mvar < 0 || t2yvar < 0) {
results <- NULL
} else {


############################################################ Model Specification
########################################## LAMBDA matrix

makeLambda <- function(inpMat = NULL, nFac = 0, nTimes = 0, npf = 0, val = c()) {
	for (i in 1:(nFac * nTimes)) {
		inpMat[((npf * (i - 1)) + 1):(((npf * (i - 1)) + npf)), i] <- val
	}
	return(inpMat)
}

loading <- matrix(0, 27, 9)
loading <- makeLambda(loading, 3, 3, 3, NA)

load.val <- matrix(0, 27, 9)
load.val <- makeLambda(load.val, 3, 3, 3, fl)

LY <- simMatrix(loading, load.val)

########################################## THETA matrix

errorLag <- function(inpMat = NULL, nVar = 0, nTime = 0, nlag = 0, val = NULL) {
	i <- 1
	while (i <= ((nVar * nTime) - (nlag * nVar))) {
		inpMat[i, (i + (nlag * nVar))] <- val
		inpMat[(i + (nlag * nVar)), i] <- val
		i <- i + 1
	}
	return(inpMat)
}

error.na <- matrix(0, 27, 27)
diag(error.na) <- NA
error.na <- errorLag(error.na, 9, 3, 1, NA)
error.na <- errorLag(error.na, 9, 3, 2, NA)

error.cor <- matrix(0, 27, 27)
diag(error.cor) <- 1
error.cor <- errorLag(error.cor, 9, 3, 1, .2)
error.cor <- errorLag(error.cor, 9, 3, 2, .04)

TE <- symMatrix(error.na, error.cor)

########################################## PSI matrix

makePsi <- function(inpMat = NULL, nFac = 0, nTimes = 0, val = c()) {
	if (length(val) == 1) {
		val <- rep(val, nFac)
	}
	if (length(val) == nFac) {
		for (i in 1:nTimes) {
			inpMat[(1 + (nFac * (i - 1))), (2 + (nFac * (i - 1)))] <- val[1]
			inpMat[(2 + (nFac * (i - 1))), (1 + (nFac * (i - 1)))] <- val[1]
			inpMat[(1 + (nFac * (i - 1))), (3 + (nFac * (i - 1)))] <- val[2]
			inpMat[(3 + (nFac * (i - 1))), (1 + (nFac * (i - 1)))] <- val[2]
			inpMat[(3 + (nFac * (i - 1))), (2 + (nFac * (i - 1)))] <- val[3]
			inpMat[(2 + (nFac * (i - 1))), (3 + (nFac * (i - 1)))] <- val[3]
		}
	}
	if (length(val) != nFac) {
		paste("Cannot evaluate: unequal val= and nFac=")
	} else {
		return(inpMat)
	}
}

factor.na <- matrix(0, 9, 9)
diag(factor.na) <- NA
factor.na <- makePsi(factor.na, 3, 3, NA)

factor.cor <- matrix(0, 9, 9)
diag(factor.cor) <- 1
factor.cor <- makePsi(factor.cor, 3, 3, wtc)

PS <- symMatrix(factor.na, factor.cor)

########################################## BETA matrix

makeBeta <- function(inpMat = NULL, nFac = 0, nTimes = 0, val = c(x, m, y, a, b, cp)) {
	if (length(val) == 1) {
		val <- rep(val, 2*nFac)
	}
	if (length(val) == 2*nFac) {
		for (i in 2:nTimes) {
			inpMat[(1 + (nFac * (i - 1))), (1 + (nFac * (i - 2)))] <- val[1]
			inpMat[(2 + (nFac * (i - 1))), (2 + (nFac * (i - 2)))] <- val[2]
			inpMat[(3 + (nFac * (i - 1))), (3 + (nFac * (i - 2)))] <- val[3]
			inpMat[(2 + (nFac * (i - 1))), (1 + (nFac * (i - 2)))] <- val[4]
			inpMat[(3 + (nFac * (i - 1))), (2 + (nFac * (i - 2)))] <- val[5]
			inpMat[(3 + (nFac * (i - 1))), (1 + (nFac * (i - 2)))] <- val[6]
		}
	}
	if (length(val) != 2*nFac) {
		paste("Cannot evaluate: unequal val= and nFac=")
	} else {
		return(inpMat)
	}
}

path.na <- matrix(0, 9, 9)
path.na <- makeBeta(path.na, 3, 3, NA)

path.st <- matrix(0, 9, 9)
path.st <- makeBeta(path.st, 3, 3, c(ar1, ar1, ar1, cl1, cl1, 0))

BE <- simMatrix(path.na, path.st)
#summary(BE)

VPS <- simVector(c(1, 1, 1, t2xvar, t2mvar, t2yvar, t3xvar, t3mvar, t3yvar))
#summary(VPS)

VTE <- simVector(rep(resvar, 27))
#summary(VTE)

##Equality constraints

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


#Bind matrices together to specify model
SEM.model2 <- simSetSEM(BE=BE, LY=LY, RPS=PS, RTE=TE, VPS=VPS, VTE=VTE)

#Set up data and model objects given to the simuation
Data.model2 <- simData(SEM.model2, 200)
SimModel2 <- simModel(SEM.model2)

SimModel2@equalCon <- equal.loading 

#Free t2-t3 factor variances b/c of weak factorial invariance
SimModel2@param@PS[4,4] <- NA
SimModel2@param@PS[5,5] <- NA
SimModel2@param@PS[6,6] <- NA
SimModel2@param@PS[7,7] <- NA
SimModel2@param@PS[8,8] <- NA
SimModel2@param@PS[9,9] <- NA


## item groups determined which variables are in the x, a, b, and c blocks
## Vary this from 1 item in a b and c, to 1 item in X block.  Uncomment 1 for each run
#groups <- list(c(c(1, 10, 19),c(2, 11, 20),c(4, 13, 22),c(5, 14, 23),c(7, 16, 25),c(8, 17, 26)),c(3, 12, 21),c(6, 15, 24),c(9, 18, 27))
#groups <- list(c(c(1, 10, 19),c(4, 13, 22),c(5, 14, 23),c(7, 16, 25),c(8, 17, 26)),c(3, 12, 21),c(6, 15, 24),c(c(2, 11, 20),c(9, 18, 27)))
#groups <- list(c(c(1, 10, 19),c(4, 13, 22),c(7, 16, 25),c(8, 17, 26)),c(c(3, 12, 21),c(5, 14, 23)),c(6, 15, 24),c(c(2, 11, 20),c(9, 18, 27)))
#groups <- list(c(c(1, 10, 19),c(4, 13, 22),c(7, 16, 25)),c(c(3, 12, 21),c(5, 14, 23)),c(c(6, 15, 24),c(8, 17, 26)),c(c(2, 11, 20),c(9, 18, 27)))
#groups <- list(c(c(1, 10, 19),c(4, 13, 22)),c(c(3, 12, 21),c(5, 14, 23),c(7, 16, 25)),c(c(6, 15, 24),c(8, 17, 26)),c(c(2, 11, 20),c(9, 18, 27)))
groups <- list(c(1, 10, 19), c(c(3, 12, 21),c(5, 14, 23),c(7, 16, 25)), c(c(6, 15, 24),c(8, 17, 26)), c(c(2, 11, 20),c(4, 13, 22),c(9, 18, 27)))

#Set up missing object, specify the parameters for missing data
#Start with 3 form design. Lets start with FIML.
missing2 <- simMissing(nforms=3, itemGroups=groups, timePoints=3, numImps=0)

#Run simulation. simResult generates data, runs model and saes results. Lets start by running 200 reps per conditions
results <- try(simResult(Data.model2, SimModel2, missing2, nRep=200, seed=MISseed, multicore=FALSE), silent=FALSE)
}
return(results)
}

## end function

#Factor loadings  
fl <- seq(0.7, 0.85, .05)
#Within time covariances 
wtc <- seq(.2, .5, .1)

## Structural parameters. Currently relationships are the same for both variables

## First order autoregressive paths 
ar1 <- seq(.4, .9, .1)
#First order cross lagged paths 
cl1 <- seq(0, .4, .1)

## Create matrix of conditions
conds <- expand.grid(fl, wtc, ar1, cl1)

## Change this here and in function above
#groups <- list(c(c(1, 10, 19),c(2, 11, 20),c(4, 13, 22),c(5, 14, 23),c(7, 16, 25),c(8, 17, 26)),c(3, 12, 21),c(6, 15, 24),c(9, 18, 27))
#groups <- list(c(c(1, 10, 19),c(4, 13, 22),c(5, 14, 23),c(7, 16, 25),c(8, 17, 26)),c(3, 12, 21),c(6, 15, 24),c(c(2, 11, 20),c(9, 18, 27)))
#groups <- list(c(c(1, 10, 19),c(4, 13, 22),c(7, 16, 25),c(8, 17, 26)),c(c(3, 12, 21),c(5, 14, 23)),c(6, 15, 24),c(c(2, 11, 20),c(9, 18, 27)))
#groups <- list(c(c(1, 10, 19),c(4, 13, 22),c(7, 16, 25)),c(c(3, 12, 21),c(5, 14, 23)),c(c(6, 15, 24),c(8, 17, 26)),c(c(2, 11, 20),c(9, 18, 27)))
#groups <- list(c(c(1, 10, 19),c(4, 13, 22)),c(c(3, 12, 21),c(5, 14, 23),c(7, 16, 25)),c(c(6, 15, 24),c(8, 17, 26)),c(c(2, 11, 20),c(9, 18, 27)))
groups <- list(c(1, 10, 19), c(c(3, 12, 21),c(5, 14, 23),c(7, 16, 25)), c(c(6, 15, 24),c(8, 17, 26)), c(c(2, 11, 20),c(4, 13, 22),c(9, 18, 27)))

##Set seed for this run
#MOD2seed <- 1234567

#clusterExport(cl, groups, MOD4seed)

#Run function over values of model parameters
runtime <- system.time(
output2 <- parRapply(cl, conds, runModel2)
)

#Save results to an Rdata file name contains parameters in the simulation
save(list=c("output2", "groups", "conds", "runtime"), file=paste("tp3.var3.n200.f", (length(groups[[1]]) / 5), "RData", sep="."))

stopCluster(cl)
mpi.quit()
