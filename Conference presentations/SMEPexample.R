# model1
# model2
# model3
# model4

N <- 300
nRep <- 1000
set.seed(123321)

############################################################# 
#### Create data generation model and analysis model D
#############################################################

# In the data generation model, the parameter values and the position of free parameters are needed. Therefore, the simMatrix, symMatrix, and simVector are needed to combine two things together. The first argument is the matrix containing the position of free parameters and fixed parameter values. The second elements contains the parameter values of all free parameters. Then, all simMatrix, symMatrix, and simVector are combined together.

##################### Model D ###############################

# BE matrix: path coefficient matrix with regression coefficient of 0.2 
pathD <- matrix(0, 9, 9)
pathD[2:4, 1] <- NA
pathD[5:7, 2] <- NA
pathD[7:8, 3] <- NA
pathD[8:9, 4] <- NA
BED <- simMatrix(pathD, 0.2)

# VPS vector: Residual factor variance of 1 and 0.8 (8 times)
VPSD <- simVector(rep(NA, 9), c(1, rep(0.8, 8)))

# RPS matrix: Factor correlation matrix with identity matrix (no factor correlation)
RPSD <- symMatrix(diag(9))

# AL vector: Factor intercepts of 0
ALgen <- simVector(rep(0, 9))

# LY matrix: Loading matrix as an identity matrix
LYgen <- simMatrix(diag(9))

# TE matrix: Error covariance as an zero matrix
TEgen <- symMatrix(matrix(0, 9, 9))

# TY vector: measurement intercept of 0
TYgen <- simVector(rep(NA, 9), 0)

# Combine all matrices and vectors for Model D
modelD <- simSetSEM(BE=BED, VPS=VPSD, RPS=RPSD, LY=LYgen, TE=TEgen, AL=ALgen, TY=TYgen)

# Create data template for Model D
dataD <- simData(modelD, N)

# Create analysis template for Model D
analyzeD <- simModel(modelD)

############################################################# 
#### Create analysis model A, B, and C
#############################################################

# In the analysis model, only free parameters are needed. Therefore, the matrices and vectors of free parameters (and fixed parameter values) can be used directly. Then, all matrices and vectors can be combined together.

##################### Model A ###############################

# BE matrix: path coefficient
BEA <- matrix(0, 9, 9)
BEA[3, 1:2] <- NA
BEA[4:9, 3] <- NA

# PS matrix: factor residual covariance matrix
PSA <- diag(NA, 9)
PSA[1, 2] <- PSA[2, 1] <- NA

# LY matrix: loading matrix --> identity matrix
LY <- diag(9)

# AL vector: factor mean vector
AL <- rep(0, 9)

# TE matrix: measurement error covariance --> zero matrix
TE <- matrix(0, 9, 9)

# TY vector: measurement intercept
TY <- rep(NA, 9)

# Combine all matrices and vectors for Model A
modelA <- simParamSEM(BE=BEA, PS=PSA, LY=LY, TE=TE, AL=AL, TY=TY)

# Create analysis template for Model D
analyzeA <- simModel(modelA)

##################### Model B ###############################

BEB <- matrix(0, 9, 9)
BEB[2, 1] <- NA
BEB[3, 2] <- NA
BEB[4, 2:3] <- NA
BEB[5, 3] <- NA
BEB[6, 2:3] <- NA
BEB[7, c(3, 4)] <- NA
BEB[8, 5] <- NA
BEB[9, c(3, 6)] <- NA

PSB <- diag(NA, 9)
PSB[7, 8] <- PSB[8, 7] <- NA
PSB[7, 9] <- PSB[9, 7] <- NA
PSB[8, 9] <- PSB[9, 8] <- NA

modelB <- simSetSEM(BE=BEB, PS=PSB, LY=LY, TE=TE, AL=AL, TY=TY)

# Create analysis template for Model D
analyzeB <- simModel(modelB)

##################### Model C ###############################

BEC <- matrix(0, 9, 9)
BEC[6, 3] <- NA
BEC[7, 4] <- NA
BEC[8, 5] <- NA
BEC[9, 6:8] <- NA

PSC <- diag(NA, 9)
PSC[1:5, 1:5] <- NA

modelC <- simSetSEM(BE=BEC, PS=PSC, LY=LY, TE=TE, AL=AL, TY=TY)

# Create analysis template for Model D
analyzeC <- simModel(modelC)

############################################################# 
#### Create data and analyze data
#############################################################

# Create data from Model D with a specified sample size
datD <- run(dataD, N)

# Analyze data by Model A, B, C, D
outDA <- run(analyzeA, datD)
outDB <- run(analyzeB, datD)
outDC <- run(analyzeC, datD)
outDD <- run(analyzeD, datD)

# Get summary from all analysis results
summary(outDA)
summary(outDB)
summary(outDC)
summary(outDD)

############################################################# 
#### Run a parametric bootstrap
#############################################################

# Run a simulation based on the result; the datD should not be necessary
simAA <- runFit(outDA, nRep=nRep, analyzeModel=analyzeA, multicore=TRUE)
simAB <- runFit(outDA, nRep=nRep, analyzeModel=analyzeB, multicore=TRUE)
simAC <- runFit(outDA, nRep=nRep, analyzeModel=analyzeC, multicore=TRUE)
simAD <- runFit(outDA, nRep=nRep, analyzeModel=analyzeD, multicore=TRUE)
simBA <- runFit(outDB, nRep=nRep, analyzeModel=analyzeA, multicore=TRUE)
simBB <- runFit(outDB, nRep=nRep, analyzeModel=analyzeB, multicore=TRUE)
simBC <- runFit(outDB, nRep=nRep, analyzeModel=analyzeC, multicore=TRUE)
simBD <- runFit(outDB, nRep=nRep, analyzeModel=analyzeD, multicore=TRUE)
simCA <- runFit(outDC, nRep=nRep, analyzeModel=analyzeA, multicore=TRUE)
simCB <- runFit(outDC, nRep=nRep, analyzeModel=analyzeB, multicore=TRUE)
simCC <- runFit(outDC, nRep=nRep, analyzeModel=analyzeC, multicore=TRUE)
simCD <- runFit(outDC, nRep=nRep, analyzeModel=analyzeD, multicore=TRUE)
simDA <- runFit(outDD, nRep=nRep, analyzeModel=analyzeA, multicore=TRUE)
simDB <- runFit(outDD, nRep=nRep, analyzeModel=analyzeB, multicore=TRUE)
simDC <- runFit(outDD, nRep=nRep, analyzeModel=analyzeC, multicore=TRUE)
simDD <- runFit(outDD, nRep=nRep, analyzeModel=analyzeD, multicore=TRUE)

########## ANOVA

anova.d.ab <- anova(outDA, outDB)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.ac <- anova(outDA, outDC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.ad <- anova(outDA, outDD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.bc <- anova(outDB, outDC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.bd <- anova(outDB, outDD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.cd <- anova(outDC, outDD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d <- c(anova.d.ab, anova.d.ac, anova.d.ad, anova.d.bc, anova.d.bd, anova.d.cd)
anova.d <- unlist(anova.d[c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)])
name <- expand.grid(c("ab", "ac", "ad", "bc", "bd", "cd"), c("AIC.d.", "BIC.d."))
names(anova.d) <- paste(name[,2], name[,1], sep="")

########## Monte Carlo Difference

mc.d.ab <- sapply(pValueNonNested(outDA, outDB, simAA, simAB, simBA, simBB), function(x) x[c("AIC")])
mc.d.ac <- sapply(pValueNonNested(outDA, outDC, simAA, simAC, simCA, simCC), function(x) x[c("AIC")])
mc.d.ad <- sapply(pValueNonNested(outDA, outDD, simAA, simAD, simDA, simDD), function(x) x[c("AIC")])
mc.d.bc <- sapply(pValueNonNested(outDB, outDC, simBB, simBC, simCB, simCC), function(x) x[c("AIC")])
mc.d.bd <- sapply(pValueNonNested(outDB, outDD, simBB, simBD, simDB, simDD), function(x) x[c("AIC")])
mc.d.cd <- sapply(pValueNonNested(outDC, outDD, simCC, simCD, simDC, simDD), function(x) x[c("AIC")])
mc.d <- c(mc.d.ab, mc.d.ac, mc.d.ad, mc.d.bc, mc.d.bd, mc.d.cd)
name <- expand.grid(c("mc1.d.", "mc2.d."), c("ab", "ac", "ad", "bc", "bd", "cd"))
names(mc.d) <- paste(name[,1], name[,2], sep="")

########## Monte Carlo Difference

lik.d.ab <- likRatioFit(outDA, outDB, simAA, simAB, simBA, simBB)[c("AIC")]
lik.d.ac <- likRatioFit(outDA, outDC, simAA, simAC, simCA, simCC)[c("AIC")]
lik.d.ad <- likRatioFit(outDA, outDD, simAA, simAD, simDA, simDD)[c("AIC")]
lik.d.bc <- likRatioFit(outDB, outDC, simBB, simBC, simCB, simCC)[c("AIC")]
lik.d.bd <- likRatioFit(outDB, outDD, simBB, simBD, simDB, simDD)[c("AIC")]
lik.d.cd <- likRatioFit(outDC, outDD, simCC, simCD, simDC, simDD)[c("AIC")]
lik.d <- c(lik.d.ab, lik.d.ac, lik.d.ad, lik.d.bc, lik.d.bd, lik.d.cd)
names(lik.d) <- paste("lik.d.", c("ab", "ac", "ad", "bc", "bd", "cd"), sep="")

fit <- NULL
target <- "AIC"
fit <- c(outAA@fit[target], 
outAB@fit[target], 
outAC@fit[target], 
outAD@fit[target], 
outBA@fit[target], 
outBB@fit[target], 
outBC@fit[target], 
outBD@fit[target], 
outCA@fit[target], 
outCB@fit[target], 
outCC@fit[target], 
outCD@fit[target], 
outDA@fit[target], 
outDB@fit[target], 
outDC@fit[target], 
outDD@fit[target])
target <- "BIC"
fit <- c(fit, outAA@fit[target], 
outAB@fit[target], 
outAC@fit[target], 
outAD@fit[target], 
outBA@fit[target], 
outBB@fit[target], 
outBC@fit[target], 
outBD@fit[target], 
outCA@fit[target], 
outCB@fit[target], 
outCC@fit[target], 
outCD@fit[target], 
outDA@fit[target], 
outDB@fit[target], 
outDC@fit[target], 
outDD@fit[target])
name <- expand.grid(c("A", "B", "C", "D"), c("A", "B", "C", "D"), c("AIC", "BIC"))
names(fit) <- paste(name[,3], name[,2], name[,1], sep=".")

result <- c(N=N, fit, anova.vec, mc.vec, lik.vec)

