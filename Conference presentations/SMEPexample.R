# Supplemental codes for the proposal for the Society of Multivariate Experimental Psychology conference 2012

# Taking into account sampling variability of model selection indices: A parametric bootstrap approach
# Sunthud Pornprasertmanit, Wei Wu, and Todd D. Little; University of Kansas
# Latest updated: July 1, 2012.

library(simsem)		# This code is applicable for simsem 0.2-7
N <- 300			# Sample Size
nRep <- 1000		# Number of bootstrap drawn
set.seed(123321)	# Set Seed Number

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

modelB <- simParamSEM(BE=BEB, PS=PSB, LY=LY, TE=TE, AL=AL, TY=TY)

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

modelC <- simParamSEM(BE=BEC, PS=PSC, LY=LY, TE=TE, AL=AL, TY=TY)

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

# Get the AIC and BIC difference
# The anova function will provide the AIC or BIC values of the first object subtracted by the second object.
# Thus, if the AIC or BIC difference is positive, the hypothesized model in the first object is preferred. 
# If the AIC or BIC difference is negative, the hypothesized model in the second object is preferred.

anova(outDA, outDB)
anova(outDA, outDC)
anova(outDA, outDD)
anova(outDB, outDC)
anova(outDB, outDD)
anova(outDC, outDD)

############################################################# 
#### Run a parametric bootstrap
#############################################################

########## Create the parametric bootstrap distributions

simAA <- runFit(outDA, nRep=nRep, data=datD, analyzeModel=analyzeA, multicore=TRUE)
simAB <- runFit(outDA, nRep=nRep, data=datD, analyzeModel=analyzeB, multicore=TRUE)
simAC <- runFit(outDA, nRep=nRep, data=datD, analyzeModel=analyzeC, multicore=TRUE)
simAD <- runFit(outDA, nRep=nRep, data=datD, analyzeModel=analyzeD, multicore=TRUE)
simBA <- runFit(outDB, nRep=nRep, data=datD, analyzeModel=analyzeA, multicore=TRUE)
simBB <- runFit(outDB, nRep=nRep, data=datD, analyzeModel=analyzeB, multicore=TRUE)
simBC <- runFit(outDB, nRep=nRep, data=datD, analyzeModel=analyzeC, multicore=TRUE)
simBD <- runFit(outDB, nRep=nRep, data=datD, analyzeModel=analyzeD, multicore=TRUE)
simCA <- runFit(outDC, nRep=nRep, data=datD, analyzeModel=analyzeA, multicore=TRUE)
simCB <- runFit(outDC, nRep=nRep, data=datD, analyzeModel=analyzeB, multicore=TRUE)
simCC <- runFit(outDC, nRep=nRep, data=datD, analyzeModel=analyzeC, multicore=TRUE)
simCD <- runFit(outDC, nRep=nRep, data=datD, analyzeModel=analyzeD, multicore=TRUE)
simDA <- runFit(outDD, nRep=nRep, data=datD, analyzeModel=analyzeA, multicore=TRUE)
simDB <- runFit(outDD, nRep=nRep, data=datD, analyzeModel=analyzeB, multicore=TRUE)
simDC <- runFit(outDD, nRep=nRep, data=datD, analyzeModel=analyzeC, multicore=TRUE)
simDD <- runFit(outDD, nRep=nRep, data=datD, analyzeModel=analyzeD, multicore=TRUE)

# The first argument is the output result used to generate data. 
# The nRep argument is the number of bootstrap samples.
# The data argument is the observed data
# The analyze model is the analysis model that is used in analyzing bootstrap samples
# The multicore is to use multi-processors in a computer, which will speed up the bootstrap analysis

########## Obtain p-values 

pValueNonNested(outDA, outDB, simAA, simAB, simBA, simBB)
pValueNonNested(outDA, outDC, simAA, simAC, simCA, simCC)
pValueNonNested(outDA, outDD, simAA, simAD, simDA, simDD)
pValueNonNested(outDB, outDC, simBB, simBC, simCB, simCC)
pValueNonNested(outDB, outDD, simBB, simBD, simDB, simDD)
pValueNonNested(outDC, outDD, simCC, simCD, simDC, simDD)

# The first argument is the original output from Model 1
# The second argument is the original output from Model 2
# The third argument is the simulation which generates data from Model 1 and analyzes data by Model 1
# The fourth argument is the simulation which generates data from Model 1 and analyzes data by Model 2
# The fifth argument is the simulation which generates data from Model 2 and analyzes data by Model 1
# The sixth argument is the simulation which generates data from Model 2 and analyzes data by Model 2

# In the output, the first object is the p-value comparing the original difference in a fit index with the sampling distribution of the difference in a fit index when bootstrap samples are generated from Model 1. This p-value is referred as p1.
# the second object is the p-value comparing the original difference in a fit index with the sampling distribution of the difference in a fit index when bootstrap samples are generated from Model 2. This p-value is referred as p2.
# Let's alpha as the alpha level. There are four scenarios:
#	1. p1 > alpha and p2 < alpha --> Model 1 is preferred
# 	2. p1 < alpha and p2 > alpha --> Model 2 is preferred
# 	3. p1 > alpha and p2 > alpha --> Model 1 and 2 are indistinguishable. The statistical power is not enough to discern the difference between two models.
# 	4. p1 < alpha and p2 < alpha --> Model 1 and 2 are indistinguishable. Model 1 and Model 2 maybe not the same as the population model underlying the original dataset.

