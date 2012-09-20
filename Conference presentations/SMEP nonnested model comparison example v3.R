# Supplemental codes for the proposal for the Society of Multivariate Experimental Psychology conference 2012

# Taking into account sampling variability of model selection indices: A parametric bootstrap approach
# Sunthud Pornprasertmanit, Wei Wu, and Todd D. Little; University of Kansas
# Latest updated: Septempber 19, 2012.

library(simsem)		# This code is applicable for simsem 0.3-5 or later
n <- 300			# Sample Size
nRep <- 1000		# Number of bootstrap drawn
set.seed(123321)	# Set Seed Number

############################################################# 
#### Create data generation model and analysis model D
#############################################################

# In the data generation model, the parameter values and the position of free parameters are needed. Therefore, the bind or binds (for symmetric matrix) functions are needed to combine two things together. The first argument is the matrix or vector containing the position of free parameters and fixed parameter values. The second elements contains the parameter values of all free parameters. 

##################### Model D ###############################

# BE matrix: path coefficient matrix with regression coefficient of 0.2 
pathD <- matrix(0, 9, 9)
pathD[2:4, 1] <- NA
pathD[5:7, 2] <- NA
pathD[7:8, 3] <- NA
pathD[8:9, 4] <- NA
BED <- bind(pathD, 0.2)

# VPS vector: Residual factor variance of 1 and 0.8 (8 times)
VPSD <- bind(rep(NA, 9), c(1, rep(0.8, 8)))

# RPS matrix: Factor correlation matrix with identity matrix (no factor correlation)
RPSD <- binds(diag(9))

# AL vector: Factor intercepts of 0
ALgen <- bind(rep(NA, 9), 0)

# The model function is used to combined different pieces to build a path analysis model
modelD <- model(BE=BED, VPS=VPSD, RPS=RPSD, AL=ALgen, modelType="Path")

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

# AL vector: factor mean vector
AL <- rep(NA, 9)

# Combine all matrices and vectors for Model A
modelA <- estmodel(BE=BEA, PS=PSA, AL=AL, modelType="Path")

##################### Model B ###############################

# BE matrix: path coefficient
BEB <- matrix(0, 9, 9)
BEB[2, 1] <- NA
BEB[3, 2] <- NA
BEB[4, 2:3] <- NA
BEB[5, 3] <- NA
BEB[6, 2:3] <- NA
BEB[7, c(3, 4)] <- NA
BEB[8, 5] <- NA
BEB[9, c(3, 6)] <- NA

# PS matrix: factor residual covariance matrix
PSB <- diag(NA, 9)
PSB[7, 8] <- PSB[8, 7] <- NA
PSB[7, 9] <- PSB[9, 7] <- NA
PSB[8, 9] <- PSB[9, 8] <- NA

# Combine all matrices and vectors for Model B
modelB <- estmodel(BE=BEB, PS=PSB, AL=AL, modelType="Path")

##################### Model C ###############################

# BE matrix: path coefficient
BEC <- matrix(0, 9, 9)
BEC[6, 3] <- NA
BEC[7, 4] <- NA
BEC[8, 5] <- NA
BEC[9, 6:8] <- NA

# PS matrix: factor residual covariance matrix
PSC <- diag(NA, 9)
PSC[1:5, 1:5] <- NA

# Combine all matrices and vectors for Model C
modelC <- estmodel(BE=BEC, PS=PSC, AL=AL, modelType="Path")

############################################################# 
#### Create data and analyze data
#############################################################

# Create data from Model D with a specified sample size
datD <- generate(modelD, n)

# Analyze data by Model A, B, C, D
outDA <- analyze(modelA, datD)
outDB <- analyze(modelB, datD)
outDC <- analyze(modelC, datD)
outDD <- analyze(modelD, datD)

# Get summary from all analysis results
summary(outDA, fit=TRUE)
summary(outDB, fit=TRUE)
summary(outDC, fit=TRUE)
summary(outDD, fit=TRUE)

# Get the AIC and BIC difference
# The anova function will provide the AIC or BIC values of the first object and the second objects.

anova(outDA, outDB)
anova(outDA, outDC)
anova(outDA, outDD)
anova(outDB, outDC)
anova(outDB, outDD)
anova(outDC, outDD)

############################################################# 
#### Run a parametric bootstrap
#############################################################

########## Create data generation model from parameter estimates from the real data

genA <- model.lavaan(outDA)
genB <- model.lavaan(outDB)
genC <- model.lavaan(outDC)
genD <- model.lavaan(outDD)

########## Create a Monte Carlo simulation based on the parameter estimates (parametric bootstrap)

simAA <- sim(nRep=nRep, n=n, model=modelA, generate=genA, multicore=TRUE)
simAB <- sim(nRep=nRep, n=n, model=modelB, generate=genA, multicore=TRUE)
simAC <- sim(nRep=nRep, n=n, model=modelC, generate=genA, multicore=TRUE)
simAD <- sim(nRep=nRep, n=n, model=modelD, generate=genA, multicore=TRUE)
simBA <- sim(nRep=nRep, n=n, model=modelA, generate=genB, multicore=TRUE)
simBB <- sim(nRep=nRep, n=n, model=modelB, generate=genB, multicore=TRUE)
simBC <- sim(nRep=nRep, n=n, model=modelC, generate=genB, multicore=TRUE)
simBD <- sim(nRep=nRep, n=n, model=modelD, generate=genB, multicore=TRUE)
simCA <- sim(nRep=nRep, n=n, model=modelA, generate=genC, multicore=TRUE)
simCB <- sim(nRep=nRep, n=n, model=modelB, generate=genC, multicore=TRUE)
simCC <- sim(nRep=nRep, n=n, model=modelC, generate=genC, multicore=TRUE)
simCD <- sim(nRep=nRep, n=n, model=modelD, generate=genC, multicore=TRUE)
simDA <- sim(nRep=nRep, n=n, model=modelA, generate=genD, multicore=TRUE)
simDB <- sim(nRep=nRep, n=n, model=modelB, generate=genD, multicore=TRUE)
simDC <- sim(nRep=nRep, n=n, model=modelC, generate=genD, multicore=TRUE)
simDD <- sim(nRep=nRep, n=n, model=modelD, generate=genD, multicore=TRUE)

# nRep = Number of replications
# n = Sample size
# model = The analysis model
# generate = The data generation model
# multicore = use multi-processors in a computer, which will speed up the bootstrap analysis

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

