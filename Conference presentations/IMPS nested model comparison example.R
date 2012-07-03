# Supplemental codes for the presentation at the International Meeting of the Psychometric Society, Lincoln, NE

# Using a Parametric Boostrap Approach for Nested Model Comparisons in Structural Equation Modeling
# Sunthud Pornprasertmanit, Wei Wu, and Todd D. Little; University of Kansas
# Latest updated: July 1, 2012.

library(simsem)
N <- 300			# Sample Size
nRep <- 1000		# Number of bootstrap samples
set.seed(123321)	# Set seed number
mis <- -0.7 		# Amount of actual misspecification
trivialMis <- -0.1	# Amount of defined trivial misspecification

############################################################# 
#### Create data generation models and analysis models
#############################################################

# In the data generation model, the parameter values and the position of free parameters are needed. Therefore, the simMatrix, symMatrix, and simVector are needed to combine two things together. The first argument is the matrix containing the position of free parameters and fixed parameter values. The second elements contains the parameter values of all free parameters. Then, all simMatrix, symMatrix, and simVector are combined together.

# LY matrix: factor loading matrix with factor loading values of 1
loading <- matrix(0, 9, 3)
loading[1, 1] <- 1
loading[2:3, 1] <- NA
loading[4, 2] <- 1
loading[5:6, 2] <- NA
loading[7, 3] <- 1
loading[8:9, 3] <- NA
LY <- simMatrix(loading, 1)

# RPS matrix: factor correlation matrix 
facCor <- matrix(NA, 3, 3)
diag(facCor) <- 1
facCorVal <- diag(3)
facCorVal[1, 2] <- facCorVal[2, 1] <- 0.7		# One timepoint lag correlation between factors = 0.7
facCorVal[2, 3] <- facCorVal[3, 2] <- 0.7
facCorVal[1, 3] <- facCorVal[3, 1] <- 0.49		# Two timepoints lag correlation between factors = 0.7
RPS <- symMatrix(facCor, facCorVal)

# VE vector: factor variances
VE <- simVector(rep(NA, 3), c(1, 1.2, 1.4))		# Variance of the factor in Time 1, 2, and 3 are 1, 1.2, and 1.4

# RTE matrix: measurement error correlations
error <- diag(9)
error[1, 4] <- error[4, 7] <- error[4, 1] <- error[7, 4] <- NA
error[2, 5] <- error[5, 8] <- error[5, 2] <- error[8, 5] <- NA
error[3, 6] <- error[6, 9] <- error[6, 3] <- error[9, 6] <- NA
error[1, 7] <- error[7, 1] <- NA
error[2, 8] <- error[8, 2] <- NA
error[3, 9] <- error[9, 3] <- NA
errorVal <- diag(9)
errorVal[1, 4] <- errorVal[4, 7] <- errorVal[4, 1] <- errorVal[7, 4] <- 0.2	# One timepoint lag correlation between measurement errors = 0.2
errorVal[2, 5] <- errorVal[5, 8] <- errorVal[5, 2] <- errorVal[8, 5] <- 0.2
errorVal[3, 6] <- errorVal[6, 9] <- errorVal[6, 3] <- errorVal[9, 6] <- 0.2
errorVal[1, 7] <- errorVal[7, 1] <- 0.04 	# Two timepoint lag correlation between measurement errors = 0.04
errorVal[2, 8] <- errorVal[8, 2] <- 0.04
errorVal[3, 9] <- errorVal[9, 3] <- 0.04
RTE <- symMatrix(error, errorVal)

# VTE vector: measurement error variances
VTE <- simVector(rep(NA, 9), 0.4)	# Measurement error variances = 0.04

# Combine all matrices and vectors for the longitudinal CFA model
longCFA <- simSetCFA(LY=LY, RPS=RPS, VE=VE, RTE=RTE, VTE=VTE)

# Making the equality constraints for the nested model
con1 <- matrix(0, 3, 2)
con1[1,] <- c(2, 1)
con1[2,] <- c(5, 2)
con1[3,] <- c(8, 3)
rownames(con1) <- rep("LY", 3)	# Constraint between the second loading on a factor in all timepoints
con2 <- matrix(0, 3, 2)
con2[1,] <- c(3, 1)
con2[2,] <- c(6, 2)
con2[3,] <- c(9, 3)
rownames(con2) <- rep("LY", 3)	# Constraint between the third loading on a factor in all timepoints
equalCon <- simEqualCon(con1, con2, modelType="CFA")	# Combine the constraints together

# Making the misspecification object to represent the actual misspecification imposed to the generated dataset
loadingMis <- matrix(0, 9, 3)
loadingMis[8:9, 3] <- NA
LYMis <- simMatrix(loadingMis, mis) 	# The second and third factor loadings on the factor at Time 3 dropped in the amount specified in the mis object
longCFAMis <- simMisspecCFA(LY=LYMis)

# Create data generation template from the nested model (because the equality constraint is imposed)
dataTemplate <- simData(longCFA, N, misspec=longCFAMis, equalCon=equalCon)

# Create a single dataset
dat <- run(dataTemplate)

# Create the analysis template for the nested and parent models
analyzeNested <- simModel(longCFA, equalCon=equalCon)
analyzeParent <- simModel(longCFA)

############################################################# 
#### Actual Data Analysis and Nested Model Comparison
#############################################################

# Analyze the dataset by nested and parent models
outNested <- run(analyzeNested, dat)
outParent <- run(analyzeParent, dat)

# Investigate the Chi-square difference test and CFI difference
anova(outNested, outParent)

############################################################# 
#### Create trivial misspecification (fixed, random, maximal)
#############################################################

# Fixed method of trivial misspecification

loadingMis2 <- matrix(0, 9, 3)
loadingMis2[8:9, 3] <- NA
LYMis2 <- simMatrix(loadingMis2, trivialMis)	# Drop in the amount of trivial misspecification
misspec2 <- simMisspecCFA(LY=LYMis2)

# Uniform method of trivial misspecification

unifMis <- simUnif(-abs(trivialMis), abs(trivialMis))	# Create random uniform distribution object with lower and upper bounds of negative and positive amount of defined trivial misspecification
loadingMis3 <- matrix(0, 9, 3)
loadingMis3[2:3, 1] <- NA
loadingMis3[5:6, 2] <- NA
loadingMis3[8:9, 3] <- NA
LYMis3 <- simMatrix(loadingMis3, unifMis)
misspec3 <- simMisspecCFA(LY=LYMis3)

# Maximal method of trivial misspecification

loadingMis4 <- matrix(0, 9, 3)
loadingMis4[2:3, 1] <- NA
loadingMis4[5:6, 2] <- NA
loadingMis4[8:9, 3] <- NA
LYMis4 <- simMatrix(loadingMis4, unifMis)
misspec4 <- simMisspecCFA(LY=LYMis4, optMisfit="max", numIter=100, misBeforeFill=FALSE)	# The optMisfit = "max" is used to find the combination of misspecified parameters that provides maximum misfit.

############################################################# 
#### Run a parametric bootstrap
#############################################################

##### Create generate bootstrap samples based on nested model and analyze by the nested model
simNestedNoMisspec <- runFit(model=analyzeNested, data=dat, nRep=nRep, analyzeModel=analyzeNested, multicore=TRUE)
simNestedFixed <- runFit(model=analyzeNested, data=dat, nRep=nRep, analyzeModel=analyzeNested, misspec=misspec2, multicore=TRUE)
simNestedRandom <- runFit(model=analyzeNested, data=dat, nRep=nRep, analyzeModel=analyzeNested, misspec=misspec3, multicore=TRUE)
simNestedMaximal <- runFit(model=analyzeNested, data=dat, nRep=nRep, analyzeModel=analyzeNested, misspec=misspec4, multicore=TRUE)

# model = model used for bootstrap sample generation
# data = original data
# nRep = the number of replications
# analyzeModel = the model used to analyze the bootstrap samples
# misspec = the trivial misspecification
# multicore = allowing multicore processing or not

##### Create generate bootstrap samples based on nested model and analyze by the parent model
simParentNoMisspec <- runFit(model=analyzeNested, data=dat, nRep=nRep, analyzeModel=analyzeParent, multicore=TRUE)
simParentFixed <- runFit(model=analyzeNested, data=dat, nRep=nRep, analyzeModel=analyzeParent, misspec=misspec2, multicore=TRUE)
simParentRandom <- runFit(model=analyzeNested, data=dat, nRep=nRep, analyzeModel=analyzeParent, misspec=misspec3, multicore=TRUE)
simParentMaximal <- runFit(model=analyzeNested, data=dat, nRep=nRep, analyzeModel=analyzeParent, misspec=misspec4, multicore=TRUE)

########## Obtain p-values 

pValueNested(outNested, outParent, simNestedNoMisspec, simParentNoMisspec)
pValueNested(outNested, outParent, simNestedFixed, simParentFixed)
pValueNested(outNested, outParent, simNestedRandom, simParentRandom)
pValueNested(outNested, outParent, simNestedMaximal, simParentMaximal)

# The first argumenet is the original output analyzed by the nested model
# The second argument is the original output analyzed by the parent model
# The third argument is the bootstrap simulation which the bootstrap samples are generated by the nested model and analyzed by the nested model
# The fourth argument is the bootstrap simulation which the bootstrap samples are generated by the nested model and analyzed by the parent model

# If p > .05, the nested model is preferred. If p < .05, the parent model is preferred.
