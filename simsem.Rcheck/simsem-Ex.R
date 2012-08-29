pkgname <- "simsem"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('simsem')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("SimDataDist-class")
### * SimDataDist-class

flush(stderr()); flush(stdout())

### Name: SimDataDist-class
### Title: Class '"SimDataDist"'
### Aliases: SimDataDist-class summary,SimDataDist-method
###   run,SimDataDist-method plotDist,SimDataDist-method
###   extract,SimDataDist-method
### Keywords: classes

### ** Examples

# Need to be fixed

showClass("SimDataDist")

chisq3 <- simChisq(3)
chisq8 <- simChisq(8)
dist <- simDataDist(chisq3, chisq8)

m <- c(0, 0)
cm <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
n <- 20
# dat <- run(dist, n, m, cm)

plotDist(dist, r=0.2)




cleanEx()
nameEx("SimFunction-class")
### * SimFunction-class

flush(stderr()); flush(stdout())

### Name: SimFunction-class
### Title: Class '"SimFunction"'
### Aliases: SimFunction-class summary,SimFunction-method
###   run,SimFunction-method
### Keywords: classes

### ** Examples

# The example still does not work

#showClass("SimFunction")

#n65 <- simNorm(0.6, 0.05)
#u35 <- simUnif(0.3, 0.5)
#u68 <- simUnif(0.6, 0.8)
#u2 <- simUnif(-0.2, 0.2)
#n1 <- simNorm(0, 0.1)

#loading <- matrix(0, 9, 3)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:9, 3] <- NA
#loading.start <- matrix("", 9, 3)
#loading.start[1:3, 1] <- 0.7
#loading.start[4:6, 2] <- 0.7
#loading.start[7:9, 3] <- "u68"
#LY <- simMatrix(loading, loading.start)

#RTE <- symMatrix(diag(9))

#factor.cor <- diag(3)
#factor.cor[1, 2] <- factor.cor[2, 1] <- NA
#RPS <- symMatrix(factor.cor, 0.5)

#path <- matrix(0, 3, 3)
#path[3, 1:2] <- NA
#path.start <- matrix(0, 3, 3)
#path.start[3, 1] <- "n65"
#path.start[3, 2] <- "u35"
#BE <- simMatrix(path, path.start)

#datGen <- simSetSEM(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

#loading.trivial <- matrix(NA, 9, 3)
#loading.trivial[is.na(loading)] <- 0
#LY.trivial <- simMatrix(loading.trivial, "u2")

#error.cor.trivial <- matrix(NA, 9, 9)
#diag(error.cor.trivial) <- 0
#RTE.trivial <- symMatrix(error.cor.trivial, "n1")

#misGen <- simMisspecSEM(LY = LY.trivial, RTE = RTE.trivial)

#Data.Mis <- simData(datGen, 300, misspec=misGen)

#loading <- matrix(0, 12, 4)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:9, 4] <- NA
#loading[10:12, 3] <- NA

#path <- matrix(0, 4, 4)
#path[4, 1:3] <- NA

#analysis <- simParamSEM(BE=path, LY=loading)

#Model <- simModel(analysis)

# Find the products of indicators
#newFUN <- function(data, var1, var2, namesProd) {
#	prod <- data[,var1] * data[,var2]
#	colnames(prod) <- namesProd
#	return(data.frame(data, prod))
#}

#fun <- simFunction(newFUN, var1=paste("y", 1:3, sep=""), var2=paste("y", 4:6, sep=""), namesProd=paste("y", 10:12, sep=""))

# Real simulation will need more than just 10 replications
#Output <- simResult(10, Data.Mis, Model, objFunction=fun)
#summary(Output)

# Example of using the simfunction
#mc <- simFunction(newFUN, var1=1:3, var2=4:6, namesProd=paste("y", 10:12, sep=""))
#run(mc, attitude[,-1])
#summary(mc)



cleanEx()
nameEx("SimMatrix-class")
### * SimMatrix-class

flush(stderr()); flush(stdout())

### Name: SimMatrix-class
### Title: Matrix object: Random parameters matrix
### Aliases: SimMatrix-class run,SimMatrix-method
###   summaryShort,SimMatrix-method summary,SimMatrix-method
### Keywords: classes

### ** Examples

showClass("SimMatrix")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- bind(loading, loadingValues)
summary(LX)
# run(LX)

LY <- bind(loading, "rnorm(1, 0.6, 0.05)")
summary(LY)
# run(LY)



cleanEx()
nameEx("SimMissing-class")
### * SimMissing-class

flush(stderr()); flush(stdout())

### Name: SimMissing-class
### Title: Class '"SimMissing"'
### Aliases: SimMissing-class summary,SimMissing-method
###   run,SimMissing-method
### Keywords: classes

### ** Examples

# No Example



cleanEx()
nameEx("SimResult-class")
### * SimResult-class

flush(stderr()); flush(stdout())

### Name: SimResult-class
### Title: Class '"SimResult"'
### Aliases: SimResult-class summary,SimResult-method
###   summaryPopulation,SimResult-method getPopulation,SimResult-method
###   setPopulation,SimResult,data.frame-method
###   setPopulation,SimResult,SimSet-method
###   setPopulation,SimResult,VirtualRSet-method
### Keywords: classes

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- bind(loading, 0.7)
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)
summary(Output)
getCutoff(Output, 0.05)
summaryParam(Output)
summaryPopulation(Output)
param <- getPopulation(Output)
Output <- setPopulation(Output, param)



cleanEx()
nameEx("SimSem-class")
### * SimSem-class

flush(stderr()); flush(stdout())

### Name: SimSem-class
### Title: Class '"SimSem"'
### Aliases: SimSem-class summary,SimSem-method

### ** Examples

showClass("SimSem")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- bind(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- binds(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- binds(error.cor)

CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")
summary(CFA.Model)
#run(CFA.Model)

#CFA.Model2 <- extract(CFA.Model, y=1:3, e=1)
#summary(CFA.Model2)



cleanEx()
nameEx("SimVector-class")
### * SimVector-class

flush(stderr()); flush(stdout())

### Name: SimVector-class
### Title: Vector object: Random parameters vector
### Aliases: SimVector-class run,SimVector-method
###   summaryShort,SimVector-method summary,SimVector-method
###   extract,SimVector-method
### Keywords: classes

### ** Examples

showClass("SimVector")

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- bind(factor.mean, factor.mean.starting)
#run(AL)
summary(AL)
summaryShort(AL)




cleanEx()
nameEx("VirtualDist-class")
### * VirtualDist-class

flush(stderr()); flush(stdout())

### Name: VirtualDist-class
### Title: Distribution Objects
### Aliases: VirtualDist-class SimBeta-class SimBinom-class SimCauchy-class
###   SimChisq-class SimExp-class SimF-class SimGamma-class SimGeom-class
###   SimHyper-class SimLnorm-class SimLogis-class SimNbinom-class
###   SimNorm-class SimPois-class SimT-class SimUnif-class SimWeibull-class
###   run,SimBeta-method run,SimBinom-method run,SimCauchy-method
###   run,SimChisq-method run,SimExp-method run,SimF-method
###   run,SimGamma-method run,SimGeom-method run,SimHyper-method
###   run,SimLnorm-method run,SimLogis-method run,SimNbinom-method
###   run,SimNorm-method run,SimPois-method run,SimT-method
###   run,SimUnif-method run,SimWeibull-method summary,SimBeta-method
###   summary,SimBinom-method summary,SimCauchy-method
###   summary,SimChisq-method summary,SimExp-method summary,SimF-method
###   summary,SimGamma-method summary,SimGeom-method
###   summary,SimHyper-method summary,SimLnorm-method
###   summary,SimLogis-method summary,SimNbinom-method
###   summary,SimNorm-method summary,SimPois-method summary,SimT-method
###   summary,SimUnif-method summary,SimWeibull-method
###   summaryShort,SimBeta-method summaryShort,SimBinom-method
###   summaryShort,SimCauchy-method summaryShort,SimChisq-method
###   summaryShort,SimExp-method summaryShort,SimF-method
###   summaryShort,SimGamma-method summaryShort,SimGeom-method
###   summaryShort,SimHyper-method summaryShort,SimLnorm-method
###   summaryShort,SimLogis-method summaryShort,SimNbinom-method
###   summaryShort,SimNorm-method summaryShort,SimPois-method
###   summaryShort,SimT-method summaryShort,SimUnif-method
###   summaryShort,SimWeibull-method toFunction,SimBeta-method
###   toFunction,SimBinom-method toFunction,SimCauchy-method
###   toFunction,SimChisq-method toFunction,SimExp-method
###   toFunction,SimF-method toFunction,SimGamma-method
###   toFunction,SimGeom-method toFunction,SimHyper-method
###   toFunction,SimLnorm-method toFunction,SimLogis-method
###   toFunction,SimNbinom-method toFunction,SimNorm-method
###   toFunction,SimPois-method toFunction,SimT-method
###   toFunction,SimUnif-method toFunction,SimWeibull-method
###   plotDist,VirtualDist-method skew,VirtualDist-method
###   kurtosis,VirtualDist-method
### Keywords: classes

### ** Examples

showClass("VirtualDist")
u1 <- simUnif(0, 1)
chi3 <- simChisq(3)
summary(chi3)
skew(chi3)
kurtosis(chi3)
plotDist(chi3)
plotDist(chi3, reverse=TRUE)



cleanEx()
nameEx("analyze")
### * analyze

flush(stderr()); flush(stdout())

### Name: analyze
### Title: TBA
### Aliases: analyze

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

dat <- generate(CFA.Model,200)
out <- analyze(CFA.Model,dat)



cleanEx()
nameEx("anova")
### * anova

flush(stderr()); flush(stdout())

### Name: anova
### Title: Provide a comparison of nested models and nonnested models
###   across replications
### Aliases: anova,SimResult-method

### ** Examples

loading1 <- matrix(0, 6, 1)
loading1[1:6, 1] <- NA
loading2 <- loading1
loading2[6,1] <- 0
LX1 <- bind(loading1, 0.7)
LX2 <- bind(loading2, 0.7)
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model1 <- model(LY = LX1, RPS = RPH, RTE = RTD, modelType="CFA")
CFA.Model2 <- model(LY = LX2, RPS = RPH, RTE = RTD, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
# Need to make sure that both simResult calls have the same seed!
Output1 <- sim(5, n=500, model=CFA.Model1, generate=CFA.Model1, seed=123567)
Output2 <- sim(5, n=500, model=CFA.Model2, generate=CFA.Model1, seed=123567)
anova(Output1, Output2)

Output1b <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model1, generate=CFA.Model1, seed=123567)
Output2b <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model2, generate=CFA.Model1, seed=123567)
anova(Output1b, Output2b)



cleanEx()
nameEx("bind")
### * bind

flush(stderr()); flush(stdout())

### Name: bind
### Title: Specify matrices for Monte Carlo simulation of structural
###   equation models
### Aliases: bind binds

### ** Examples


loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LY <- bind(loading, loadingValues)
summary(LY)

# Set both factor correlations to .05
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

# Misspecify all error covarainces
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- NA
RTE <- binds(error.cor,1,"runif(1,-.05,.05)")




cleanEx()
nameEx("centralMoment")
### * centralMoment

flush(stderr()); flush(stdout())

### Name: centralMoment
### Title: Calculate central moments of a variable
### Aliases: centralMoment

### ** Examples

# This function is not public.

# centralMoment(1:5, 2)



cleanEx()
nameEx("clean")
### * clean

flush(stderr()); flush(stdout())

### Name: clean
### Title: Extract only converged replications in the result objects
### Aliases: clean

### ** Examples

# No example



cleanEx()
nameEx("cleanSimResult")
### * cleanSimResult

flush(stderr()); flush(stdout())

### Name: cleanSimResult
### Title: Extract only converged replications in the result object
### Aliases: cleanSimResult

### ** Examples

# No example



cleanEx()
nameEx("continuousPower")
### * continuousPower

flush(stderr()); flush(stdout())

### Name: continuousPower
### Title: Find power of model parameters when simulations have randomly
###   varying parameters
### Aliases: continuousPower

### ** Examples

## Not run: 
##D # Specify Sample Size by n
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LX <- bind(loading, 0.7)
##D RPH <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")
##D dat <- generate(CFA.Model, 50)
##D out <- analyze(CFA.Model, dat)
##D 
##D # We will use only 5 replications to save time.
##D # In reality, more replications are needed.
##D 
##D # Specify both sample size and percent missing completely at random
##D 
##D Output <- sim(NULL, CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
##D summary(Output)
##D 
##D Cpow <- continuousPower(Output, contN = TRUE, contMCAR = TRUE)
##D Cpow
##D 
##D Cpow2 <- continuousPower(Output, contN = TRUE, contMCAR = TRUE, pred=list(N = 200, pmMCAR = 0.3))
##D Cpow2
## End(Not run)



cleanEx()
nameEx("createData")
### * createData

flush(stderr()); flush(stdout())

### Name: createData
### Title: TBA
### Aliases: createData

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

param <- draw(CFA.Model)

dat <- createData(param[[1]], n = 200) 
# Get the first-group parameter from the param object



cleanEx()
nameEx("draw")
### * draw

flush(stderr()); flush(stdout())

### Name: draw
### Title: TBA
### Aliases: draw

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

param <- draw(CFA.Model)



cleanEx()
nameEx("extractLavaanFit")
### * extractLavaanFit

flush(stderr()); flush(stdout())

### Name: extractLavaanFit
### Title: Extract fit indices from the lavaan object
### Aliases: extractLavaanFit

### ** Examples

# No example



cleanEx()
nameEx("find2Dhist")
### * find2Dhist

flush(stderr()); flush(stdout())

### Name: find2Dhist
### Title: Fit the 2D Kernel Density Estimate
### Aliases: find2Dhist

### ** Examples

# No example



cleanEx()
nameEx("findFactorIntercept")
### * findFactorIntercept

flush(stderr()); flush(stdout())

### Name: findFactorIntercept
### Title: Find factor intercept from regression coefficient matrix and
###   factor total means
### Aliases: findFactorIntercept

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
factorMean <- c(5, 2, 3, 0, 0, 0, 0, 0, 0)
findFactorIntercept(path, factorMean)



cleanEx()
nameEx("findFactorMean")
### * findFactorMean

flush(stderr()); flush(stdout())

### Name: findFactorMean
### Title: Find factor total means from regression coefficient matrix and
###   factor intercept
### Aliases: findFactorMean

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
intcept <- c(5, 2, 3, 0, 0, 0, 0, 0, 0)
findFactorMean(path, intcept)



cleanEx()
nameEx("findFactorResidualVar")
### * findFactorResidualVar

flush(stderr()); flush(stdout())

### Name: findFactorResidualVar
### Title: Find factor residual variances from regression coefficient
###   matrix, factor (residual) correlations, and total factor variances
### Aliases: findFactorResidualVar

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- 0.4
facCor[1, 3] <- facCor[3, 1] <- 0.4
facCor[2, 3] <- facCor[3, 2] <- 0.4
totalVar <- rep(1, 9)
findFactorResidualVar(path, facCor, totalVar)



cleanEx()
nameEx("findFactorTotalCov")
### * findFactorTotalCov

flush(stderr()); flush(stdout())

### Name: findFactorTotalCov
### Title: Find factor total covariance from regression coefficient matrix,
###   factor residual covariance
### Aliases: findFactorTotalCov

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- 0.4
facCor[1, 3] <- facCor[3, 1] <- 0.4
facCor[2, 3] <- facCor[3, 2] <- 0.4
residualVar <- c(1, 1, 1, 0.64, 0.288, 0.288, 0.64, 0.29568, 0.21888)
findFactorTotalCov(path, corPsi=facCor, errorVarPsi=residualVar)



cleanEx()
nameEx("findFactorTotalVar")
### * findFactorTotalVar

flush(stderr()); flush(stdout())

### Name: findFactorTotalVar
### Title: Find factor total variances from regression coefficient matrix,
###   factor (residual) correlations, and factor residual variances
### Aliases: findFactorTotalVar

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- 0.4
facCor[1, 3] <- facCor[3, 1] <- 0.4
facCor[2, 3] <- facCor[3, 2] <- 0.4
residualVar <- c(1, 1, 1, 0.64, 0.288, 0.288, 0.64, 0.29568, 0.21888)
findFactorTotalVar(path, facCor, residualVar)



cleanEx()
nameEx("findIndIntercept")
### * findIndIntercept

flush(stderr()); flush(stdout())

### Name: findIndIntercept
### Title: Find indicator intercepts from factor loading matrix, total
###   factor mean, and indicator mean.
### Aliases: findIndIntercept

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facMean <- c(0.5, 0.2)
indMean <- rep(1, 6)
findIndIntercept(loading, facMean, indMean)



cleanEx()
nameEx("findIndMean")
### * findIndMean

flush(stderr()); flush(stdout())

### Name: findIndMean
### Title: Find indicator total means from factor loading matrix, total
###   factor mean, and indicator intercept.
### Aliases: findIndMean

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facMean <- c(0.5, 0.2)
intcept <- rep(0, 6)
findIndMean(loading, facMean, intcept)



cleanEx()
nameEx("findIndResidualVar")
### * findIndResidualVar

flush(stderr()); flush(stdout())

### Name: findIndResidualVar
### Title: Find indicator residual variances from factor loading matrix,
###   total factor covariance, and total indicator variances.
### Aliases: findIndResidualVar

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facCov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
totalVar <- rep(1, 6)
findIndResidualVar(loading, facCov, totalVar)



cleanEx()
nameEx("findIndTotalVar")
### * findIndTotalVar

flush(stderr()); flush(stdout())

### Name: findIndTotalVar
### Title: Find indicator total variances from factor loading matrix, total
###   factor covariance, and indicator residual variances.
### Aliases: findIndTotalVar

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facCov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
resVar <- c(0.64, 0.51, 0.36, 0.64, 0.51, 0.36)
findIndTotalVar(loading, facCov, resVar)



cleanEx()
nameEx("findPossibleFactorCor")
### * findPossibleFactorCor

flush(stderr()); flush(stdout())

### Name: findPossibleFactorCor
### Title: Find the appropriate position for freely estimated correlation
###   (or covariance) given a regression coefficient matrix
### Aliases: findPossibleFactorCor

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- NA
path[5, 2] <- path[8, 5] <- NA
path[6, 3] <- path[9, 6] <- NA
path[5, 1] <- path[8, 4] <- NA
path[6, 2] <- path[9, 5] <- NA
findPossibleFactorCor(path)



cleanEx()
nameEx("findPower")
### * findPower

flush(stderr()); flush(stdout())

### Name: findPower
### Title: Find a value of independent variables that provides a given
###   value of power.
### Aliases: findPower

### ** Examples

## Not run: 
##D # Specify Sample Size by n
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LX <- bind(loading, 0.4)
##D RPH <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D # Specify both sample size and percent missing completely at random
##D Output <- sim(NULL, model=CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
##D pow <- getPower(Output)
##D findPower(pow, "N", 0.80)
## End(Not run)



cleanEx()
nameEx("findRecursiveSet")
### * findRecursiveSet

flush(stderr()); flush(stdout())

### Name: findRecursiveSet
### Title: Group variables regarding the position in mediation chain
### Aliases: findRecursiveSet

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- NA
path[5, 2] <- path[8, 5] <- NA
path[6, 3] <- path[9, 6] <- NA
path[5, 1] <- path[8, 4] <- NA
path[6, 2] <- path[9, 5] <- NA
findRecursiveSet(path)



cleanEx()
nameEx("findRowZero")
### * findRowZero

flush(stderr()); flush(stdout())

### Name: findRowZero
### Title: Find rows in a matrix that all elements are zero in non-fixed
###   subset rows and columns.
### Aliases: findRowZero

### ** Examples

# No example



cleanEx()
nameEx("findTargetPower")
### * findTargetPower

flush(stderr()); flush(stdout())

### Name: findTargetPower
### Title: Find a value of varying parameters that provides a given value
###   of power.
### Aliases: findTargetPower

### ** Examples

# No example



cleanEx()
nameEx("findphist")
### * findphist

flush(stderr()); flush(stdout())

### Name: findphist
### Title: Find the density (likelihood) of a pair value in 2D Kernel
###   Density Estimate
### Aliases: findphist

### ** Examples

# No example



cleanEx()
nameEx("fitMeasuresChi")
### * fitMeasuresChi

flush(stderr()); flush(stdout())

### Name: fitMeasuresChi
### Title: Find fit indices from the discrepancy values of the target model
###   and null models.
### Aliases: fitMeasuresChi

### ** Examples

# No example



cleanEx()
nameEx("generate")
### * generate

flush(stderr()); flush(stdout())

### Name: generate
### Title: TBA
### Aliases: generate

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

dat <- generate(CFA.Model,200)



cleanEx()
nameEx("getCondQtile")
### * getCondQtile

flush(stderr()); flush(stdout())

### Name: getCondQtile
### Title: Get a quantile of a variable given values of predictors
### Aliases: getCondQtile

### ** Examples

# No example



cleanEx()
nameEx("getCutoff")
### * getCutoff

flush(stderr()); flush(stdout())

### Name: getCutoff
### Title: Find fit indices cutoff given a priori alpha level
### Aliases: getCutoff getCutoff-methods getCutoff,data.frame-method
###   getCutoff,matrix-method getCutoff,SimResult-method

### ** Examples

## Not run: 
##D loading <- matrix(0, 6, 2)
##D loading[1:3, 1] <- NA
##D loading[4:6, 2] <- NA
##D loadingValues <- matrix(0, 6, 2)
##D loadingValues[1:3, 1] <- 0.7
##D loadingValues[4:6, 2] <- 0.7
##D LX <- bind(loading, loadingValues)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPH <- binds(latent.cor, 0.5)
##D error.cor <- matrix(0, 6, 6)
##D diag(error.cor) <- 1
##D RTD <- binds(error.cor)
##D CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output <- sim(5, n = 200, model=CFA.Model)
##D getCutoff(Output, 0.05)
##D 
##D # Finding the cutoff when the sample size is varied.
##D Output2 <- sim(NULL, model=CFA.Model, n=seq(50, 100, 10))
##D getCutoff(Output2, 0.05, nVal = 75)
## End(Not run)



cleanEx()
nameEx("getCutoffNested")
### * getCutoffNested

flush(stderr()); flush(stdout())

### Name: getCutoffNested
### Title: Find fit indices cutoff for nested model comparison given a
###   priori alpha level
### Aliases: getCutoffNested

### ** Examples

## Not run: 
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LX.NULL <- bind(loading.null, 0.7)
##D RPH.NULL <- binds(diag(1))
##D 
##D error.cor.mis <- matrix("rnorm(1, 0, 0.1)", 6, 6)
##D diag(error.cor.mis) <- 1
##D RTD <- binds(diag(6), misspec=error.cor.mis)
##D CFA.Model.NULL <- model(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD, modelType="CFA")
##D 
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LX.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPH.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
##D CFA.Model.ALT <- model(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.NULL)
##D Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL)
##D 
##D getCutoffNested(Output.NULL.NULL, Output.NULL.ALT)
## End(Not run)



cleanEx()
nameEx("getCutoffNonNested")
### * getCutoffNonNested

flush(stderr()); flush(stdout())

### Name: getCutoffNonNested
### Title: Find fit indices cutoff for non-nested model comparison given a
###   priori alpha level
### Aliases: getCutoffNonNested

### ** Examples

## Not run: 
##D loading.A <- matrix(0, 8, 2)
##D loading.A[1:3, 1] <- NA
##D loading.A[4:8, 2] <- NA
##D LX.A <- bind(loading.A, 0.7)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPH <- binds(latent.cor, "runif(1, 0.7, 0.9)")
##D RTD <- binds(diag(8))
##D CFA.Model.A <- model(LY = LX.A, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D loading.B <- matrix(0, 8, 2)
##D loading.B[1:4, 1] <- NA
##D loading.B[5:8, 2] <- NA
##D LX.B <- bind(loading.B, 0.7)
##D CFA.Model.B <- model(LY = LX.B, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A)
##D Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A)
##D Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B)
##D Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B)
##D 
##D getCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
##D getCutoffNonNested(Output.A.A, Output.A.B)
##D getCutoffNonNested(Output.B.B, Output.B.A)
## End(Not run)



cleanEx()
nameEx("getKeywords")
### * getKeywords

flush(stderr()); flush(stdout())

### Name: getKeywords
### Title: List of all keywords used in the 'simsem' package
### Aliases: getKeywords

### ** Examples

# This function is not a public function.

# getKeywords()



cleanEx()
nameEx("getPopulation")
### * getPopulation

flush(stderr()); flush(stdout())

### Name: getPopulation
### Title: Extract the data generation population model underlying an
###   object
### Aliases: getPopulation getPopulation-methods getPopulation,ANY-method

### ** Examples

# See each class for an example.



cleanEx()
nameEx("getPower")
### * getPower

flush(stderr()); flush(stdout())

### Name: getPower
### Title: Find power of model parameters
### Aliases: getPower

### ** Examples

## Not run: 
##D # Specify Sample Size by n
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LX <- bind(loading, 0.7)
##D RPH <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D # We will use only 5 replications to save time.
##D # In reality, more replications are needed.
##D 
##D # Specify both sample size and percent missing completely at random
##D Output <- sim(NULL, model=CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
##D summary(Output)
##D 
##D getPower(Output)
##D 
##D getPower(Output, nVal=c(100, 200), pmMCARval=c(0, 0.1, 0.2))
## End(Not run)



cleanEx()
nameEx("getPowerFit")
### * getPowerFit

flush(stderr()); flush(stdout())

### Name: getPowerFit
### Title: Find power in rejecting alternative models based on fit indices
###   criteria
### Aliases: getPowerFit getPowerFit-methods
###   getPowerFit,data.frame,vector-method getPowerFit,matrix,vector-method
###   getPowerFit,SimResult,vector-method
###   getPowerFit,SimResult,missing-method

### ** Examples

## Not run: 
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LX.NULL <- bind(loading.null, 0.7)
##D RPH.NULL <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD, modelType="CFA")
##D 
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output.NULL <- sim(5, n=500, model=CFA.Model.NULL)
##D Cut.NULL <- getCutoff(Output.NULL, 0.95)
##D 
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LX.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPH.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
##D CFA.Model.ALT <- model(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD, modelType="CFA")
##D 
##D Output.ALT <- sim(5, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT)
##D getPowerFit(Output.ALT, cutoff=Cut.NULL)
##D Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
##D getPowerFit(Output.ALT, cutoff=Rule.of.thumb, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
##D 
##D Output.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.NULL)
##D Output.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.ALT)
##D getPowerFit(Output.ALT2, nullObject=Output.NULL2, nVal=250)
## End(Not run)



cleanEx()
nameEx("getPowerFitNested")
### * getPowerFitNested

flush(stderr()); flush(stdout())

### Name: getPowerFitNested
### Title: Find power in rejecting nested models based on the differences
###   in fit indices
### Aliases: getPowerFitNested getPowerFitNested-methods
###   getPowerFitNested,SimResult,SimResult,vector-method
###   getPowerFitNested,SimResult,SimResult,missing-method

### ** Examples

## Not run: 
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LX.NULL <- bind(loading.null, 0.7)
##D RPH.NULL <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD, modelType="CFA")
##D 
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LX.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPH.ALT <- binds(latent.cor.alt, 0.7)
##D CFA.Model.ALT <- model(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD, modelType="CFA")
##D 
##D Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D Output.ALT.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT) 
##D Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL) 
##D Output.ALT.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.ALT) 
##D 
##D getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
##D getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10))
##D 
##D Output.NULL.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D Output.ALT.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.ALT) 
##D Output.NULL.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.ALT, generate=CFA.Model.NULL) 
##D Output.ALT.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.ALT, generate=CFA.Model.ALT) 
##D 
##D getPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2, nVal = 250)
##D getPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, cutoff=c(Chi=3.84, CFI=-0.10), nVal = 250)
## End(Not run)



cleanEx()
nameEx("getPowerFitNonNested")
### * getPowerFitNonNested

flush(stderr()); flush(stdout())

### Name: getPowerFitNonNested
### Title: Find power in rejecting non-nested models based on the
###   differences in fit indices
### Aliases: getPowerFitNonNested getPowerFitNonNested-methods
###   getPowerFitNonNested,SimResult,SimResult,vector-method
###   getPowerFitNonNested,SimResult,SimResult,missing-method

### ** Examples

## Not run: 
##D loading.A <- matrix(0, 8, 2)
##D loading.A[1:3, 1] <- NA
##D loading.A[4:8, 2] <- NA
##D LX.A <- bind(loading.A, 0.7)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPH <- binds(latent.cor, "runif(1, 0.7, 0.9)")
##D RTD <- binds(diag(8))
##D CFA.Model.A <- model(LY = LX.A, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D loading.B <- matrix(0, 8, 2)
##D loading.B[1:4, 1] <- NA
##D loading.B[5:8, 2] <- NA
##D LX.B <- bind(loading.B, 0.7)
##D CFA.Model.B <- model(LY = LX.B, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A) 
##D Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A) 
##D Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B) 
##D Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B) 
##D 
##D getPowerFitNonNested(Output.B.A, Output.B.B, dat1Mod1=Output.A.A, dat1Mod2=Output.A.B)
##D getPowerFitNonNested(Output.B.A, Output.B.B, cutoff=c(AIC=0, BIC=0))
## End(Not run)



cleanEx()
nameEx("imposeMissing")
### * imposeMissing

flush(stderr()); flush(stdout())

### Name: imposeMissing
### Title: Impose MAR, MCAR, planned missingness, or attrition on a data
###   set
### Aliases: imposeMissing

### ** Examples

  data <- matrix(rep(rnorm(10,1,1),19),ncol=19)
  datac <- cbind(data,rnorm(10,0,1),rnorm(10,5,5))
 
  # Imposing Missing with the following arguments produces no missing values
  imposeMissing(data)
  imposeMissing(data,cov=c(1,2))
  imposeMissing(data,pmMCAR=0)
  imposeMissing(data,pmMAR=0)
  imposeMissing(data,nforms=0)

  #Some more usage examples
  imposeMissing(data,cov=c(1,2),pmMCAR=.1)
  
 
  imposeMissing(data,nforms=3)
  imposeMissing(data,nforms=3,itemGroups=list(c(1,2,3,4,5),c(6,7,8,9,10),c(11,12,13,14,15),c(16,17,18,19)))
  imposeMissing(datac,cov=c(20,21),nforms=3)
  imposeMissing(data,twoMethod=c(19,.8))
  imposeMissing(datac,cov=21,prAttr=.1,timePoints=5)




cleanEx()
nameEx("interpolate")
### * interpolate

flush(stderr()); flush(stdout())

### Name: interpolate
### Title: Find the value of one vector relative to a value of another
###   vector by interpolation
### Aliases: interpolate

### ** Examples

# No Example



cleanEx()
nameEx("kStat")
### * kStat

flush(stderr()); flush(stdout())

### Name: kStat
### Title: Calculate the _k_-statistic of a variable
### Aliases: kStat

### ** Examples

# This function is not a public function.

# kStat(1:5, 4)



cleanEx()
nameEx("kurtosis")
### * kurtosis

flush(stderr()); flush(stdout())

### Name: kurtosis
### Title: Finding excessive kurtosis
### Aliases: kurtosis kurtosis-methods kurtosis,vector-method

### ** Examples

kurtosis(1:5)



cleanEx()
nameEx("likRatioFit")
### * likRatioFit

flush(stderr()); flush(stdout())

### Name: likRatioFit
### Title: Find the likelihood ratio (or Bayes factor) based on the
###   bivariate distribution of fit indices
### Aliases: likRatioFit

### ** Examples

## Not run: 
##D # It does not work now.
##D 
##D #library(lavaan)
##D #loading <- matrix(0, 11, 3)
##D #loading[1:3, 1] <- NA
##D #loading[4:7, 2] <- NA
##D #loading[8:11, 3] <- NA
##D #path.A <- matrix(0, 3, 3)
##D #path.A[2:3, 1] <- NA
##D #path.A[3, 2] <- NA
##D #param.A <- model(LY=bind(loading), BE=bind(path.A), modelType="SEM")
##D 
##D #model.A <- simModel(param.A, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
##D #out.A <- run(model.A, PoliticalDemocracy)
##D 
##D #path.B <- matrix(0, 3, 3)
##D #path.B[1:2, 3] <- NA
##D #path.B[1, 2] <- NA
##D #param.B <- simParamSEM(LY=loading, BE=path.B)
##D 
##D #model.B <- simModel(param.B, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
##D #out.B <- run(model.B, PoliticalDemocracy)
##D 
##D #u2 <- simUnif(-0.2, 0.2)
##D #loading.mis <- matrix(NA, 11, 3)
##D #loading.mis[is.na(loading)] <- 0
##D #LY.mis <- simMatrix(loading.mis, "u2")
##D #misspec <- simMisspecSEM(LY=LY.mis)
##D 
##D #output.A.A <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec)
##D #output.A.B <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.B)
##D #output.B.A <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.A)
##D #output.B.B <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec)
##D 
##D # The output may contain some warnings here. When the number of replications increases (e.g., 1000), the warnings should disappear.
##D #likRatioFit(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)
## End(Not run)



cleanEx()
nameEx("loadingFromAlpha")
### * loadingFromAlpha

flush(stderr()); flush(stdout())

### Name: loadingFromAlpha
### Title: Find standardized factor loading from coefficient alpha
### Aliases: loadingFromAlpha

### ** Examples

    loadingFromAlpha(0.8, 4)



cleanEx()
nameEx("miPool")
### * miPool

flush(stderr()); flush(stdout())

### Name: miPool
### Title: Function to pool imputed results
### Aliases: miPool

### ** Examples

# No Example



cleanEx()
nameEx("miPoolChi")
### * miPoolChi

flush(stderr()); flush(stdout())

### Name: miPoolChi
### Title: Function to pool chi-square statistics from the result from
###   multiple imputation
### Aliases: miPoolChi

### ** Examples

miPoolChi(c(89.864, 81.116, 71.500, 49.022, 61.986, 64.422, 55.256, 57.890, 79.416, 63.944), 2)



cleanEx()
nameEx("miPoolVector")
### * miPoolVector

flush(stderr()); flush(stdout())

### Name: miPoolVector
### Title: Function to pool imputed results that saved in a matrix format
### Aliases: miPoolVector

### ** Examples

param <- matrix(c(0.7, 0.1, 0.5,
					0.75, 0.12, 0.54,
					0.66, 0.11, 0.56,
					0.74, 0.09, 0.55), nrow=4, byrow=TRUE)
SE <- matrix(c(0.1, 0.01, 0.05,
				0.11, 0.023, 0.055,
				0.10, 0.005, 0.04,
				0.14, 0.012, 0.039), nrow=4, byrow=TRUE)
nimps <- 4
miPoolVector(param, SE, nimps)



cleanEx()
nameEx("miss")
### * miss

flush(stderr()); flush(stdout())

### Name: miss
### Title: TBA
### Aliases: miss

### ** Examples

#Example of imposing 10% MCAR missing in all variables with no imputations (FIML method)
Missing <- miss(pmMCAR=0.1)
summary(Missing)

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- bind(loading, 0.7)
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")

#Create data
dat <- generate(CFA.Model, n = 20)

#Impose missing
#dat <- run(Missing, dat)

#Analyze data
#out <- run(SimModel, dat)
#summary(out)

#Example to create simMissing object for 3 forms design at 3 timepoints with 10 imputations
Missing <- miss(nforms=3, timePoints=3, numImps=10)




cleanEx()
nameEx("model")
### * model

flush(stderr()); flush(stdout())

### Name: model
### Title: Data generation template and analysis template for simulation.
### Aliases: model

### ** Examples


loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")



cleanEx()
nameEx("multipleAllEqual")
### * multipleAllEqual

flush(stderr()); flush(stdout())

### Name: multipleAllEqual
### Title: Test whether all objects are equal
### Aliases: multipleAllEqual

### ** Examples

multipleAllEqual(1:5, 1:5, seq(2, 10, 2)/2)
multipleAllEqual(1:5, 1:6, seq(2, 10, 2)/2)



cleanEx()
nameEx("overlapHist")
### * overlapHist

flush(stderr()); flush(stdout())

### Name: overlapHist
### Title: Plot overlapping histograms
### Aliases: overlapHist

### ** Examples

# This function is not a public function.

# a <- rnorm(10000, 0, 1)
# b <- rnorm(10000, 1, 1.5)
# overlapHist(a, b, main="Example")



cleanEx()
nameEx("pValue")
### * pValue

flush(stderr()); flush(stdout())

### Name: pValue
### Title: Find p-values (1 - percentile)
### Aliases: pValue pValue-methods pValue,ANY-method
###   pValue,numeric,vector-method pValue,numeric,data.frame-method

### ** Examples

## Not run: 
##D ########## Make the pValue comparing between lavaan and SimResult work
##D 
##D 
##D 
##D 
##D # Compare number with a vector
##D pValue(0.5, rnorm(1000, 0, 1))
##D 
##D # Compare numbers with a data frame
##D pValue(c(0.5, 0.2), data.frame(rnorm(1000, 0, 1), runif(1000, 0, 1)))
##D 
##D # Compare an analysis result with a result of simulation study
##D #library(lavaan)
##D #loading <- matrix(0, 9, 3)
##D #loading[1:3, 1] <- NA
##D #loading[4:6, 2] <- NA
##D #loading[7:9, 3] <- NA
##D #model <- simParamCFA(LY=loading)
##D #SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
##D #u2 <- simUnif(-0.2, 0.2)
##D #loading.trivial <- matrix(NA, 9, 3)
##D #loading.trivial[is.na(loading)] <- 0
##D #LY.trivial <- simMatrix(loading.trivial, "u2")
##D #mis <- simMisspecCFA(LY = LY.trivial)
##D #out <- run(SimModel, HolzingerSwineford1939)
##D #Output2 <- runFit(out, HolzingerSwineford1939, 20, mis)
##D #pValue(out, Output2)
## End(Not run)



cleanEx()
nameEx("pValueCondCutoff")
### * pValueCondCutoff

flush(stderr()); flush(stdout())

### Name: pValueCondCutoff
### Title: Find a p value when the target is conditional (valid) on a
###   specific value of a predictor
### Aliases: pValueCondCutoff

### ** Examples

# No example



cleanEx()
nameEx("pValueNested")
### * pValueNested

flush(stderr()); flush(stdout())

### Name: pValueNested
### Title: Find p-values (1 - percentile) for a nested model comparison
### Aliases: pValueNested

### ** Examples

## Not run: 
##D #library(lavaan)
##D 
##D #LY <- matrix(1, 4, 2)
##D #LY[,2] <- 0:3
##D #PS <- matrix(NA, 2, 2)
##D #TY <- rep(0, 4)
##D #AL <- rep(NA, 2)
##D #TE <- diag(NA, 4)
##D #linearModel <- simParamCFA(LY=LY, PS=PS, TY=TY, AL=AL, TE=TE)
##D 
##D #LY2 <- matrix(1, 4, 2)
##D #LY2[,2] <- c(0, NA, NA, 3)
##D #unconstrainModel <- simParamCFA(LY=LY2, PS=PS, TY=TY, AL=AL, TE=TE)
##D 
##D #nested <- simModel(linearModel, indLab=paste("t", 1:4, sep=""))
##D #parent <- simModel(unconstrainModel, indLab=paste("t", 1:4, sep=""))
##D 
##D #outNested <- run(nested, Demo.growth)
##D #outParent <- run(parent, Demo.growth)
##D 
##D #loadingMis <- matrix(0, 4, 2)
##D #loadingMis[2:3, 2] <- NA
##D #LYmis <- simMatrix(loadingMis, "runif(1, -0.1, 0.1)")
##D #linearMis <- simMisspecCFA(LY=LYmis)
##D 
##D #simNestedNested <- runFit(model=nested, data=Demo.growth, nRep=10, misspec=linearMis)
##D #simNestedParent <- runFit(model=nested, data=Demo.growth, nRep=10, misspec=linearMis, analyzeModel=parent)
##D 
##D #pValueNested(outNested, outParent, simNestedNested, simNestedParent)
## End(Not run)



cleanEx()
nameEx("pValueNonNested")
### * pValueNonNested

flush(stderr()); flush(stdout())

### Name: pValueNonNested
### Title: Find p-values (1 - percentile) for a non-nested model comparison
### Aliases: pValueNonNested

### ** Examples

## Not run: 
##D #library(lavaan)
##D #loading <- matrix(0, 11, 3)
##D #loading[1:3, 1] <- NA
##D #loading[4:7, 2] <- NA
##D #loading[8:11, 3] <- NA
##D #path.A <- matrix(0, 3, 3)
##D #path.A[2:3, 1] <- NA
##D #path.A[3, 2] <- NA
##D #param.A <- simParamSEM(LY=loading, BE=path.A)
##D 
##D #model.A <- simModel(param.A, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
##D #out.A <- run(model.A, PoliticalDemocracy)
##D 
##D #path.B <- matrix(0, 3, 3)
##D #path.B[1:2, 3] <- NA
##D #path.B[1, 2] <- NA
##D #param.B <- simParamSEM(LY=loading, BE=path.B)
##D 
##D #model.B <- simModel(param.B, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
##D #out.B <- run(model.B, PoliticalDemocracy)
##D 
##D #u2 <- simUnif(-0.2, 0.2)
##D #loading.mis <- matrix(NA, 11, 3)
##D #loading.mis[is.na(loading)] <- 0
##D #LY.mis <- simMatrix(loading.mis, "u2")
##D #misspec <- simMisspecSEM(LY=LY.mis)
##D 
##D #output.A.A <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec)
##D #output.A.B <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.B)
##D #output.B.A <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.A)
##D #output.B.B <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec)
##D 
##D # The output may contain some warnings here. When the number of replications increases (e.g., 1000), the warnings should disappear.
##D #pValueNonNested(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)
## End(Not run)



cleanEx()
nameEx("pValueVariedCutoff")
### * pValueVariedCutoff

flush(stderr()); flush(stdout())

### Name: pValueVariedCutoff
### Title: Find a p value when the cutoff is specified as a vector given
###   the values of predictors
### Aliases: pValueVariedCutoff

### ** Examples

# No example



cleanEx()
nameEx("plot3DQtile")
### * plot3DQtile

flush(stderr()); flush(stdout())

### Name: plot3DQtile
### Title: Build a persepctive plot or contour plot of a quantile of
###   predicted values
### Aliases: plot3DQtile

### ** Examples

# No example



cleanEx()
nameEx("plotCutoff")
### * plotCutoff

flush(stderr()); flush(stdout())

### Name: plotCutoff
### Title: Plot sampling distributions of fit indices with fit indices
###   cutoffs
### Aliases: plotCutoff plotCutoff-methods plotCutoff,data.frame-method
###   plotCutoff,SimResult-method

### ** Examples

## Not run: 
##D loading <- matrix(0, 6, 2)
##D loading[1:3, 1] <- NA
##D loading[4:6, 2] <- NA
##D loadingValues <- matrix(0, 6, 2)
##D loadingValues[1:3, 1] <- 0.7
##D loadingValues[4:6, 2] <- 0.7
##D LX <- bind(loading, loadingValues)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPH <- binds(latent.cor, 0.5)
##D error.cor <- matrix(0, 6, 6)
##D diag(error.cor) <- 1
##D RTD <- binds(error.cor)
##D CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output <- sim(5, n=200, model=CFA.Model) 
##D plotCutoff(Output, 0.05, usedFit=c("RMSEA", "SRMR", "CFI", "TLI"))
##D 
##D # Varying N
##D Output2 <- sim(NULL, n=seq(450, 500, 10), model=CFA.Model)
##D plotCutoff(Output2, 0.05)
##D 
##D # Varying N and pmMCAR
##D Output3 <- sim(NULL, n=seq(450, 500, 10), pmMCAR=c(0, 0.05, 0.1, 0.15), model=CFA.Model)
##D plotCutoff(Output3, 0.05)
## End(Not run)



cleanEx()
nameEx("plotCutoffNested")
### * plotCutoffNested

flush(stderr()); flush(stdout())

### Name: plotCutoffNested
### Title: Plot sampling distributions of the differences in fit indices
###   between nested models with fit indices cutoffs
### Aliases: plotCutoffNested

### ** Examples

## Not run: 
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LX.NULL <- bind(loading.null, 0.7)
##D RPH.NULL <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD, modelType="CFA")
##D 
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LX.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPH.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
##D CFA.Model.ALT <- model(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL) 
##D Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL)
##D 
##D plotCutoffNested(Output.NULL.NULL, Output.NULL.ALT, alpha=0.05)
## End(Not run)



cleanEx()
nameEx("plotCutoffNonNested")
### * plotCutoffNonNested

flush(stderr()); flush(stdout())

### Name: plotCutoffNonNested
### Title: Plot sampling distributions of the differences in fit indices
###   between non-nested models with fit indices cutoffs
### Aliases: plotCutoffNonNested

### ** Examples

## Not run: 
##D loading.A <- matrix(0, 8, 2)
##D loading.A[1:3, 1] <- NA
##D loading.A[4:8, 2] <- NA
##D LX.A <- bind(loading.A, 0.7)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPH <- binds(latent.cor, "runif(1, 0.7, 0.9)")
##D RTD <- binds(diag(8))
##D CFA.Model.A <- model(LY = LX.A, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D loading.B <- matrix(0, 8, 2)
##D loading.B[1:4, 1] <- NA
##D loading.B[5:8, 2] <- NA
##D LX.B <- bind(loading.B, 0.7)
##D CFA.Model.B <- model(LY = LX.B, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A)
##D Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A)
##D Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B)
##D Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B)
##D 
##D plotCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
##D plotCutoffNonNested(Output.A.A, Output.A.B)
##D plotCutoffNonNested(Output.A.A, Output.A.B, onetailed=TRUE)
## End(Not run)



cleanEx()
nameEx("plotDist")
### * plotDist

flush(stderr()); flush(stdout())

### Name: plotDist
### Title: Plot a distribution of a distribution object or data
###   distribution object
### Aliases: plotDist plotDist-methods

### ** Examples

gamma11 <- simGamma(1, 1)
plotDist(gamma11)

chi <- simChisq(5)
#dataDist <- simDataDist(chi, chi)
#plotDist(dataDist)



cleanEx()
nameEx("plotIndividualScatter")
### * plotIndividualScatter

flush(stderr()); flush(stdout())

### Name: plotIndividualScatter
### Title: Plot an overlaying scatter plot visualizing the power of
###   rejecting misspecified models
### Aliases: plotIndividualScatter

### ** Examples

# No example



cleanEx()
nameEx("plotLogisticFit")
### * plotLogisticFit

flush(stderr()); flush(stdout())

### Name: plotLogisticFit
### Title: Plot multiple logistic curves for predicting whether rejecting a
###   misspecified model
### Aliases: plotLogisticFit

### ** Examples

# No example



cleanEx()
nameEx("plotMisfit")
### * plotMisfit

flush(stderr()); flush(stdout())

### Name: plotMisfit
### Title: Plot the population misfit in parameter result object
### Aliases: plotMisfit

### ** Examples

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1, 0.5, 0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1, -0.1, 0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")

ME <- bind(rep(NA, 4), 0)

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType="Path")

# The number of replications in actual analysis should be much more than 5
ParamObject <- sim(20, n=500, Path.Model, misfitType="rmsea", paramOnly=TRUE)
#plotMisfit(ParamObject)

#plotMisfit(ParamObject, misParam=1:2)



cleanEx()
nameEx("plotOverHist")
### * plotOverHist

flush(stderr()); flush(stdout())

### Name: plotOverHist
### Title: Plot multiple overlapping histograms
### Aliases: plotOverHist

### ** Examples

# No example



cleanEx()
nameEx("plotPower")
### * plotPower

flush(stderr()); flush(stdout())

### Name: plotPower
### Title: Make a power plot of a parameter given varying parameters
### Aliases: plotPower

### ** Examples

## Not run: 
##D # Specify Sample Size by n
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LX <- bind(loading, 0.4)
##D RPH <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D 
##D # Specify both sample size and percent missing completely at random
##D Output <- sim(NULL, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2), model=CFA.Model)
##D plotPower(Output, "1.LY1_1", contMCAR=FALSE)
## End(Not run)



cleanEx()
nameEx("plotPowerFit")
### * plotPowerFit

flush(stderr()); flush(stdout())

### Name: plotPowerFit
### Title: Plot sampling distributions of fit indices that visualize power
###   of rejecting datasets underlying misspecified models
### Aliases: plotPowerFit

### ** Examples

## Not run: 
##D ### Something is wrong here. Why the alternative data has the better fit than the null data.
##D 
##D 
##D 
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LX.NULL <- bind(loading.null, 0.7)
##D RPH.NULL <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD, modelType="CFA")
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output.NULL <- sim(50, n=50, model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D 
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LX.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPH.ALT <- binds(latent.cor.alt, 0.5)
##D CFA.Model.ALT <- model(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD, modelType="CFA")
##D Output.ALT <- sim(50, n=50, model=CFA.Model.NULL, generate=CFA.Model.ALT)
##D 
##D datNull <- generate(CFA.Model.NULL, n=50, params=TRUE)
##D datAlt <- generate(CFA.Model.ALT, n=50, params=TRUE)
##D outNull <- analyze(CFA.Model.NULL, datNull)
##D outAlt <- analyze(CFA.Model.NULL, datAlt)
##D summaryFit(Output.NULL)
##D summaryFit(Output.ALT)
##D  
##D plotPowerFit(Output.ALT, nullObject=Output.NULL, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
##D Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
##D plotPowerFit(Output.ALT, cutoff=Rule.of.thumb, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
##D 
##D Output.NULL2 <- simResult(NULL, SimData.NULL, SimModel, n=seq(50, 250, 25))
##D Output.ALT2 <- simResult(NULL, SimData.ALT, SimModel, n=seq(50, 250, 25))
##D 
##D plotPowerFit(Output.ALT2, nullObject=Output.NULL2, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
##D plotPowerFit(Output.ALT2, cutoff=Rule.of.thumb, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
## End(Not run)



cleanEx()
nameEx("plotPowerFitDf")
### * plotPowerFitDf

flush(stderr()); flush(stdout())

### Name: plotPowerFitDf
### Title: Plot sampling distributions of fit indices that visualize power
###   of rejecting datasets underlying misspecified models
### Aliases: plotPowerFitDf

### ** Examples

# No example



cleanEx()
nameEx("plotPowerFitNested")
### * plotPowerFitNested

flush(stderr()); flush(stdout())

### Name: plotPowerFitNested
### Title: Plot power of rejecting a nested model in a nested model
###   comparison by each fit index
### Aliases: plotPowerFitNested

### ** Examples

## Not run: 
##D ############# Still does not work. Check it back later
##D 
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LX.NULL <- bind(loading.null, 0.7)
##D RPH.NULL <- binds(diag(1))
##D RTD <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD, modelType="CFA")
##D 
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LX.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPH.ALT <- binds(latent.cor.alt, 0.7)
##D CFA.Model.ALT <- model(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD, modelType="CFA")
##D 
##D Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D Output.ALT.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT) 
##D Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL) 
##D Output.ALT.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.ALT) 
##D 
##D plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
##D plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, usedFit="CFI")
##D 
##D Output.NULL.NULL2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D Output.ALT.NULL2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.NULL, generate=CFA.Model.ALT) 
##D Output.NULL.ALT2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.ALT, generate=CFA.Model.NULL) 
##D Output.ALT.ALT2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.ALT, generate=CFA.Model.ALT) 
##D 
##D plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2)
##D 
##D plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2, logistic=FALSE)
##D 
##D plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, cutoff=c(CFI=-0.1), logistic=FALSE)
## End(Not run)



cleanEx()
nameEx("plotPowerFitNonNested")
### * plotPowerFitNonNested

flush(stderr()); flush(stdout())

### Name: plotPowerFitNonNested
### Title: Plot power of rejecting a non-nested model based on a difference
###   in fit index
### Aliases: plotPowerFitNonNested

### ** Examples

## Not run: 
##D # Still does not work. Check it later.
##D 
##D loading.A <- matrix(0, 8, 2)
##D loading.A[1:3, 1] <- NA
##D loading.A[4:8, 2] <- NA
##D LX.A <- bind(loading.A, 0.7)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPH <- binds(latent.cor, "runif(1, 0.7, 0.9)")
##D RTD <- binds(diag(8))
##D CFA.Model.A <- model(LY = LX.A, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D loading.B <- matrix(0, 8, 2)
##D loading.B[1:4, 1] <- NA
##D loading.B[5:8, 2] <- NA
##D LX.B <- bind(loading.B, 0.7)
##D CFA.Model.B <- model(LY = LX.B, RPS = RPH, RTE = RTD, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A)
##D Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A)
##D Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B)
##D Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B)
##D 
##D plotPowerFitNonNested(Output.B.A, Output.B.B, dat1Mod1=Output.A.A, dat1Mod2=Output.A.B)
##D plotPowerFitNonNested(Output.B.A, Output.B.B, cutoff=c(AIC=0, BIC=0))
## End(Not run)



cleanEx()
nameEx("plotPowerSig")
### * plotPowerSig

flush(stderr()); flush(stdout())

### Name: plotPowerSig
### Title: Plot multiple logistic curves given a significance result matrix
### Aliases: plotPowerSig

### ** Examples

# No example



cleanEx()
nameEx("plotQtile")
### * plotQtile

flush(stderr()); flush(stdout())

### Name: plotQtile
### Title: Build a scatterplot with overlaying line of quantiles of
###   predicted values
### Aliases: plotQtile

### ** Examples

# No example



cleanEx()
nameEx("plotScatter")
### * plotScatter

flush(stderr()); flush(stdout())

### Name: plotScatter
### Title: Plot overlaying scatter plots visualizing the power of rejecting
###   misspecified models
### Aliases: plotScatter

### ** Examples

# No example



cleanEx()
nameEx("popDiscrepancy")
### * popDiscrepancy

flush(stderr()); flush(stdout())

### Name: popDiscrepancy
### Title: Find the discrepancy value between two means and covariance
###   matrices
### Aliases: popDiscrepancy

### ** Examples

m1 <- rep(0, 3)
m2 <- c(0.1, -0.1, 0.05)
S1 <- matrix(c(1, 0.6, 0.5, 0.6, 1, 0.4, 0.5, 0.4, 1), 3, 3)
S2 <- matrix(c(1, 0.55, 0.55, 0.55, 1, 0.55, 0.55, 0.55, 1), 3, 3)
popDiscrepancy(m1, S1, m2, S2)



cleanEx()
nameEx("popMisfitMACS")
### * popMisfitMACS

flush(stderr()); flush(stdout())

### Name: popMisfitMACS
### Title: Find population misfit by sufficient statistics
### Aliases: popMisfitMACS

### ** Examples

m1 <- rep(0, 3)
m2 <- c(0.1, -0.1, 0.05)
S1 <- matrix(c(1, 0.6, 0.5, 0.6, 1, 0.4, 0.5, 0.4, 1), 3, 3)
S2 <- matrix(c(1, 0.55, 0.55, 0.55, 1, 0.55, 0.55, 0.55, 1), 3, 3)
popMisfitMACS(m1, S1, m2, S2)



cleanEx()
nameEx("predProb")
### * predProb

flush(stderr()); flush(stdout())

### Name: predProb
### Title: Function to get predicted probabilities from logistic regression
### Aliases: predProb

### ** Examples

# No example



cleanEx()
nameEx("printIfNotNull")
### * printIfNotNull

flush(stderr()); flush(stdout())

### Name: printIfNotNull
### Title: Provide basic summary of each object if that object is not NULL.
### Aliases: printIfNotNull

### ** Examples

# This function is not public

# AL <- simVector(rep(NA, 5), "0")
# printIfNotNull(AL, "Factor mean")



cleanEx()
nameEx("revText")
### * revText

flush(stderr()); flush(stdout())

### Name: revText
### Title: Reverse the proportion value by subtracting it from 1
### Aliases: revText

### ** Examples

# This is a private function.

# revText(.96)
# revText("> .60")



cleanEx()
nameEx("run")
### * run

flush(stderr()); flush(stdout())

### Name: run
### Title: Run a particular object in 'simsem' package.
### Aliases: run run-methods run,ANY-method
### Keywords: run

### ** Examples

n02 <- simNorm(0, 0.2)
run(n02)



cleanEx()
nameEx("runMI")
### * runMI

flush(stderr()); flush(stdout())

### Name: runMI
### Title: Multiply impute and analyze data using lavaan
### Aliases: runMI

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data.mat,data.model,imps) {
  #Impute missing data
  imputed.l<-imputeMissing(data.mat,imps)
  
  #Run models on each imputed data set
  #Does this give results from each dataset in the list?
  
  imputed.results<-result.object(imputed.l[[1]],sim.data.model,10)

  imputed.results <- lapply(imputed.l,result.object,data.model,1)
  comb.results<-MIpool(imputed.results,imps)
  
  return(comb.results)

  }



cleanEx()
nameEx("setPopulation")
### * setPopulation

flush(stderr()); flush(stdout())

### Name: setPopulation
### Title: Set the data generation population model underlying an object
### Aliases: setPopulation setPopulation-methods setPopulation,ANY-method

### ** Examples

# See each class for an example.



cleanEx()
nameEx("sim")
### * sim

flush(stderr()); flush(stdout())

### Name: sim
### Title: TBA
### Aliases: sim

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

Output <- sim(20, CFA.Model,n=200)
summary(Output)



cleanEx()
nameEx("simBeta")
### * simBeta

flush(stderr()); flush(stdout())

### Name: simBeta
### Title: Create random beta distribution object
### Aliases: simBeta

### ** Examples

    b11 <- simBeta(1, 1)
    run(b11)



cleanEx()
nameEx("simBinom")
### * simBinom

flush(stderr()); flush(stdout())

### Name: simBinom
### Title: Create random binomial distribution object
### Aliases: simBinom

### ** Examples

    b55 <- simBinom(5, 0.5)
    run(b55)
	summary(b55)



cleanEx()
nameEx("simCauchy")
### * simCauchy

flush(stderr()); flush(stdout())

### Name: simCauchy
### Title: Create random Cauchy distribution object
### Aliases: simCauchy

### ** Examples

    c02 <- simCauchy(0, 2)
    run(c02)
	summary(c02)



cleanEx()
nameEx("simChisq")
### * simChisq

flush(stderr()); flush(stdout())

### Name: simChisq
### Title: Create random chi-squared distribution object
### Aliases: simChisq

### ** Examples

    chi5 <- simChisq(5)
    run(chi5)
	summary(chi5)



cleanEx()
nameEx("simDataDist")
### * simDataDist

flush(stderr()); flush(stdout())

### Name: simDataDist
### Title: Create a data distribution object.
### Aliases: simDataDist

### ** Examples

# Need an example



cleanEx()
nameEx("simExp")
### * simExp

flush(stderr()); flush(stdout())

### Name: simExp
### Title: Create random exponential distribution object
### Aliases: simExp

### ** Examples

    exp2 <- simExp(2)
    run(exp2)
	summary(exp2)



cleanEx()
nameEx("simF")
### * simF

flush(stderr()); flush(stdout())

### Name: simF
### Title: Create random F distribution object
### Aliases: simF

### ** Examples

    f27 <- simF(2, 7)
    run(f27)
	summary(f27)



cleanEx()
nameEx("simFunction")
### * simFunction

flush(stderr()); flush(stdout())

### Name: simFunction
### Title: Create function object
### Aliases: simFunction

### ** Examples

# The example still does not work
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading.start <- matrix("", 9, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:9, 3] <- "runif(1, 0.6, 0.8)"

loading.trivial <- matrix("runif(1, -0.2, 0.2)", 9, 3)
loading.trivial[is.na(loading)] <- 0

LY <- bind(loading, loading.start, misspec=loading.trivial)

error.cor.trivial <- matrix("rnorm(1, 0, 0.1)", 9, 9)
diag(error.cor.trivial) <- 0

RTE <- binds(diag(9), misspec=error.cor.trivial)

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- binds(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1, 0.6, 0.05)"
path.start[3, 2] <- "runif(1, 0.3, 0.5)"
BE <- bind(path, path.start)

datGen <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

#loading <- matrix(0, 12, 4)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:9, 4] <- NA
#loading[10:12, 3] <- NA

#path <- matrix(0, 4, 4)
#path[4, 1:3] <- NA

#analysis <- simParamSEM(BE=path, LY=loading)

#Model <- simModel(analysis)

# Find the products of indicators
#newFUN <- function(data, var1, var2, namesProd) {
#	prod <- data[,var1] * data[,var2]
#	colnames(prod) <- namesProd
#	return(data.frame(data, prod))
#}

#fun <- simFunction(newFUN, var1=paste("y", 1:3, sep=""), var2=paste("y", 4:6, sep=""), namesProd=paste("y", 10:12, sep=""))

# Real simulation will need more than just 10 replications
#Output <- simResult(10, Data.Mis, Model, objFunction=fun)
#summary(Output)



cleanEx()
nameEx("simGamma")
### * simGamma

flush(stderr()); flush(stdout())

### Name: simGamma
### Title: Create random gamma distribution object
### Aliases: simGamma

### ** Examples

    g11 <- simGamma(1, 1)
    run(g11)
	summary(g11)



cleanEx()
nameEx("simGeom")
### * simGeom

flush(stderr()); flush(stdout())

### Name: simGeom
### Title: Create random geometric distribution object
### Aliases: simGeom

### ** Examples

    geom5 <- simGeom(0.05)
    run(geom5)
	summary(geom5)



cleanEx()
nameEx("simHyper")
### * simHyper

flush(stderr()); flush(stdout())

### Name: simHyper
### Title: Create random hypergeometric distribution object
### Aliases: simHyper

### ** Examples

    hyp <- simHyper(20, 5, 10)
    run(hyp)
	summary(hyp)



cleanEx()
nameEx("simLnorm")
### * simLnorm

flush(stderr()); flush(stdout())

### Name: simLnorm
### Title: Create random log normal distribution object
### Aliases: simLnorm

### ** Examples

    lognorm <- simLnorm(0, exp(1))
    run(lognorm)
	summary(lognorm)



cleanEx()
nameEx("simLogis")
### * simLogis

flush(stderr()); flush(stdout())

### Name: simLogis
### Title: Create random logistic distribution object
### Aliases: simLogis

### ** Examples

    logis <- simLogis(0, 1)
    run(logis)
	summary(logis)



cleanEx()
nameEx("simNbinom")
### * simNbinom

flush(stderr()); flush(stdout())

### Name: simNbinom
### Title: Create random negative binomial distribution object
### Aliases: simNbinom

### ** Examples

    nbinom <- simNbinom(5, 0.25)
    run(nbinom)
	summary(nbinom)



cleanEx()
nameEx("simNorm")
### * simNorm

flush(stderr()); flush(stdout())

### Name: simNorm
### Title: Create random normal distribution object
### Aliases: simNorm

### ** Examples

    n02 <- simNorm(0, 0.2)
    run(n02)
	summary(n02)



cleanEx()
nameEx("simPois")
### * simPois

flush(stderr()); flush(stdout())

### Name: simPois
### Title: Create random Poisson distribution object
### Aliases: simPois

### ** Examples

    pois5 <- simPois(5)
    run(pois5)
	summary(pois5)



cleanEx()
nameEx("simT")
### * simT

flush(stderr()); flush(stdout())

### Name: simT
### Title: Create random t distribution object
### Aliases: simT

### ** Examples

    nct82 <- simT(8, ncp=2)
    run(nct82)
	summary(nct82)



cleanEx()
nameEx("simUnif")
### * simUnif

flush(stderr()); flush(stdout())

### Name: simUnif
### Title: Create random uniform distribution object
### Aliases: simUnif

### ** Examples

u1 <- simUnif(-0.1, 0.1)
run(u1)
summary(u1)



cleanEx()
nameEx("simWeibull")
### * simWeibull

flush(stderr()); flush(stdout())

### Name: simWeibull
### Title: Create random Weibull distribution object
### Aliases: simWeibull

### ** Examples

    exWeibull <- simWeibull(2, 100)
    run(exWeibull)
	summary(exWeibull)



cleanEx()
nameEx("skew")
### * skew

flush(stderr()); flush(stdout())

### Name: skew
### Title: Find skewness
### Aliases: skew skew-methods skew,vector-method

### ** Examples

skew(1:5)



cleanEx()
nameEx("sortList")
### * sortList

flush(stderr()); flush(stdout())

### Name: sortList
### Title: Sort two objects in a list
### Aliases: sortList

### ** Examples

# No example



cleanEx()
nameEx("subtractObject")
### * subtractObject

flush(stderr()); flush(stdout())

### Name: subtractObject
### Title: Make a subtraction of each element in an object
### Aliases: subtractObject subtractObject-methods
###   subtractObject,ANY,ANY-method subtractObject,SimRSet,SimRSet-method

### ** Examples

# This function is not public

#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#startingValues(LX, 10)

#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
#result <- startingValues(CFA.Model, 10)
#summary(result)



cleanEx()
nameEx("summaryFit")
### * summaryFit

flush(stderr()); flush(stdout())

### Name: summaryFit
### Title: Provide summary of model fit across replications
### Aliases: summaryFit summaryFit-methods summaryFit,ANY-method
###   summaryFit,SimResult-method

### ** Examples

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- bind(loading, 0.7)
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)
summaryFit(Output)



cleanEx()
nameEx("summaryMisspec")
### * summaryMisspec

flush(stderr()); flush(stdout())

### Name: summaryMisspec
### Title: Provide summary of model misspecification imposed across
###   replications
### Aliases: summaryMisspec summaryMisspec-methods
###   summaryMisspec,ANY-method

### ** Examples

# Incomplete

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1, 0.5, 0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1, -0.1, 0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")

ME <- bind(rep(NA, 4), 0)

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType="Path")


# The number of replications in actual analysis should be much more than 5
# ParamObject <- simResultParam(5, Path.Model, Path.Mis.Model)

# summaryMisspec(ParamObject)



cleanEx()
nameEx("summaryParam")
### * summaryParam

flush(stderr()); flush(stdout())

### Name: summaryParam
### Title: Provide summary of parameter estimates and standard error across
###   replications
### Aliases: summaryParam summaryParam-methods summaryParam,ANY-method
###   summaryParam,SimResult-method

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- bind(loading, 0.7)
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)
summaryParam(Output)
summaryParam(Output, detail=TRUE)



cleanEx()
nameEx("summaryPopulation")
### * summaryPopulation

flush(stderr()); flush(stdout())

### Name: summaryPopulation
### Title: Summarize the data generation population model underlying an
###   object
### Aliases: summaryPopulation summaryPopulation-methods
###   summaryPopulation,ANY-method

### ** Examples

# See each class for an example.



cleanEx()
nameEx("summaryShort")
### * summaryShort

flush(stderr()); flush(stdout())

### Name: summaryShort
### Title: Provide short summary of an object.
### Aliases: summaryShort summaryShort-methods summaryShort,ANY-method
###   summaryShort,vector-method summaryShort,matrix-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
LX <- bind(loading, "runif(1, 0.8, 0.9)")
summaryShort(LX)



cleanEx()
nameEx("toFunction")
### * toFunction

flush(stderr()); flush(stdout())

### Name: toFunction
### Title: Export the distribution object to a function command in text
###   that can be evaluated directly.
### Aliases: toFunction toFunction-methods toFunction,ANY-method

### ** Examples

u2 <- simUnif(-0.2, 0.2)
toFunction(u2)



cleanEx()
nameEx("twoTailedPValue")
### * twoTailedPValue

flush(stderr()); flush(stdout())

### Name: twoTailedPValue
### Title: Find two-tailed _p_ value from one-tailed _p_ value
### Aliases: twoTailedPValue

### ** Examples

# No example



cleanEx()
nameEx("validateCovariance")
### * validateCovariance

flush(stderr()); flush(stdout())

### Name: validateCovariance
### Title: Validate whether all elements provides a good covariance matrix
### Aliases: validateCovariance

### ** Examples

# No example



cleanEx()
nameEx("validateObject")
### * validateObject

flush(stderr()); flush(stdout())

### Name: validateObject
### Title: Validate whether the drawn parameters are good.
### Aliases: validateObject

### ** Examples

# No example



cleanEx()
nameEx("validatePath")
### * validatePath

flush(stderr()); flush(stdout())

### Name: validatePath
### Title: Validate whether the regression coefficient (or loading) matrix
###   is good
### Aliases: validatePath

### ** Examples

# No example



cleanEx()
nameEx("weightedMean")
### * weightedMean

flush(stderr()); flush(stdout())

### Name: weightedMean
### Title: Calculate the weighted mean of a variable
### Aliases: weightedMean

### ** Examples

# This function is not public

# weightedMean(1:5, c(1,1,1,1,2))



cleanEx()
nameEx("whichMonotonic")
### * whichMonotonic

flush(stderr()); flush(stdout())

### Name: whichMonotonic
### Title: Extract a part of a vector that is monotonically increasing or
###   decreasing
### Aliases: whichMonotonic

### ** Examples

# This is a private function.

# whichMonotonic(c(3, 4, 1, 2, 3, 5, 2, 1))



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
