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
### Title: Class '"SimDataDist"': Data distribution object
### Aliases: SimDataDist-class summary,SimDataDist-method
###   plotDist,SimDataDist-method
### Keywords: classes

### ** Examples

showClass("SimDataDist")

d1 <- list(df=2)
d2 <- list(df=3)
d3 <- list(df=4)
d4 <- list(df=5)
d5 <- list(df=3)
d6 <- list(df=4)
d7 <- list(df=5)
d8 <- list(df=6)

dist <- bindDist(c(rep("t", 4), rep("chisq", 8)), d1, d2, d3, d4, d5, d6, d7, d8, d5, d6, d7, d8)
summary(dist)

dist2 <- bindDist(skewness = seq(-3, 3, length.out=12), kurtosis = seq(2, 5, length.out=12))
summary(dist2)



cleanEx()
nameEx("SimMatrix-class")
### * SimMatrix-class

flush(stderr()); flush(stdout())

### Name: SimMatrix-class
### Title: Matrix object: Random parameters matrix
### Aliases: SimMatrix-class summaryShort,SimMatrix-method
###   summary,SimMatrix-method
### Keywords: classes

### ** Examples

showClass("SimMatrix")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LY <- bind(loading, loadingValues)
summary(LY)
rawDraw(LY)

LY <- bind(loading, "rnorm(1, 0.6, 0.05)")
summary(LY)
rawDraw(LY)

mis <- matrix("runif(1, -0.1, 0.1)", 6, 2)
mis[is.na(loading)] <- 0
LY <- bind(loading, "rnorm(1, 0.6, 0.05)", mis)
summary(LY)
rawDraw(LY)



cleanEx()
nameEx("SimMissing-class")
### * SimMissing-class

flush(stderr()); flush(stdout())

### Name: SimMissing-class
### Title: Class '"SimMissing"'
### Aliases: SimMissing-class summary,SimMissing-method
### Keywords: classes

### ** Examples

misstemplate <- miss(pmMCAR=0.2)
summary(misstemplate)



cleanEx()
nameEx("SimResult-class")
### * SimResult-class

flush(stderr()); flush(stdout())

### Name: SimResult-class
### Title: Class '"SimResult"': Simulation Result Object
### Aliases: SimResult-class summary,SimResult-method
###   summaryShort,SimResult-method
### Keywords: classes

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)

# Summary the simulation result
summary(Output)

# Short summary of the simulation result
summaryShort(Output)

# Find the fit index cutoff
getCutoff(Output, 0.05)

# Summary of parameter estimates
summaryParam(Output)

# Summary of population parameters
summaryPopulation(Output)



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
LY <- bind(loading, loadingValues)
summary(LY)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTE <- binds(error.cor)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
summary(CFA.Model)



cleanEx()
nameEx("SimVector-class")
### * SimVector-class

flush(stderr()); flush(stdout())

### Name: SimVector-class
### Title: Vector object: Random parameters vector
### Aliases: SimVector-class summaryShort,SimVector-method
###   summary,SimVector-method
### Keywords: classes

### ** Examples

showClass("SimVector")

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- bind(factor.mean, factor.mean.starting)
rawDraw(AL)
summary(AL)
summaryShort(AL)



cleanEx()
nameEx("analyze")
### * analyze

flush(stderr()); flush(stdout())

### Name: analyze
### Title: Data analysis using the model specification
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

## Not run: 
##D loading1 <- matrix(0, 6, 1)
##D loading1[1:6, 1] <- NA
##D loading2 <- loading1
##D loading2[6,1] <- 0
##D LY1 <- bind(loading1, 0.7)
##D LY2 <- bind(loading2, 0.7)
##D RPS <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model1 <- model(LY = LY1, RPS = RPS, RTE = RTE, modelType="CFA")
##D CFA.Model2 <- model(LY = LY2, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D # Need to make sure that both simResult calls have the same seed!
##D Output1 <- sim(5, n=500, model=CFA.Model1, generate=CFA.Model1, seed=123567)
##D Output2 <- sim(5, n=500, model=CFA.Model2, generate=CFA.Model1, seed=123567)
##D anova(Output1, Output2)
##D 
##D # The example when the sample size is varying
##D Output1b <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model1, generate=CFA.Model1, seed=123567)
##D Output2b <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model2, generate=CFA.Model1, seed=123567)
##D anova(Output1b, Output2b)
## End(Not run)



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
nameEx("bindDist")
### * bindDist

flush(stderr()); flush(stdout())

### Name: bindDist
### Title: Create a data distribution object.
### Aliases: bindDist

### ** Examples

library(copula)

# Create three-dimensional distribution by gaussian copula with 
# the following marginal distributions
#   1. t-distribution with df = 2
# 	2. chi-square distribution with df = 3
#	3. normal distribution with mean = 0 and sd = 1

# Setting the attribute of each marginal distribution
d1 <- list(df=2)
d2 <- list(df=3)
d3 <- list(mean=0, sd=1)

# Create a data distribution object by setting the names of each distribution
# and their arguments
dist <- bindDist(c("t", "chisq", "norm"), d1, d2, d3)

# Create data by using Gumbel Copula as the multivariate distribution
dist <- bindDist(c("t", "chisq", "norm"), d1, d2, d3, copula = gumbelCopula(2, dim = 3))

# Reverse the direction of chi-square distribution from positively skew to negatively skew
dist <- bindDist(c("t", "chisq", "norm"), d1, d2, d3, copula = gumbelCopula(2, dim = 3),
	reverse = c(FALSE, TRUE, FALSE))
	
# Create data based on Vale and Maurelli's method by specifying skewness and kurtosis
dist <- bindDist(skewness = c(0, -2, 2), kurtosis = c(0, 8, 4))



cleanEx()
nameEx("combineSim")
### * combineSim

flush(stderr()); flush(stdout())

### Name: combineSim
### Title: Combine result objects
### Aliases: combineSim

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
Output1 <- sim(5, CFA.Model, n=200, seed=123321)
Output2 <- sim(4, CFA.Model, n=200, seed=324567)
Output3 <- sim(3, CFA.Model, n=200, seed=789987)
Output <- combineSim(Output1, Output2, Output3)
summary(Output)



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
##D LY <- bind(loading, 0.7)
##D RPS <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D dat <- generate(CFA.Model, 50)
##D out <- analyze(CFA.Model, dat)
##D 
##D # Specify both continuous sample size and percent missing completely at random. Note that more fine-grained 
##D # values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) and pmMCAR=seq(0, 0.2, 0.01)
##D Output <- sim(NULL, CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
##D summary(Output)
##D 
##D # Find the power of all combinations of different sample size and percent MCAR missing
##D Cpow <- continuousPower(Output, contN = TRUE, contMCAR = TRUE)
##D Cpow
##D 
##D # Find the power of parameter estimates when sample size is 200 and percent MCAR missing is 0.3
##D Cpow2 <- continuousPower(Output, contN = TRUE, contMCAR = TRUE, pred=list(N = 200, pmMCAR = 0.3))
##D Cpow2
## End(Not run)



cleanEx()
nameEx("createData")
### * createData

flush(stderr()); flush(stdout())

### Name: createData
### Title: Create data from a set of drawn parameters.
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

# Draw a parameter set for data generation.
param <- draw(CFA.Model)

# Generate data from the first group in the paramList.
dat <- createData(param[[1]], n = 200) 



cleanEx()
nameEx("draw")
### * draw

flush(stderr()); flush(stdout())

### Name: draw
### Title: Draw parameters from a 'SimSem' object.
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

# Draw a parameter set for data generation.
param <- draw(CFA.Model)

# Example of Multiple Group Model with Weak Invariance

loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- c("load1", "load2", "load3")
loading.in[4:6, 2] <- c("load4", "load5", "load6")
mis <- matrix(0,6,2)
mis[loading.in == "0"] <- "runif(1, -0.1, 0.1)"
LY.in <- bind(loading.in, "runif(1, 0.7, 0.8)", mis)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

VPS1 <- bind(rep(1, 2))

VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))

# Inequality constraint
script <- "
sth := load1 + load2 + load3
load4 == (load5 + load6) / 2
load4 > 0
load5 > 0
sth2 := load1 - load2
"

# Model Template
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA", con=script)

# Constraint --> Misspecification --> Fill Parameters
draw(weak, createOrder=c(1, 2, 3))

# Constraint --> Fill Parameters --> Misspecification 
draw(weak, createOrder=c(1, 3, 2))

# Misspecification --> Constraint --> Fill Parameters
draw(weak, createOrder=c(2, 1, 3))

# Misspecification --> Fill Parameters --> Constraint
draw(weak, createOrder=c(2, 3, 1))

# Fill Parameters --> Constraint --> Misspecification
draw(weak, createOrder=c(3, 1, 2))

# Fill Parameters --> Misspecification --> Constraint
draw(weak, createOrder=c(3, 2, 1))



cleanEx()
nameEx("estmodel")
### * estmodel

flush(stderr()); flush(stdout())

### Name: estmodel
### Title: Shortcut for data analysis template for simulation.
### Aliases: estmodel estmodel.cfa estmodel.path estmodel.sem

### ** Examples

loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA

CFA.Model <- estmodel(LY = loading, modelType = "CFA")

path <- matrix(0, 4, 4)
path[3, 1:2] <- NA
path[4, 3] <- NA
Path.Model <- estmodel(BE = path, modelType = "Path")

SEM.Model <- estmodel(BE = path, LY = loading, modelType="SEM")

# Shortcut
CFA.Model <- estmodel.cfa(LY = loading)
Path.Model <- estmodel.path(BE = path)
SEM.Model <- estmodel.sem(BE = path, LY = loading)



cleanEx()
nameEx("exportData")
### * exportData

flush(stderr()); flush(stdout())

### Name: exportData
### Title: Export data sets for analysis with outside SEM program.
### Aliases: exportData

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

## Export 20 replications to an external data file (not run).
#exportData(20, CFA.Model, 200)



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
##D LY <- bind(loading, 0.4)
##D RPS <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # Specify both sample size and percent missing completely at random. Note that more fine-grained 
##D # values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) and pmMCAR=seq(0, 0.2, 0.01)
##D Output <- sim(NULL, model=CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
##D 
##D # Find the power of all possible combination of N and pmMCAR
##D pow <- getPower(Output)
##D 
##D # Find the sample size that provides the power of 0.8
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
nameEx("generate")
### * generate

flush(stderr()); flush(stdout())

### Name: generate
### Title: Generate data using SimSem template
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

dat <- generate(CFA.Model, 200)



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
##D LY <- bind(loading, loadingValues)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPS <- binds(latent.cor, 0.5)
##D error.cor <- matrix(0, 6, 6)
##D diag(error.cor) <- 1
##D RTE <- binds(error.cor)
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output <- sim(5, n = 200, model=CFA.Model)
##D 
##D # Get the cutoff (critical value) when alpha is 0.05
##D getCutoff(Output, 0.05)
##D 
##D # Finding the cutoff when the sample size is varied. Note that more fine-grained 
##D # values of n is needed, e.g., n=seq(50, 500, 1)
##D Output2 <- sim(NULL, model=CFA.Model, n=seq(50, 100, 10))
##D 
##D # Get the fit index cutoff when sample size is 75.
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
##D # Nested Model
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LY.NULL <- bind(loading.null, 0.7)
##D RPS.NULL <- binds(diag(1))
##D 
##D error.cor.mis <- matrix("rnorm(1, 0, 0.1)", 6, 6)
##D diag(error.cor.mis) <- 1
##D RTE <- binds(diag(6), misspec=error.cor.mis)
##D CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")
##D 
##D # Parent Model
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LY.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPS.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
##D CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.NULL)
##D Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL)
##D 
##D # Find the fix index cutoff from the sampling distribution of the difference
##D # in fit index of nested models where the alpha is 0.05.
##D getCutoffNested(Output.NULL.NULL, Output.NULL.ALT, alpha=0.05)
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
##D # Model A: Factor 1 with items 1-3 and Factor 2 with items 4-8
##D loading.A <- matrix(0, 8, 2)
##D loading.A[1:3, 1] <- NA
##D loading.A[4:8, 2] <- NA
##D LY.A <- bind(loading.A, 0.7)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
##D RTE <- binds(diag(8))
##D CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # Model B: Factor 1 with items 1-4 and Factor 2 with items 5-8
##D loading.B <- matrix(0, 8, 2)
##D loading.B[1:4, 1] <- NA
##D loading.B[5:8, 2] <- NA
##D LY.B <- bind(loading.B, 0.7)
##D CFA.Model.B <- model(LY = LY.B, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A)
##D Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A)
##D Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B)
##D Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B)
##D 
##D # Find the cutoffs from the sampling distribution to reject model A (model 1)
##D # and to reject model B (model 2)
##D getCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
##D 
##D # Find the cutoffs from the sampling distribution to reject model A (model 1)
##D getCutoffNonNested(Output.A.A, Output.A.B)
##D 
##D # Find the cutoffs from the sampling distribution to reject model B (model 1)
##D getCutoffNonNested(Output.B.B, Output.B.A)
## End(Not run)



cleanEx()
nameEx("getExtraOutput")
### * getExtraOutput

flush(stderr()); flush(stdout())

### Name: getExtraOutput
### Title: Get extra outputs from the result of simulation
### Aliases: getExtraOutput

### ** Examples

## Not run: 
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LY <- bind(loading, 0.7)
##D RPS <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # Write a function to extract the modification index from lavaan object
##D outfun <- function(out) {
##D 	result <- inspect(out, "mi")
##D }
##D 
##D # We will use only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output <- sim(5, n=200, model=CFA.Model, outfun=outfun)
##D 
##D # Get the modification index of each replication
##D getExtraOutput(Output)
## End(Not run)



cleanEx()
nameEx("getPopulation")
### * getPopulation

flush(stderr()); flush(stdout())

### Name: getPopulation
### Title: Extract the data generation population model underlying a result
###   object
### Aliases: getPopulation

### ** Examples

## Not run: 
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LY <- bind(loading, "runif(1, 0.4, 0.9)")
##D RPS <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # We will use only 10 replications to save time.
##D # In reality, more replications are needed.
##D Output <- sim(10, n=200, model=CFA.Model)
##D 
##D # Get the population parameters
##D getPopulation(Output)
## End(Not run)



cleanEx()
nameEx("getPower")
### * getPower

flush(stderr()); flush(stdout())

### Name: getPower
### Title: Find power of model parameters
### Aliases: getPower

### ** Examples

## Not run: 
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LY <- bind(loading, 0.7)
##D RPS <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # Specify both sample size and percent missing completely at random. Note that more fine-grained 
##D # values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) and pmMCAR=seq(0, 0.2, 0.01)
##D Output <- sim(NULL, model=CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
##D summary(Output)
##D 
##D # Get the power of all possible combinations of n and pmMCAR
##D getPower(Output)
##D 
##D # Get the power of the combinations of n of 100 and 200 and pmMCAR of 0, 0.1, and 0.2
##D getPower(Output, nVal=c(100, 200), pmMCARval=c(0, 0.1, 0.2))
## End(Not run)



cleanEx()
nameEx("getPowerFit")
### * getPowerFit

flush(stderr()); flush(stdout())

### Name: getPowerFit
### Title: Find power in rejecting alternative models based on fit indices
###   criteria
### Aliases: getPowerFit

### ** Examples

## Not run: 
##D # Null model with one factor
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LY.NULL <- bind(loading.null, 0.7)
##D RPS.NULL <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")
##D 
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output.NULL <- sim(5, n=500, model=CFA.Model.NULL)
##D 
##D # Get the fit index cutoff from the null model
##D Cut.NULL <- getCutoff(Output.NULL, 0.05)
##D 
##D # Alternative model with two factor
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LY.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPS.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
##D CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")
##D 
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output.ALT <- sim(5, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT)
##D 
##D # Get the power based on the derived cutoff
##D getPowerFit(Output.ALT, cutoff=Cut.NULL)
##D 
##D # Get the power based on the rule of thumb proposed by Hu & Bentler (1999)
##D Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
##D getPowerFit(Output.ALT, cutoff=Rule.of.thumb, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
##D 
##D # The example of continous varying sample size. Note that more fine-grained 
##D # values of n is needed, e.g., n=seq(50, 500, 1)
##D Output.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.NULL)
##D Output.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.ALT)
##D 
##D # Get the power based on the derived cutoff from the null model at the sample size of 250
##D getPowerFit(Output.ALT2, nullObject=Output.NULL2, nVal=250)
## End(Not run)



cleanEx()
nameEx("getPowerFitNested")
### * getPowerFitNested

flush(stderr()); flush(stdout())

### Name: getPowerFitNested
### Title: Find power in rejecting nested models based on the differences
###   in fit indices
### Aliases: getPowerFitNested

### ** Examples

## Not run: 
##D # Null model (Nested model) with one factor
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LY.NULL <- bind(loading.null, 0.7)
##D RPS.NULL <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")
##D 
##D # Alternative model (Parent model) with two factors
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LY.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPS.ALT <- binds(latent.cor.alt, 0.7)
##D CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")
##D 
##D # We make the examples running only 10 replications to save time.
##D # In reality, more replications are needed.
##D Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D Output.ALT.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT) 
##D Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL) 
##D Output.ALT.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.ALT) 
##D 
##D # Find the power based on the derived cutoff from the models analyzed on the null datasets
##D getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
##D 
##D # Find the power based on the chi-square value at df=1 and the CFI change (intentionally
##D # use a cutoff from Cheung and Rensvold (2002) in an appropriate situation).
##D getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10))
##D 
##D # The example of continous varying sample size. Note that more fine-grained 
##D # values of n is needed, e.g., n=seq(50, 500, 1)
##D Output.NULL.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D Output.ALT.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.ALT) 
##D Output.NULL.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.ALT, generate=CFA.Model.NULL) 
##D Output.ALT.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.ALT, generate=CFA.Model.ALT) 
##D 
##D # Get the power based on the derived cutoff from the null model at the sample size of 250
##D getPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2, nVal = 250)
##D 
##D # Get the power based on the rule of thumb from the null model at the sample size of 250
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
##D # Model A: Factor 1 on Items 1-3 and Factor 2 on Items 4-8
##D loading.A <- matrix(0, 8, 2)
##D loading.A[1:3, 1] <- NA
##D loading.A[4:8, 2] <- NA
##D LY.A <- bind(loading.A, 0.7)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
##D RTE <- binds(diag(8))
##D CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # Model B: Factor 1 on Items 1-4 and Factor 2 on Items 5-8
##D loading.B <- matrix(0, 8, 2)
##D loading.B[1:4, 1] <- NA
##D loading.B[5:8, 2] <- NA
##D LY.B <- bind(loading.B, 0.7)
##D CFA.Model.B <- model(LY = LY.B, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A) 
##D Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A) 
##D Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B) 
##D Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B) 
##D 
##D # Find the power based on the derived cutoff for both models
##D getPowerFitNonNested(Output.B.A, Output.B.B, dat1Mod1=Output.A.A, dat1Mod2=Output.A.B)
##D 
##D # Find the power based on the AIC and BIC of 0 (select model B if Output.B.B has lower AIC or BIC)
##D getPowerFitNonNested(Output.B.A, Output.B.B, cutoff=c(AIC=0, BIC=0))
## End(Not run)



cleanEx()
nameEx("imposeMissing")
### * imposeMissing

flush(stderr()); flush(stdout())

### Name: imposeMissing
### Title: Impose MAR, MCAR, planned missingness, or attrition on a data
###   set
### Aliases: imposeMissing impose

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
  
  # No missing at variables 1 and 2
  imposeMissing(data,cov=c(1,2),pmMCAR=.1)
  
  # 3-Form design
  imposeMissing(data,nforms=3)
  
  # 3-Form design with specified groups of items (XABC)
  imposeMissing(data,nforms=3,itemGroups=list(c(1,2,3,4,5),c(6,7,8,9,10),c(11,12,13,14,15),c(16,17,18,19)))
  
  # 3-Form design when variables 20 and 21 are not missing
  imposeMissing(datac,cov=c(20,21),nforms=3)
  
  # 2 method design where the expensive measure is on Variable 19
  imposeMissing(data,twoMethod=c(19,.8))
  
  # Impose missing data with percent attrition of 0.1 in 5 time points
  imposeMissing(datac,cov=21,prAttr=.1,timePoints=5)

  # Logistic-regression MAR
  colnames(data) <- paste("y", 1:ncol(data), sep="")
  script <- 'y1 ~ 0.05 + 0.1*y2 + 0.3*y3
			y4 ~ -2 + 0.1*y4
			y5 ~ -0.5'
  imposeMissing(data, logit=script)



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
##D # Model A; Factor 1 --> Factor 2; Factor 2 --> Factor 3
##D library(lavaan)
##D loading <- matrix(0, 11, 3)
##D loading[1:3, 1] <- NA
##D loading[4:7, 2] <- NA
##D loading[8:11, 3] <- NA
##D path.A <- matrix(0, 3, 3)
##D path.A[2, 1] <- NA
##D path.A[3, 2] <- NA
##D model.A <- estmodel(LY=loading, BE=path.A, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
##D 
##D out.A <- analyze(model.A, PoliticalDemocracy)
##D 
##D # Model A; Factor 1 --> Factor 3; Factor 3 --> Factor 2
##D path.B <- matrix(0, 3, 3)
##D path.B[3, 1] <- NA
##D path.B[2, 3] <- NA
##D model.B <- estmodel(LY=loading, BE=path.B, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
##D 
##D out.B <- analyze(model.B, PoliticalDemocracy)
##D 
##D loading.mis <- matrix("runif(1, -0.2, 0.2)", 11, 3)
##D loading.mis[is.na(loading)] <- 0
##D 
##D # Create SimSem object for data generation and data analysis template
##D datamodel.A <- model.lavaan(out.A, std=TRUE, LY=loading.mis)
##D datamodel.B <- model.lavaan(out.B, std=TRUE, LY=loading.mis)
##D 
##D # Get sample size
##D n <- nrow(PoliticalDemocracy)
##D 
##D # The actual number of replications should be greater than 20.
##D output.A.A <- sim(20, n=n, model.A, generate=datamodel.A) 
##D output.A.B <- sim(20, n=n, model.B, generate=datamodel.A)
##D output.B.A <- sim(20, n=n, model.A, generate=datamodel.B)
##D output.B.B <- sim(20, n=n, model.B, generate=datamodel.B)
##D 
##D # Find the likelihood ratio ;The output may contain some warnings here. 
##D # When the number of replications increases (e.g., 1000), the warnings should disappear.
##D likRatioFit(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)
## End(Not run)



cleanEx()
nameEx("miss")
### * miss

flush(stderr()); flush(stdout())

### Name: miss
### Title: Specifying the missing template to impose on a dataset
### Aliases: miss

### ** Examples

#Example of imposing 10% MCAR missing in all variables with no imputations (FIML method)
Missing <- miss(pmMCAR=0.1, ignoreCols="group")
summary(Missing)

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

#Create data
dat <- generate(CFA.Model, n = 20)

#Impose missing
datmiss <- impose(Missing, dat)

#Analyze data
out <- analyze(CFA.Model, datmiss)
summary(out)

#Missing using logistic regression
script <- 'y1 ~ 0.05 + 0.1*y2 + 0.3*y3
	y4 ~ -2 + 0.1*y4
	y5 ~ -0.5' 
Missing2 <- miss(logit=script, pmMCAR=0.1, ignoreCols="group")
summary(Missing2)
datmiss2 <- impose(Missing2, dat)

#Missing using logistic regression (2)
script <- 'y1 ~ 0.05 + 0.5*y3
	y2 ~ p(0.2)
	y3 ~ p(0.1) + -1*y1
	y4 ~ p(0.3) + 0.2*y1 + -0.3*y2
	y5 ~ -0.5' 
Missing2 <- miss(logit=script)
summary(Missing2)
datmiss2 <- impose(Missing2, dat)

#Example to create simMissing object for 3 forms design at 3 timepoints with 10 imputations
Missing <- miss(nforms=3, timePoints=3, numImps=10)

#Missing template for data analysis with multiple imputation
Missing <- miss(package="mice", m=10, chi="all", convergentCutoff=0.6)



cleanEx()
nameEx("model")
### * model

flush(stderr()); flush(stdout())

### Name: model
### Title: Data generation template and analysis template for simulation.
### Aliases: model model.cfa model.path model.sem

### ** Examples

# Example 1: Confirmatory factor analysis
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

# Example 2: Multiple-group CFA with weak invariance
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- paste0("con", 1:3)
loading[4:6, 2] <- paste0("con", 4:6)
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

CFA.Model <- model(LY = LY, RPS = list(RPS, RPS), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

# Example 3: Linear growth curve model with model misspecification
factor.loading <- matrix(NA, 4, 2)
factor.loading[,1] <- 1
factor.loading[,2] <- 0:3
LY <- bind(factor.loading)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- bind(factor.mean, factor.mean.starting)

factor.var <- rep(NA, 2)
factor.var.starting <- c(1, 0.25)
VPS <- bind(factor.var, factor.var.starting)

factor.cor <- matrix(NA, 2, 2)
diag(factor.cor) <- 1
RPS <- binds(factor.cor, 0.5)

VTE <- bind(rep(NA, 4), 1.2)

RTE <- binds(diag(4))

TY <- bind(rep(0, 4))

LCA.Model <- model(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY, modelType="CFA")

# Example 4: Path analysis model with misspecified direct effect
path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1,0.5,0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1,-0.1,0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1,0.3,0.1)")

ME <- bind(rep(NA, 4), 0)

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType="Path")

# Example 5: Full SEM model 
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- "con1"
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
LY <- bind(loading, loading.start)

RTE <- binds(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- binds(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"
BE <- bind(path, path.start)

SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

# Shortcut example
SEM.model <- model.sem(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

# Example 6: Multiple Group Model
loading1 <- matrix(NA, 6, 1)
LY1 <- bind(loading1, 0.7)
loading2 <- matrix(0, 6, 2)
loading2[1:3, 1] <- NA
loading2[4:6, 2] <- NA
LY2 <- bind(loading2, 0.7)

latent.cor2 <- matrix(NA, 2, 2)
diag(latent.cor2) <- 1
RPS1 <- binds(as.matrix(1))
RPS2 <- binds(latent.cor2, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

noninvariance <- model(LY = list(LY1, LY2), RPS = list(RPS1, RPS2), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

# Example 7: Inequality Constraints

loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- c("load1", "load2", "load3")
loading.in[4:6, 2] <- c("load4", "load5", "load6")
mis <- matrix(0,6,2)
mis[loading.in == "0"] <- "runif(1, -0.1, 0.1)"
LY.in <- bind(loading.in, "runif(1, 0.7, 0.8)", mis)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

VPS1 <- bind(rep(1, 2))

VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))

# Inequality constraint
script <- "
sth := load1 + load2 + load3
load4 == (load5 + load6) / 2
load4 > 0
load5 > 0
sth2 := load1 - load2
"

# Model Template
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA", con=script)



cleanEx()
nameEx("modelLavaan")
### * modelLavaan

flush(stderr()); flush(stdout())

### Name: model.lavaan
### Title: Build the data generation template and analysis template from
###   the lavaan result
### Aliases: model.lavaan

### ** Examples

HS.model <- ' visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)

# Create data generation and data analysis model from lavaan
# Data generation is based on standardized parameters
datamodel1 <- model.lavaan(fit, std=TRUE)

# Data generation is based on unstandardized parameters
datamodel2 <- model.lavaan(fit, std=FALSE)

# Data generation model with misspecification on cross-loadings
crossload <- matrix("runif(1, -0.1, 0.1)", 9, 3)
crossload[1:3, 1] <- 0
crossload[4:6, 2] <- 0
crossload[7:9, 3] <- 0
datamodel3 <- model.lavaan(fit, std=TRUE, LY=crossload)



cleanEx()
nameEx("multipleAllEqual")
### * multipleAllEqual

flush(stderr()); flush(stdout())

### Name: multipleAllEqual
### Title: Test whether all objects are equal
### Aliases: multipleAllEqual

### ** Examples

multipleAllEqual(1:5, 1:5, seq(2, 10, 2)/2) # Should be TRUE
multipleAllEqual(1:5, 1:6, seq(2, 10, 2)/2) # Should be FALSE



cleanEx()
nameEx("pValue")
### * pValue

flush(stderr()); flush(stdout())

### Name: pValue
### Title: Find p-values (1 - percentile) by comparing a single analysis
###   output from the result object
### Aliases: pValue

### ** Examples

## Not run: 
##D # Compare number with a vector
##D pValue(0.5, rnorm(1000, 0, 1))
##D 
##D # Compare numbers with a data frame
##D pValue(c(0.5, 0.2), data.frame(rnorm(1000, 0, 1), runif(1000, 0, 1)))
##D 
##D # Compare an analysis result with a result of simulation study
##D library(lavaan)
##D loading <- matrix(0, 9, 3)
##D loading[1:3, 1] <- NA
##D loading[4:6, 2] <- NA
##D loading[7:9, 3] <- NA
##D targetmodel <- estmodel(LY=loading, modelType="CFA", indLab=paste("x", 1:9, sep=""))
##D out <- analyze(targetmodel, HolzingerSwineford1939)
##D 
##D loading.trivial <- matrix("runif(1, -0.2, 0.2)", 9, 3)
##D loading.trivial[is.na(loading)] <- 0
##D mismodel <- model.lavaan(out, std=TRUE, LY=loading.trivial)
##D 
##D # The actual number of replications should be much greater than 20.
##D simout <- sim(20, n=nrow(HolzingerSwineford1939), mismodel)
##D 
##D # Find the p-value comparing the observed fit indices against the simulated 
##D # sampling distribution of fit indices
##D pValue(out, simout)
## End(Not run)



cleanEx()
nameEx("pValueNested")
### * pValueNested

flush(stderr()); flush(stdout())

### Name: pValueNested
### Title: Find p-values (1 - percentile) for a nested model comparison
### Aliases: pValueNested

### ** Examples

## Not run: 
##D library(lavaan)
##D 
##D # Nested Model: Linear growth curve model
##D LY <- matrix(1, 4, 2)
##D LY[,2] <- 0:3
##D PS <- matrix(NA, 2, 2)
##D TY <- rep(0, 4)
##D AL <- rep(NA, 2)
##D TE <- diag(NA, 4)
##D nested <- estmodel(LY=LY, PS=PS, TY=TY, AL=AL, TE=TE, modelType="CFA", indLab=paste("t", 1:4, sep=""))
##D 
##D # Parent Model: Unconditional growth curve model
##D LY2 <- matrix(1, 4, 2)
##D LY2[,2] <- c(0, NA, NA, 3)
##D parent <- estmodel(LY=LY2, PS=PS, TY=TY, AL=AL, TE=TE, modelType="CFA", indLab=paste("t", 1:4, sep=""))
##D 
##D # Analyze the output
##D outNested <- analyze(nested, Demo.growth)
##D outParent <- analyze(parent, Demo.growth)
##D 
##D # Create data template from the nested model with small misfit on the linear curve
##D loadingMis <- matrix(0, 4, 2)
##D loadingMis[2:3, 2] <- "runif(1, -0.1, 0.1)"
##D datamodel <- model.lavaan(outNested, LY=loadingMis)
##D 
##D # Get the sample size
##D n <- nrow(Demo.growth)
##D 
##D # The actual replications should be much greater than 30.
##D simNestedNested <- sim(30, n=n, nested, generate=datamodel) 
##D simNestedParent <- sim(30, n=n, parent, generate=datamodel)
##D 
##D # Find the p-value comparing the observed fit indices against the simulated 
##D # sampling distribution of fit indices
##D pValueNested(outNested, outParent, simNestedNested, simNestedParent)
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
##D # Model A; Factor 1 --> Factor 2; Factor 2 --> Factor 3
##D library(lavaan)
##D loading <- matrix(0, 11, 3)
##D loading[1:3, 1] <- NA
##D loading[4:7, 2] <- NA
##D loading[8:11, 3] <- NA
##D path.A <- matrix(0, 3, 3)
##D path.A[2, 1] <- NA
##D path.A[3, 2] <- NA
##D model.A <- estmodel(LY=loading, BE=path.A, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
##D 
##D out.A <- analyze(model.A, PoliticalDemocracy)
##D 
##D # Model A; Factor 1 --> Factor 3; Factor 3 --> Factor 2
##D path.B <- matrix(0, 3, 3)
##D path.B[3, 1] <- NA
##D path.B[2, 3] <- NA
##D model.B <- estmodel(LY=loading, BE=path.B, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
##D 
##D out.B <- analyze(model.B, PoliticalDemocracy)
##D 
##D loading.mis <- matrix("runif(1, -0.2, 0.2)", 11, 3)
##D loading.mis[is.na(loading)] <- 0
##D 
##D # Create SimSem object for data generation and data analysis template
##D datamodel.A <- model.lavaan(out.A, std=TRUE, LY=loading.mis)
##D datamodel.B <- model.lavaan(out.B, std=TRUE, LY=loading.mis)
##D 
##D # Get sample size
##D n <- nrow(PoliticalDemocracy)
##D 
##D # The actual number of replications should be greater than 20.
##D output.A.A <- sim(20, n=n, model.A, generate=datamodel.A) 
##D output.A.B <- sim(20, n=n, model.B, generate=datamodel.A)
##D output.B.A <- sim(20, n=n, model.A, generate=datamodel.B)
##D output.B.B <- sim(20, n=n, model.B, generate=datamodel.B)
##D 
##D # Find the p-value comparing the observed fit indices against the simulated 
##D # sampling distribution of fit indices
##D 
##D pValueNonNested(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)
##D 
##D # If the p-value for model A is significant but the p-value for model B is not
##D # significant, model B is preferred.
## End(Not run)



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
##D LY <- bind(loading, loadingValues)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPS <- binds(latent.cor, 0.5)
##D error.cor <- matrix(0, 6, 6)
##D diag(error.cor) <- 1
##D RTE <- binds(error.cor)
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output <- sim(5, n=200, model=CFA.Model) 
##D 
##D # Plot the cutoffs with desired fit indices
##D plotCutoff(Output, 0.05, usedFit=c("RMSEA", "SRMR", "CFI", "TLI"))
##D 
##D # The example of continous varying sample size. Note that more fine-grained 
##D # values of n is needed, e.g., n=seq(50, 500, 1)
##D Output2 <- sim(NULL, n=seq(450, 500, 10), model=CFA.Model)
##D 
##D # Plot the cutoffs along sample size value
##D plotCutoff(Output2, 0.05)
##D 
##D # Specify both continuoussample size and percent missing completely at random. Note that more fine-grained 
##D # values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) and pmMCAR=seq(0, 0.2, 0.01)
##D Output3 <- sim(NULL, n=seq(450, 500, 10), pmMCAR=c(0, 0.05, 0.1, 0.15), model=CFA.Model)
##D 
##D # Plot the contours that each contour represents the value of cutoff at each level
##D # of sample size and percent missing completely at random
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
##D # Nested model: One factor
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LY.NULL <- bind(loading.null, 0.7)
##D RPS.NULL <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")
##D 
##D # Parent model: two factors
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LY.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPS.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
##D CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL) 
##D Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL)
##D 
##D # Plot the cutoffs in nested model comparison
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
##D # Model A: Factor 1 on Items 1-3 and Factor 2 on Items 4-8
##D loading.A <- matrix(0, 8, 2)
##D loading.A[1:3, 1] <- NA
##D loading.A[4:8, 2] <- NA
##D LY.A <- bind(loading.A, 0.7)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
##D RTE <- binds(diag(8))
##D CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # Model B: Factor 1 on Items 1-4 and Factor 2 on Items 5-8
##D loading.B <- matrix(0, 8, 2)
##D loading.B[1:4, 1] <- NA
##D loading.B[5:8, 2] <- NA
##D LY.B <- bind(loading.B, 0.7)
##D CFA.Model.B <- model(LY = LY.B, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A)
##D Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A)
##D Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B)
##D Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B)
##D 
##D # Plot cutoffs for both model A and model B
##D plotCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
##D 
##D # Plot cutoffs for the model A only
##D plotCutoffNonNested(Output.A.A, Output.A.B)
##D 
##D # Plot cutoffs for the model A with one-tailed test
##D plotCutoffNonNested(Output.A.A, Output.A.B, onetailed=TRUE)
## End(Not run)



cleanEx()
nameEx("plotDist")
### * plotDist

flush(stderr()); flush(stdout())

### Name: plotDist
### Title: Plot a distribution of a data distribution object
### Aliases: plotDist

### ** Examples

datadist <- bindDist(c("chisq", "t", "f"), list(df=5), list(df=3), list(df1=3, df2=5))

# Plot the joint distribution of Variables 1 and 2 with correlation of 0.5
plotDist(datadist, r=0.5, var=1:2)

# Plot the marginal distribution of the variable 3
plotDist(datadist, var=3)

datadist2 <- bindDist(skewness = c(0, -2, 2), kurtosis = c(2, 4, 4))

# Plot the joint distribution of Variables 1 and 2 with correlation of 0.5
plotDist(datadist2, r=0.5, var=1:2)

# Plot the marginal distribution of the variable 3
plotDist(datadist2, var=3)



cleanEx()
nameEx("plotLogitMiss")
### * plotLogitMiss

flush(stderr()); flush(stdout())

### Name: plotLogitMiss
### Title: Visualize the missing proportion when the logistic regression
###   method is used.
### Aliases: plotLogitMiss

### ** Examples

script <- 'y1 ~ 0.05 + 0.1*y2 + 0.3*y3
	y4 ~ -2 + 0.1*y4
	y5 ~ -0.5' 
plotLogitMiss(script)

script2 <- 'y1 ~ 0.05 + 0.5*y3
	y2 ~ p(0.2)
	y3 ~ p(0.1) + -1*y1
	y4 ~ p(0.3) + 0.2*y1 + -0.3*y2
	y5 ~ -0.5' 
plotLogitMiss(script2)



cleanEx()
nameEx("plotMisfit")
### * plotMisfit

flush(stderr()); flush(stdout())

### Name: plotMisfit
### Title: Plot the population misfit in the result object
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

# The number of replications in actual analysis should be much more than 20
Output <- sim(20, n=500, Path.Model)

# Plot the distribution of population misfit
plotMisfit(Output)

# Plot the relationship between population RMSEA and all misspecified direct effects
plotMisfit(Output, misParam=1:2)

# Plot the relationship between sample CFI and all misspecified direct effects 
plotMisfit(Output, usedFit="CFI", misParam=1:2)



cleanEx()
nameEx("plotPower")
### * plotPower

flush(stderr()); flush(stdout())

### Name: plotPower
### Title: Make a power plot of a parameter given varying parameters
### Aliases: plotPower

### ** Examples

## Not run: 
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LY <- bind(loading, 0.4)
##D RPS <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # Specify both continuous sample size and percent missing completely at random. Note that more fine-grained 
##D # values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) and pmMCAR=seq(0, 0.2, 0.01)
##D 
##D Output <- sim(NULL, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2), model=CFA.Model)
##D 
##D # Plot the power of the first factor loading along the sample size value
##D plotPower(Output, "1.f1=~y1", contMCAR=FALSE)
##D 
##D # Plot the power of the correlation along the sample size and percent missing completely at random
##D plotPower(Output, "1.f1=~y1")
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
##D # Null model: One factor model
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LY.NULL <- bind(loading.null, 0.7)
##D RPS.NULL <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")
##D 
##D # We make the examples running only 5 replications to save time.
##D # In reality, more replications are needed.
##D Output.NULL <- sim(50, n=50, model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D 
##D # Alternative model: Two-factor model
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LY.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPS.ALT <- binds(latent.cor.alt, 0.5)
##D CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")
##D Output.ALT <- sim(50, n=50, model=CFA.Model.NULL, generate=CFA.Model.ALT)
##D 
##D # Plot the power based on derived cutoff from the null model using four fit indices
##D plotPowerFit(Output.ALT, nullObject=Output.NULL, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
##D 
##D # Plot the power of rejecting null model when the rule of thumb from Hu & Bentler (1999) is used
##D Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
##D plotPowerFit(Output.ALT, cutoff=Rule.of.thumb, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
##D 
##D # The example of continous varying sample size. Note that more fine-grained 
##D # values of n is needed, e.g., n=seq(50, 500, 1)
##D Output.NULL2 <- sim(NULL, n=seq(50, 250, 25), model=CFA.Model.NULL, generate=CFA.Model.NULL)
##D Output.ALT2 <- sim(NULL, n=seq(50, 250, 25), model=CFA.Model.NULL, generate=CFA.Model.ALT)
##D 
##D # Plot the power based on derived cutoff from the null model using four fit indices along sample size
##D plotPowerFit(Output.ALT2, nullObject=Output.NULL2, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
##D 
##D # Plot the power based on rule of thumb along sample size
##D plotPowerFit(Output.ALT2, cutoff=Rule.of.thumb, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
## End(Not run)



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
##D # Null model: One-factor model
##D loading.null <- matrix(0, 6, 1)
##D loading.null[1:6, 1] <- NA
##D LY.NULL <- bind(loading.null, 0.7)
##D RPS.NULL <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")
##D 
##D # Alternative model: Two-factor model
##D loading.alt <- matrix(0, 6, 2)
##D loading.alt[1:3, 1] <- NA
##D loading.alt[4:6, 2] <- NA
##D LY.ALT <- bind(loading.alt, 0.7)
##D latent.cor.alt <- matrix(NA, 2, 2)
##D diag(latent.cor.alt) <- 1
##D RPS.ALT <- binds(latent.cor.alt, 0.7)
##D CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")
##D 
##D # In reality, more than 10 replications are needed
##D Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D Output.ALT.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT) 
##D Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL) 
##D Output.ALT.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.ALT) 
##D 
##D # Plot the power based on the derived cutoff from the models analyzed on the null datasets
##D plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
##D 
##D # Plot the power by only CFI
##D plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, usedFit="CFI")
##D 
##D # The example of continous varying sample size. Note that more fine-grained 
##D # values of n is needed, e.g., n=seq(50, 500, 1)
##D Output.NULL.NULL2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.NULL, generate=CFA.Model.NULL) 
##D Output.ALT.NULL2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.NULL, generate=CFA.Model.ALT) 
##D Output.NULL.ALT2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.ALT, generate=CFA.Model.NULL) 
##D Output.ALT.ALT2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.ALT, generate=CFA.Model.ALT) 
##D 
##D # Plot logistic line for the power based on the derived cutoff from the null model along sample size values
##D plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2)
##D 
##D # Plot scatterplot for the power based on the derived cutoff from the null model along sample size values
##D plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2, logistic=FALSE)
##D 
##D # Plot scatterplot for the power based on the advanced CFI value
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
##D # Model A: Factor 1 on Items 1-3 and Factor 2 on Items 4-8
##D loading.A <- matrix(0, 8, 2)
##D loading.A[1:3, 1] <- NA
##D loading.A[4:8, 2] <- NA
##D LY.A <- bind(loading.A, 0.7)
##D latent.cor <- matrix(NA, 2, 2)
##D diag(latent.cor) <- 1
##D RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
##D RTE <- binds(diag(8))
##D CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # Model B: Factor 1 on Items 1-4 and Factor 2 on Items 5-8
##D loading.B <- matrix(0, 8, 2)
##D loading.B[1:4, 1] <- NA
##D loading.B[5:8, 2] <- NA
##D LY.B <- bind(loading.B, 0.7)
##D CFA.Model.B <- model(LY = LY.B, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # The actual number of replications should be greater than 10.
##D Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A)
##D Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A)
##D Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B)
##D Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B)
##D 
##D # Plot the power based on the derived cutoff for both models
##D plotPowerFitNonNested(Output.B.A, Output.B.B, dat1Mod1=Output.A.A, dat1Mod2=Output.A.B)
##D 
##D # Plot the power based on AIC and BIC cutoffs
##D plotPowerFitNonNested(Output.B.A, Output.B.B, cutoff=c(AIC=0, BIC=0))
## End(Not run)



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
nameEx("rawDraw")
### * rawDraw

flush(stderr()); flush(stdout())

### Name: rawDraw
### Title: Draw values from vector or matrix objects
### Aliases: rawDraw

### ** Examples

loading <- matrix(0, 7, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[1:7, 3] <- NA
loadingVal <- matrix(0, 7, 3)
loadingVal[1:3, 1] <- "runif(1, 0.5, 0.7)"
loadingVal[4:6, 2] <- "runif(1, 0.5, 0.7)"
loadingVal[1:6, 3] <- "runif(1, 0.3, 0.5)"
loadingVal[7, 3] <- 1
loading.mis <- matrix("runif(1, -0.2, 0.2)", 7, 3)
loading.mis[is.na(loading)] <- 0
loading.mis[,3] <- 0
loading.mis[7,] <- 0
loading[1:3, 1] <- "con1"
LY <- bind(loading, loadingVal, misspec=loading.mis)

# Draw values
rawDraw(LY)

# Draw only model parameters containing misspecification
rawDraw(LY, parMisOnly=TRUE)

# Draw only misspecification.
rawDraw(LY, misOnly=TRUE)



cleanEx()
nameEx("setPopulation")
### * setPopulation

flush(stderr()); flush(stdout())

### Name: setPopulation
### Title: Set the data generation population model underlying an object
### Aliases: setPopulation

### ** Examples

# See each class for an example.
## Not run: 
##D # Data generation model
##D loading <- matrix(0, 7, 3)
##D loading[1:3, 1] <- NA
##D loading[4:6, 2] <- NA
##D loading[1:7, 3] <- NA
##D loadingVal <- matrix(0, 7, 3)
##D loadingVal[1:3, 1] <- "runif(1, 0.5, 0.7)"
##D loadingVal[4:6, 2] <- "runif(1, 0.5, 0.7)"
##D loadingVal[1:6, 3] <- "runif(1, 0.3, 0.5)"
##D loadingVal[7, 3] <- 1
##D loading.mis <- matrix("runif(1, -0.2, 0.2)", 7, 3)
##D loading.mis[is.na(loading)] <- 0
##D loading.mis[,3] <- 0
##D loading.mis[7,] <- 0
##D LY <- bind(loading, loadingVal, misspec=loading.mis)
##D 
##D RPS <- binds(diag(3))
##D 
##D path <- matrix(0, 3, 3)
##D path[2, 1] <- NA
##D BE <- bind(path, "runif(1, 0.3, 0.5)")
##D 
##D RTE <- binds(diag(7))
##D 
##D VY <- bind(c(rep(NA, 6), 0), c(rep(1, 6), ""))
##D 
##D datamodel <- model(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY, modelType="SEM")
##D 
##D # Data analysis model
##D loading <- matrix(0, 7, 3)
##D loading[1:3, 1] <- NA
##D loading[4:6, 2] <- NA
##D loading[7, 3] <- NA
##D path <- matrix(0, 3, 3)
##D path[2, 1] <- NA
##D path[1, 3] <- NA
##D path[2, 3] <- NA
##D errorCov <- diag(NA, 7)
##D errorCov[7, 7] <- 0
##D facCov <- diag(3)
##D analysis <- estmodel(LY=loading, BE=path, TE=errorCov, PS=facCov, modelType="SEM", indLab=paste("y", 1:7, sep=""))
##D 
##D # In reality, more than 10 replications are needed.
##D Output <- sim(10, n=200, analysis, generate=datamodel)
##D 
##D # Population 
##D loadingVal <- matrix(0, 7, 3)
##D loadingVal[1:3, 1] <- 0.6
##D loadingVal[4:6, 2] <- 0.6
##D loadingVal[7, 3] <- 1
##D LY <- bind(loading, loadingVal)
##D pathVal <- matrix(0, 3, 3)
##D pathVal[2, 1] <- 0.4
##D pathVal[1, 3] <- 0.4
##D pathVal[2, 3] <- 0.4
##D BE <- bind(path, pathVal)
##D PS <- binds(facCov)
##D errorCovVal <- diag(0.64, 7)
##D errorCovVal[7, 7] <- 0
##D TE <- binds(errorCov, errorCovVal)
##D population <- model(LY=LY, PS=PS, BE=BE, TE=TE, modelType="SEM")
##D 
##D # Set up the new population
##D Output <- setPopulation(Output, population) 
##D 
##D # This summary will contain the bias information
##D summary(Output)
## End(Not run)



cleanEx()
nameEx("sim")
### * sim

flush(stderr()); flush(stdout())

### Name: sim
### Title: Run a monte carlo simulation with a structural equation model.
### Aliases: sim

### ** Examples

# Please go to www.simsem.org for more examples.

# Example of using simsem model template

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

# In reality, more than 5 replications are needed.
Output <- sim(5, CFA.Model, n=200)
summary(Output)

# Example of using simsem model template

popModel <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3
f2 =~ 0.7*y4 + 0.7*y5 + 0.7*y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.49*y1
y2 ~~ 0.49*y2
y3 ~~ 0.49*y3
y4 ~~ 0.49*y4
y5 ~~ 0.49*y5
y6 ~~ 0.49*y6
"

analysisModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
"

Output <- sim(5, model=analysisModel, n=200, generate=popModel, std.lv=TRUE, lavaanfun = "cfa")
summary(Output)

# Example of using population data

pop <- data.frame(y1 = rnorm(100000, 0, 1), y2 = rnorm(100000, 0, 1))

covModel <- "
y1 ~~ y2
"

Output <- sim(5, model=covModel, n=200, rawData=pop, lavaanfun = "cfa")
summary(Output)

# Example of data transformation: Transforming to standard score
fun1 <- function(data) {
	temp <- scale(data)
	as.data.frame(temp)
}

# Example of additional output: Extract modification indices from lavaan
fun2 <- function(out) {
	inspect(out, "mi")
}

# In reality, more than 5 replications are needed.
Output <- sim(5, CFA.Model,n=200,datafun=fun1, outfun=fun2)
summary(Output)

# Get modification indices
getExtraOutput(Output)

# Example of analyze using a function

analyzeFUN <- function(data) {
	out <- lm(y2 ~ y1, data=data)
	coef <- coef(out)
	se <- sqrt(diag(vcov(out)))
	fit <- c(loglik = as.numeric(logLik(out)))
	converged <- TRUE # Assume to be convergent all the time
	return(list(coef = coef, se = se, fit = fit, converged = converged))
}

Output <- sim(5, model=analyzeFUN, n=200, rawData=pop, lavaanfun = "cfa")
summary(Output)



cleanEx()
nameEx("summaryConverge")
### * summaryConverge

flush(stderr()); flush(stdout())

### Name: summaryConverge
### Title: Provide a comparison between the characteristics of convergent
###   replications and nonconvergent replications
### Aliases: summaryConverge

### ** Examples

## Not run: 
##D path.BE <- matrix(0, 4, 4)
##D path.BE[3, 1:2] <- NA
##D path.BE[4, 3] <- NA
##D starting.BE <- matrix("", 4, 4)
##D starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
##D starting.BE[4, 3] <- "runif(1, 0.5, 0.7)"
##D mis.path.BE <- matrix(0, 4, 4)
##D mis.path.BE[4, 1:2] <- "runif(1, -0.1, 0.1)"
##D BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)
##D 
##D residual.error <- diag(4)
##D residual.error[1,2] <- residual.error[2,1] <- NA
##D RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")
##D 
##D loading <- matrix(0, 12, 4)
##D loading[1:3, 1] <- NA
##D loading[4:6, 2] <- NA
##D loading[7:9, 3] <- NA
##D loading[10:12, 4] <- NA
##D mis.loading <- matrix("runif(1, -0.3, 0.3)", 12, 4)
##D mis.loading[is.na(loading)] <- 0
##D LY <- bind(loading, "runif(1, 0.7, 0.9)", misspec=mis.loading)
##D 
##D mis.error.cor <- matrix("rnorm(1, 0, 0.1)", 12, 12)
##D diag(mis.error.cor) <- 0
##D RTE <- binds(diag(12), misspec=mis.error.cor)
##D 
##D SEM.Model <- model(RPS = RPS, BE = BE, LY=LY, RTE=RTE, modelType="SEM")
##D 
##D n1 <- list(mean = 0, sd = 0.1)
##D chi5 <- list(df = 5)
##D 
##D facDist <- bindDist(c("chisq", "chisq", "norm", "norm"), chi5, chi5, n1, n1)
##D 
##D # In reality, more than 50 replications are needed.
##D simOut <- sim(50, n=500, SEM.Model, sequential=TRUE, facDist=facDist, estimator="mlr")
##D 
##D # Summary the convergent and nonconvergent replications
##D summaryConverge(simOut)
## End(Not run)



cleanEx()
nameEx("summaryFit")
### * summaryFit

flush(stderr()); flush(stdout())

### Name: summaryFit
### Title: Provide summary of model fit across replications
### Aliases: summaryFit

### ** Examples

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)

# Summarize the sample fit indices
summaryFit(Output)



cleanEx()
nameEx("summaryMisspec")
### * summaryMisspec

flush(stderr()); flush(stdout())

### Name: summaryMisspec
### Title: Provide summary of the population misfit and
###   misspecified-parameter values across replications
### Aliases: summaryMisspec

### ** Examples

## Not run: 
##D path <- matrix(0, 4, 4)
##D path[3, 1:2] <- NA
##D path[4, 3] <- NA
##D pathVal <- matrix("", 4, 4)
##D pathVal[3, 1:2] <- "runif(1, 0.3, 0.5)"
##D pathVal[4, 3] <- "runif(1, 0.5, 0.7)"
##D pathMis <- matrix(0, 4, 4)
##D pathMis[4, 1:2] <- "runif(1, -0.1, 0.1)"
##D BE <- bind(path, pathVal, pathMis)
##D 
##D residual.error <- diag(4)
##D residual.error[1,2] <- residual.error[2,1] <- NA
##D RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")
##D 
##D Path.Model <- model(RPS = RPS, BE = BE, modelType="Path")
##D 
##D # The number of replications in actual analysis should be much more than 5
##D ParamObject <- sim(5, n=200, Path.Model)
##D 
##D # Summarize the model misspecification that is specified in the 'pathMis' object
##D summaryMisspec(ParamObject)
## End(Not run)



cleanEx()
nameEx("summaryParam")
### * summaryParam

flush(stderr()); flush(stdout())

### Name: summaryParam
### Title: Provide summary of parameter estimates and standard error across
###   replications
### Aliases: summaryParam

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)

# Summary of the parameter estimates
summaryParam(Output)

# Summary of the parameter estimates with additional details
summaryParam(Output, detail=TRUE)



cleanEx()
nameEx("summaryPopulation")
### * summaryPopulation

flush(stderr()); flush(stdout())

### Name: summaryPopulation
### Title: Summarize the population model used for data generation
###   underlying a result object
### Aliases: summaryPopulation

### ** Examples

## Not run: 
##D loading <- matrix(0, 6, 1)
##D loading[1:6, 1] <- NA
##D LY <- bind(loading, "runif(1, 0.4, 0.9)")
##D RPS <- binds(diag(1))
##D RTE <- binds(diag(6))
##D CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
##D 
##D # We will use only 10 replications to save time.
##D # In reality, more replications are needed.
##D Output <- sim(10, n=200, model=CFA.Model)
##D 
##D # Get the summary of population model
##D summaryPopulation(Output)
## End(Not run)



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
LY <- bind(loading, "runif(1, 0.8, 0.9)")
summaryShort(LY)



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
