#Include Gamma Hat and Adjusted Gamma Hat in Fit Indices
#TRUE and FALSE + Number of starting values\
#Call it population value instead of starting value
#Summary function put star for fixed parameters (Any nonzero values that is fixed is labelled as stars)

# Consider run function in the SimMatrix and SimVector
#    PM 9/1: This is implemented as "rawDraw", internal to drawParam.
#    Consider making "draw" a generic method that dispatches on a SimMatrix or SimVector
# Summary simsem got a vector for the symmetric matrix
# Fix misfitType="rmsea", optMisfit="max", optDraws=10,  in example 4
# Fix the starting values for each rep. It would be faster.
# Adjust to be equal with the same matrix
# summaryPopulation that does not converge
# summaryMisspec in the result object --> result object has misspecification slot
# Reparameterize the population value to be suitable with the parameter estimates


#####Result
# Coverage by confidence interval
# Bias from its expected value of random distribution
# Trimed means #

################################## Example 1 ##############################################
##library(simsem)

#install.packages("C:/Users/Sunthud/Desktop/My Dropbox/simsem/simsem_0.0-11.tar.gz", repos=NULL, type="source")
#install.packages("C:/Users/student/Dropbox/simsem/simsem_0.1-1.tar.gz", repos=NULL, type="source")

library(tools)
dirMan <- "C:/Users/student/Dropbox/simsem/simsem/man/runMI.Rd"
showNonASCIIfile(dirMan)

myTry <- function(expr) {
    withRestarts(
        withCallingHandlers(expr,
                            error = function(e) {
                                t <- sys.calls()
                                invokeRestart("myAbort", e, t)
                            }),
        myAbort = function(e, t)
            list(error = e, traceback = t))
}

#test1 <- function(a, b) test2(a, a) + test2(b,b)
#test2 <- function(a, b) (a+b)^2

#myTry(test1(2, "2"))

sourceDir <- function(path, trace = TRUE, ...) {
     for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
		if(nm != "AllClass.R" & nm != "AllGenerics.R") {
        if(trace) cat(nm,":") 
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
		}
     }
}

# dir2 <- "C:/Users/student/Desktop/lavaan/R/"
 # source(paste(dir2, "00Class.R", sep=""))
 # source(paste(dir2, "00Generic.R", sep=""))
 # sourceDir(dir2)

#get
#assign
#dir <- "C:/Users/Sunthud/Desktop/My Dropbox/simsem/simsem/R/"
#dir <- "C:/Users/Sunthud/simsem_backup/simsem/R/"
dir <- "C:/Users/student/Dropbox/simsem/simsem/R/"
 source(paste(dir, "AllClass.R", sep=""))
 source(paste(dir, "AllGenerics.R", sep=""))
 sourceDir(dir)



# library(formatR)
# tidy.dir(dir)

##library(simsem)

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, VTE=VTE, modelType = "CFA")

param <- draw(CFA.Model)
dat <- createData(param[[1]], n = 200)

dat <- generate(CFA.Model,200)
dat2 <- generate(CFA.Model, 200, params=TRUE)
out <- analyze(CFA.Model,dat)

# Try auxiliary
datm <- imposeMissing(dat, cov="group", pmMCAR=0.2)
datx <- data.frame(datm, z=rnorm(nrow(dat), 0, 1))
out <- analyze(CFA.Model,aux="z",datx)


#SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
Output <- sim(20, CFA.Model,n=200)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# Try auxiliary
m <- miss(pmMCAR=0.1, cov="group")
Output <- sim(20, CFA.Model,n=200, miss=m)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# Try extra output

outfun <- function(out) {
	inspect(out, "mi")
}
Output <- sim(20, CFA.Model,n=200, outfun=outfun)
getExtraOutput(Output)

################################# Example 1.1 Multiple Group

# Noninvariance
loading1 <- matrix(NA, 6, 1)
loading2 <- matrix(0, 6, 2)
loading2[1:3, 1] <- NA
loading2[4:6, 2] <- NA
LY1 <- bind(loading1, 0.7)
LY2 <- bind(loading2, 0.7)

latent.cor2 <- matrix(NA, 2, 2)
diag(latent.cor2) <- 1
RPS1 <- binds(as.matrix(1))
RPS2 <- binds(latent.cor2, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

CFA.Model <- model(LY = list(LY1, LY2), RPS = list(RPS1, RPS2), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

# Configural Invariance
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

CFA.Model <- model(LY = list(LY, LY), RPS = list(RPS, RPS), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

# Weak Invariance
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

# Strong Invariance
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- paste0("con", 1:3)
loading[4:6, 2] <- paste0("con", 4:6)
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

TY <- bind(paste0("ty", 1:6), 0)

CFA.Model <- model(LY = LY, RPS = list(RPS, RPS), RTE = list(RTE, RTE), VTE=list(VTE, VTE), TY=TY, ngroups=2, modelType = "CFA")



param <- draw(CFA.Model)
dat <- createData(param[[1]], n = 200)

dat <- generate(CFA.Model,200)
dat2 <- generate(CFA.Model, 200, params=TRUE)
out <- analyze(CFA.Model,dat)



#SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
Output <- sim(20, CFA.Model,n=200)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

################################ Try misspec ############


loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading.mis <- matrix("runif(1, -0.1, 0.1)", 6, 2)
loading.mis[is.na(loading)] <- ""
LY <- bind(loading, 0.7, loading.mis)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, VTE=VTE, modelType = "CFA")

param <- draw(CFA.Model, misfitOut=TRUE)
dat <- createData(param[[1]], n = 200)






#################################### Example 2 #######################

##library(simsem)

loading <- matrix(0, 9, 3)
loading[1:3, 1] <- c(1, NA, NA)
loading[4:6, 2] <- c(1, NA, NA)
loading[7:9, 3] <- c(1, NA, NA)
loadingVal <- matrix(0, 9, 3)
loadingVal[2:3, 1] <- c(0.6, 0.7)
loadingVal[5:6, 2] <- c(1.1, 0.9)
loadingVal[8:9, 3] <- c(1.2, 1.1)
LY <- bind(loading, loadingVal)

facCov <- matrix(NA, 3, 3)
facCovVal <- diag(c(0.8, 0.9, 0.4))
facCovVal[lower.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
facCovVal[upper.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
PS <- binds(facCov, facCovVal)

errorCov <- diag(NA, 9)
errorCovVal <- diag(c(0.5, 1.1, 0.8, 0.4, 0.4, 0.8, 0.8, 0.5, 0.6))
TE <- binds(errorCov, errorCovVal)

AL <- bind(rep(NA, 3), 0)
TY <- bind(c(0, NA, NA, 0, NA, NA, 0, NA, NA), 0)

HS.Model <- model(LY=LY, PS=PS, TE=TE, AL=AL, TY=TY, modelType="CFA")

out <- analyze(HS.Model,generate(HS.Model,200))

Output <- sim(100, HS.Model, n=200)

getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

########################## Example 3 ##########################################

##library(simsem)

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

Data.True <- generate(LCA.Model, 300)
out <- analyze(LCA.Model, Data.True)

#Output.True <- sim(100, n=300, LCA.Model)

loading.trivial <- matrix(0, 4, 2)
loading.trivial[2:3, 2] <- "runif(1,-0.1,0.1)"

LY.mis <- bind(factor.loading, misspec=loading.trivial)

LCA.Mis <- model(LY=LY.mis, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY, modelType="CFA")


Data.Mis <- generate(LCA.Mis, 300, params=TRUE)
out <- analyze(LCA.Mis, Data.Mis)

Output.Mis <- sim(100, n=300, model=LCA.Mis)#, multicore=TRUE)
getCutoff(Output.Mis, 0.05)
plotCutoff(Output.Mis, 0.05)
summaryParam(Output.Mis)

################################# Example 4 ##################################
##library(simsem)

#u35 <- simUnif(0.3, 0.5)
#u57 <- simUnif(0.5, 0.7)
#u1 <- simUnif(-0.1, 0.1)
#n31 <- simNorm(0.3, 0.1)
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

param <- draw(Path.Model,misfitOut=TRUE)
dat <- createData(param[[1]], n = 200)


dat <- generate(Path.Model, n=500, params=TRUE)


out <- analyze(Path.Model, dat)
# The VTE is still wrong. Check after the LCA comes in.


# Output is wrong. It contains some bugs.
Output <- sim(100, n=500, Path.Model)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# It does not stop at the ParamOnly
# paramOut <- sim(100, n=500, Path.Model, paramOnly=TRUE)

############# Example 5 ################################
#library(simsem)

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

dat <- generate(SEM.model, n=300)
out <- analyze(SEM.model, dat)

Output <- sim(200, n=300, SEM.model) # Data.Mis.Con, Model.Con) #, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

################### Example 6 ##################################

#library(simsem)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6), misspec=matrix("rnorm(1,0,0.1)", 6, 6))

CFA.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")

Output.NULL <- sim(20, n=500, CFA.NULL)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
loading.alt.mis <- matrix("runif(1,-.2,.2)", 6, 2)
loading.alt.mis[is.na(loading.alt)] <- 0
LY.ALT <- bind(loading.alt, 0.7, misspec=loading.alt.mis)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, "runif(1,0.7,0.9)")
CFA.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")

Output.ALT <- sim(20, n=500, model=CFA.NULL, generate=CFA.ALT)

cutoff <- getCutoff(Output.NULL, 0.05)
getPowerFit(Output.ALT, cutoff)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "SRMR", "CFI"))

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff2)
plotPowerFit(Output.ALT, cutoff=cutoff2)
plotPowerFit(Output.ALT, cutoff=cutoff2, usedFit=c("RMSEA", "SRMR", "CFI"))

plotPowerFit(Output.ALT, Output.NULL, cutoff=cutoff2, usedFit=c("RMSEA", "SRMR", "CFI"))

################################## Example 7 ##########################################
#library(simsem)

#u2 <- simUnif(-0.2, 0.2)
#u49 <- simUnif(0.4, 0.9)
#u36 <- simUnif(0.3, 0.6)
#n1 <- simNorm(0, 0.1)
#n21 <- simNorm(0.2, 0.1)
#n31 <- simNorm(0.3, 0.1)
#n41 <- simNorm(0.4, 0.1)
loading <- matrix(0, 9, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[c(1, 4, 7), 4] <- NA
loading.v <- matrix(0, 9, 4)
loading.v[1:3, 1] <- "runif(1,.4,.9)"
loading.v[4:6, 2] <- "runif(1,.4,.9)"
loading.v[7:9, 3] <- "runif(1,.4,.9)"
loading.v[c(1, 4, 7), 4] <- "runif(1,.3,.6)"
loading.mis <- matrix("runif(1,-.2,.2)", 9, 4)
loading.mis[is.na(loading)] <- 0
loading.mis[,4] <- 0
LY <- bind(loading, loading.v, misspec=loading.mis)

faccor <- diag(4)
faccor[1, 2] <- faccor[2, 1] <- NA
faccor[1, 3] <- faccor[3, 1] <- NA
faccor[2, 3] <- faccor[3, 2] <- NA
faccor.v <- diag(4)
faccor.v[1, 2] <- faccor.v[2, 1] <- "rnorm(1,.4,.1)"
faccor.v[1, 3] <- faccor.v[3, 1] <- "rnorm(1,.2,.1)"
faccor.v[2, 3] <- faccor.v[3, 2] <- "rnorm(1,.3,.1)"
RPS <- binds(faccor, faccor.v)

error.cor.mis <- matrix("rnorm(1,0,.1)", 9, 9)
diag(error.cor.mis) <- 1
RTE <- binds(diag(9), misspec=error.cor.mis)

mtmm.model <- model(LY=LY, RPS=RPS, RTE=RTE, modelType="CFA")
out <- sim(10, n=500, mtmm.model)


miss.model <- miss(pmMCAR=0.2, ignoreCols="group") #, numImps=5)

Output <- sim(10, n=500, mtmm.model, miss=miss.model)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

################################## Example 8 ##########################################
#library(simsem)

loading <- matrix(0, 48, 4)
loading[1:12, 1] <- NA
loading[13:24, 2] <- NA
loading[25:36, 3] <- NA
loading[37:48, 4] <- NA
loading.mis <- matrix("runif(1, -0.2, 0.2)", 48, 4)
loading.mis[is.na(loading)] <- 0
LY <- bind(loading, "runif(1, 0.4, 0.9)", misspec=loading.mis)

faccor <- matrix(NA, 4, 4)
diag(faccor) <- 1
RPS <- binds(faccor, "runif(1, 0.1, 0.6)")

RTE <- binds(diag(48))

CFA.model <- model(LY=LY, RPS=RPS, RTE=RTE, modelType="CFA")

setx <- c(1:3, 13:15, 25:27, 37:39)
set1 <- setx + 3
set2 <- set1 + 3
set3 <- set2 + 3
itemGroups <- list(setx, set1, set2, set3)

missModel <- miss(nforms=3, itemGroups=itemGroups)#, numImps=5)

dat <- generate(CFA.model, n=1000)
dat <- imposeMissing(data=dat, nforms=3, itemGroups=itemGroups)
# dat <- run(missModel, dat) ################### runMissModel!!!!
out <- analyze(CFA.model, dat)

Output <- sim(20, n=1000, CFA.model, miss=missModel)#, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)


############################# Example 9 #############################

#library(simsem)

loading <- matrix(0, 12, 3)
loading[1:4, 1] <- NA
loading[5:8, 2] <- NA
loading[9:12, 3] <- NA
loading.mis <- matrix("runif(1, -0.2, 0.2)", 12, 3)
loading.mis[is.na(loading)] <- 0
LY <- bind(loading, 0.7, misspec=loading.mis)

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, "runif(1, -0.5, 0.5)")

error.cor <- matrix(0, 12, 12)
diag(error.cor) <- 1
RTE <- binds(error.cor)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA") 

# margins 	

# a character vector specifying all the marginal distributions. See details below.

# paramMargins 	

# a list whose each component is a list of named components, giving the parameter values of the marginal distributions. See details below.

d1 <- list(df=2)
d2 <- list(df=3)
d3 <- list(df=4)
d4 <- list(df=5)
d5 <- list(df=3)
d6 <- list(df=4)
d7 <- list(df=5)
d8 <- list(df=6)


dist <- bindDist(c(rep("t", 4), rep("chisq", 8)), d1, d2, d3, d4, d5, d6, d7, d8, d5, d6, d7, d8)

dat <- generate(CFA.Model, n=200, indDist=dist)
out <- analyze(CFA.Model, dat, estimator="mlr")

Output <- sim(50, n=200, CFA.Model, indDist=dist, estimator="mlm")
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

##################################### Example 10 #######################

#library(simsem)


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

loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA
mis.loading <- matrix("runif(1, -0.3, 0.3)", 12, 4)
mis.loading[is.na(loading)] <- 0
LY <- bind(loading, "runif(1, 0.7, 0.9)", misspec=mis.loading)

mis.error.cor <- matrix("rnorm(1, 0, 0.1)", 12, 12)
diag(mis.error.cor) <- 0
RTE <- binds(diag(12), misspec=mis.error.cor)

SEM.Model <- model(RPS = RPS, BE = BE, LY=LY, RTE=RTE, modelType="SEM")

n1 <- list(mean = 0, sd = 0.1)
chi5 <- list(df = 5)

facDist <- bindDist(c("chisq", "chisq", "norm", "norm"), chi5, chi5, n1, n1)

dat <- generate(SEM.Model, n=500, sequential=TRUE, facDist=facDist)
out <- analyze(SEM.Model, dat, estimator="mlr")

# Need to be fixed!!! Sequential
simOut <- sim(100, n=500, SEM.Model, sequential=TRUE, facDist=facDist, estimator="mlr") #, multicore=TRUE)
getCutoff(simOut, 0.05)
plotCutoff(simOut, 0.05)
summaryParam(simOut)

####################################### Example 11 ############################

#library(simsem)

loading <- matrix(0, 5, 3)
loading[1:3, 1] <- NA
loading[4, 2] <- NA
loading[5, 3] <- NA
loadingVal <- matrix(0, 5, 3)
loadingVal[1:3, 1] <- "runif(1, 0.7, 0.9)"
loadingVal[4, 2] <- 1
loadingVal[5, 3] <- 1
LY <- bind(loading, loadingVal)

facCor <- diag(3)
facCor[2, 1] <- NA
facCor[1, 2] <- NA
RPS <- binds(facCor, "runif(1, -0.5, 0.5)")

path <- matrix(0, 3, 3)
path[3, 1] <- NA
path[3, 2] <- NA
BE <- bind(path, "runif(1, -0.5, 0.5)")

errorCorMis <- diag(5)
errorCorMis[1:3, 1:3] <- "rnorm(1, 0, 0.1)"
errorCorMis <- diag(5)
RTE <- binds(diag(5), misspec=errorCorMis)

VY <- bind(c(NA, NA, NA, 0, 0), 1)

SEM.Model <- model(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY, modelType="SEM")

dist <- c("norm", "chisq", "norm")
n01 <- list(mean=0, sd=1)
c5 <- list(df=5)
facDist <- bindDist(dist, n01, c5, n01)

dat <- generate(SEM.Model, n=200, sequential=TRUE, facDist=facDist, params=TRUE)
out <- analyze(SEM.Model, dat, estimator="mlm")

Output <- sim(100, n=200, SEM.Model, sequential=TRUE, facDist=facDist, estimator="mlm")
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

################################### Example 12 ######################################

#library(simsem)

loading <- matrix(0, 7, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
mis.loading <- matrix(0, 7, 2)
mis.loading[1:3, 2] <- "runif(1, -0.2, 0.2)"
mis.loading[4:6, 1] <- "runif(1, -0.2, 0.2)"

LY <- bind(loading, "runif(1, 0.5, 0.7)", misspec=mis.loading)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, "runif(1, 0.3, 0.5)")

error.cor <- diag(7)
error.cor[1:6, 7] <- NA
error.cor[7, 1:6] <- NA
RTE <- binds(error.cor, "runif(1, -0.4, 0.4)")

VX <- bind(rep(NA, 7), 1)

CFA.Model.Aux <- model(LY = LY, RPS = RPS, RTE = RTE, VY = VX, modelType="CFA") 

dat <- generate(CFA.Model.Aux, n=200)
missmodel <- miss(pmMAR=0.1, cov=7, ignoreCols=8, threshold = 0.5)

dat <- impose(missmodel, dat)


############### Analysis model ###########################


loading2 <- matrix(0, 6, 2)
loading2[1:3, 1] <- NA
loading2[4:6, 2] <- NA


latent.cor2 <- matrix(NA, 2, 2)
diag(latent.cor2) <- 1

error.cor2 <- diag(NA, 6)

CFA.Model <- estmodel(LY = loading2, PS = latent.cor2, TE = error.cor2, modelType="CFA")
out <- analyze(CFA.Model, dat, indLab=paste0("y", 1:6), aux="y7")

Output <- sim(10, n=200, model=CFA.Model, generate=CFA.Model.Aux, miss=missmodel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)




########################################## Example 13 ###################
#library(simsem)
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
cfamodel <- estmodel(LY=loading, modelType="CFA", indLab=paste("x", 1:9, sep=""))
out <- analyze(cfamodel, HolzingerSwineford1939)

### Making result object without trivial model misspecification
#output <- runFit(SimModel, HolzingerSwineford1939, 1000)
#pValue(out, output)

loading.mis <- matrix("runif(1, -0.2, 0.2)", 9, 3)
loading.mis[is.na(loading)] <- 0

datamodel <- model.lavaan(out, std=TRUE, LY=loading.mis)
output <- sim(200, n=nrow(HolzingerSwineford1939), datamodel)

pValue(out, output)

############################### Example 14 #######################

#library(simsem)
#library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path <- matrix(0, 3, 3)
path[2:3, 1] <- NA
path[3, 2] <- NA
param <- estmodel(LY=loading, BE=path, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")), facLab=c("ind60", "dem60", "dem65"))

# Fix indLab facLab and groupLab

usedData <- imposeMissing(PoliticalDemocracy, pmMCAR=0.03)

#model <- simModel(param)
#miss <- simMissing(numImps=5)
out <- analyze(param, usedData)

usedData2 <- imposeMissing(PoliticalDemocracy, logical=is.na(usedData))
misstemplate <- miss(logical=is.na(usedData), ignoreCols="group")

loading.mis <- matrix("runif(1, -0.2, 0.2)", 11, 3)
loading.mis[is.na(loading)] <- 0
datamodel <- model.lavaan(out, std=TRUE, LY=loading.mis)
output <- sim(200, n=nrow(PoliticalDemocracy), datamodel, miss=misstemplate)
pValue(out, output)

############################# Example 15 ############################################

#library(simsem)

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
LY <- bind(loading, loadingVal, misspec=loading.mis)

RPS <- binds(diag(3))

path <- matrix(0, 3, 3)
path[2, 1] <- NA
BE <- bind(path, "runif(1, 0.3, 0.5)")

RTE <- binds(diag(7))

VY <- bind(c(rep(NA, 6), 0), c(rep(1, 6), ""))

datamodel <- model(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY, modelType="SEM")

# First analysis model: Model without covariate
loading2 <- matrix(0, 6, 2)
loading2[1:3, 1] <- NA
loading2[4:6, 2] <- NA
path2 <- matrix(0, 2, 2)
path2[2,1] <- NA
analysis1 <- estmodel(LY=loading2, BE=path2, modelType="SEM", indLab=paste("y", 1:6, sep=""))

Output1 <- sim(100, n=200, analysis1, generate=datamodel)


# param <- getPopulation(Output1)
# param <- extract(param, y=1:6, e=1:2)
# Output1 <- setPopulation(Output1, param) 
# summary(Output1)

# Second analysis model: Model accounting for covariate in the indicator level
Output2 <- sim(100, n=200, datamodel)
summary(Output2)

# Third analysis model: Model accounting for covariate with orthogonalization
library(semTools)

datafun <- function(data) {
	residualCovariate(data, targetVar=1:6, covVar=7)
}

dat <- generate(datamodel, n=200)
dat2 <- datafun(dat)

analysis3 <- analysis1
Output3 <- sim(100, n=200, analysis3, generate=datamodel, datafun=datafun)
summary(Output3)

# Fourth analysis model: Model accounting for covariate in factor level
loading <- matrix(0, 7, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7, 3] <- NA
path <- matrix(0, 3, 3)
path[2, 1] <- NA
path[1, 3] <- NA
path[2, 3] <- NA
errorCov <- diag(NA, 7)
errorCov[7, 7] <- 0
facCov <- diag(3)
analysis4 <- estmodel(LY=loading, BE=path, TE=errorCov, PS=facCov, modelType="SEM", indLab=paste("y", 1:7, sep=""))

Output4 <- sim(100, n=200, analysis4, generate=datamodel)

loadingVal <- matrix(0, 7, 3)
loadingVal[1:3, 1] <- 0.6
loadingVal[4:6, 2] <- 0.6
loadingVal[7, 3] <- 1
LY <- bind(loading, loadingVal)
pathVal <- matrix(0, 3, 3)
pathVal[2, 1] <- 0.4
pathVal[1, 3] <- 0.4
pathVal[2, 3] <- 0.4
BE <- bind(path, pathVal)
PS <- binds(facCov)
errorCovVal <- diag(0.64, 7)
errorCovVal[7, 7] <- 0
TE <- binds(errorCov, errorCovVal)
population <- model(LY=LY, PS=PS, BE=BE, TE=TE, modelType="SEM")
Output4 <- setPopulation(Output4, population) 
summary(Output4)

############################# Example 16 ############################################

#library(simsem)
u35 <- simUnif(0.1, 0.3)
u57 <- simUnif(0.5, 0.7)
u2 <- simUnif(-0.2, 0.2)

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- NA
path[5, 2] <- path[8, 5] <- NA
path[6, 3] <- path[9, 6] <- NA
path[5, 1] <- path[8, 4] <- NA
path[6, 2] <- path[9, 5] <- NA
pathVal <- matrix(0, 9, 9)
pathVal[4, 1] <- pathVal[7, 4] <- "runif(1, 0.5, 0.7)"
pathVal[5, 2] <- pathVal[8, 5] <- "runif(1, 0.5, 0.7)"
pathVal[6, 3] <- pathVal[9, 6] <- "runif(1, 0.5, 0.7)"
pathVal[5, 1] <- pathVal[8, 4] <- "runif(1, 0.3, 0.5)"
pathVal[6, 2] <- pathVal[9, 5] <- "runif(1, 0.3, 0.5)"
BE <- bind(path, pathVal)
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- NA
facCor[1, 3] <- facCor[3, 1] <- NA
facCor[2, 3] <- facCor[3, 2] <- NA
RPS <- binds(facCor, "u35")
loading <- matrix(0, 27, 9)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA
loading[13:15, 5] <- NA
loading[16:18, 6] <- NA
loading[19:21, 7] <- NA
loading[22:24, 8] <- NA
loading[25:27, 9] <- NA
LY <- bind(loading, "u57")
errorCor <- diag(27)
errorCor[1, 10] <- errorCor[10, 19] <- NA
errorCor[2, 11] <- errorCor[11, 20] <- NA
errorCor[3, 12] <- errorCor[12, 21] <- NA
errorCor[4, 13] <- errorCor[13, 22] <- NA
errorCor[5, 14] <- errorCor[14, 23] <- NA
errorCor[6, 15] <- errorCor[15, 24] <- NA
errorCor[7, 16] <- errorCor[16, 25] <- NA
errorCor[8, 17] <- errorCor[17, 26] <- NA
errorCor[9, 18] <- errorCor[18, 27] <- NA
errorCor[1, 19] <- NA
errorCor[2, 20] <- NA
errorCor[3, 21] <- NA
errorCor[4, 22] <- NA
errorCor[5, 23] <- NA
errorCor[6, 24] <- NA
errorCor[7, 25] <- NA
errorCor[8, 26] <- NA
errorCor[9, 27] <- NA
errorCor <- errorCor + t(errorCor)
diag(errorCor) <- 1
errorCorVal <- diag(27)
errorCorVal[1, 10] <- errorCorVal[10, 19] <- 0.2
errorCorVal[2, 11] <- errorCorVal[11, 20] <- 0.2
errorCorVal[3, 12] <- errorCorVal[12, 21] <- 0.2
errorCorVal[4, 13] <- errorCorVal[13, 22] <- 0.2
errorCorVal[5, 14] <- errorCorVal[14, 23] <- 0.2
errorCorVal[6, 15] <- errorCorVal[15, 24] <- 0.2
errorCorVal[7, 16] <- errorCorVal[16, 25] <- 0.2
errorCorVal[8, 17] <- errorCorVal[17, 26] <- 0.2
errorCorVal[9, 18] <- errorCorVal[18, 27] <- 0.2
errorCorVal[1, 19] <- 0.04
errorCorVal[2, 20] <- 0.04
errorCorVal[3, 21] <- 0.04
errorCorVal[4, 22] <- 0.04
errorCorVal[5, 23] <- 0.04
errorCorVal[6, 24] <- 0.04
errorCorVal[7, 25] <- 0.04
errorCorVal[8, 26] <- 0.04
errorCorVal[9, 27] <- 0.04
errorCorVal <- errorCorVal + t(errorCorVal)
diag(errorCorVal) <- 1
TE <- binds(errorCor, errorCorVal)
longMed <- simSetSEM(BE=BE, RPS=RPS, LY=LY, RTE=TE)

c1 <- matrix(NA, 2, 1)
c1[,1] <- c(4, 7)
rownames(c1) <- rep("VPS", 2)

c2 <- matrix(NA, 2, 1)
c2[,1] <- c(5, 8)
rownames(c2) <- rep("VPS", 2)

c3 <- matrix(NA, 2, 1)
c3[,1] <- c(6, 9)
rownames(c3) <- rep("VPS", 2)

c4 <- matrix(NA, 2, 2)
c4[1,] <- c(4, 1)
c4[2,] <- c(7, 4)
rownames(c4) <- rep("BE", 2)

c5 <- matrix(NA, 2, 2)
c5[1,] <- c(5, 2)
c5[2,] <- c(8, 5)
rownames(c5) <- rep("BE", 2)

c6 <- matrix(NA, 2, 2)
c6[1,] <- c(6, 3)
c6[2,] <- c(9, 6)
rownames(c6) <- rep("BE", 2)

c7 <- matrix(NA, 2, 2)
c7[1,] <- c(5, 1)
c7[2,] <- c(8, 4)
rownames(c7) <- rep("BE", 2)

c8 <- matrix(NA, 2, 2)
c8[1,] <- c(6, 2)
c8[2,] <- c(9, 5)
rownames(c8) <- rep("BE", 2)

c9 <- matrix(NA, 3, 2)
c9[1,] <- c(1, 1)
c9[2,] <- c(10, 4)
c9[3,] <- c(19, 7)
rownames(c9) <- rep("LY", 3)

c10 <- matrix(NA, 3, 2)
c10[1,] <- c(2, 1)
c10[2,] <- c(11, 4)
c10[3,] <- c(20, 7)
rownames(c10) <- rep("LY", 3)

c11 <- matrix(NA, 3, 2)
c11[1,] <- c(3, 1)
c11[2,] <- c(12, 4)
c11[3,] <- c(21, 7)
rownames(c11) <- rep("LY", 3)

c12 <- matrix(NA, 3, 2)
c12[1,] <- c(4, 2)
c12[2,] <- c(13, 5)
c12[3,] <- c(22, 8)
rownames(c12) <- rep("LY", 3)

c13 <- matrix(NA, 3, 2)
c13[1,] <- c(5, 2)
c13[2,] <- c(14, 5)
c13[3,] <- c(23, 8)
rownames(c13) <- rep("LY", 3)

c14 <- matrix(NA, 3, 2)
c14[1,] <- c(6, 2)
c14[2,] <- c(15, 5)
c14[3,] <- c(24, 8)
rownames(c14) <- rep("LY", 3)

c15 <- matrix(NA, 3, 2)
c15[1,] <- c(7, 3)
c15[2,] <- c(16, 6)
c15[3,] <- c(25, 9)
rownames(c15) <- rep("LY", 3)

c16 <- matrix(NA, 3, 2)
c16[1,] <- c(8, 3)
c16[2,] <- c(17, 6)
c16[3,] <- c(26, 9)
rownames(c16) <- rep("LY", 3)

c17 <- matrix(NA, 3, 2)
c17[1,] <- c(9, 3)
c17[2,] <- c(18, 6)
c17[3,] <- c(27, 9)
rownames(c17) <- rep("LY", 3)

con <- simEqualCon(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, modelType="SEM", conBeforeFill=FALSE)


datModel <- simData(longMed, 200, equalCon=con)
SimModel <- simModel(longMed, equalCon=con)
output <- sim(1000, datModel, SimModel)

LY2 <- matrix(0, 9, 3)
LY2[1:3, 1] <- NA
LY2[4:6, 2] <- NA
LY2[7:9, 3] <- NA
BE2 <- matrix(0, 3, 3)
BE2[2,1] <- NA
BE2[3,2] <- NA
crossMed <- simParamSEM(LY=LY2, BE=BE2)
SimModel2 <- simModel(crossMed, indLab=19:27)
output2 <- sim(100, datModel, SimModel2)

n05 <- simNorm(0, 0.05)
pathMis <- matrix(0, 9, 9)
pathMis[6, 1] <- pathMis[9, 4] <- NA
BEMis <- bind(pathMis, "n05")
longMedMis <- simMisspecSEM(BE=BEMis, misBeforeFill=FALSE, misBeforeCon=FALSE)
datModel <- simData(longMed, 200, misspec=longMedMis, equalCon=con)

############################## Example 17 ###########################

#library(simsem)
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTE <- binds(error.cor)

CFA.Model <- simSetCFA(LY = LY, RPS = RPS, RTE = RTE)

SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
Output <- sim(NULL, SimData, SimModel, n=50:1000)
summary(Output)
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200)
findPower(Cpow, "N", 0.80)
plotPower(Output, powerParam=c("LY1_1", "PS2_1"))


#Output <- sim(1000, SimData, SimModel, n=simUnif(50, 1000))

############################## Example 18 ###########################

#library(simsem)

loading <- matrix(0, 5, 3)
loading[1,1] <- 1
loading[2:5,2] <- 1
loading[2:5,3] <- 0:3
LY <- bind(loading)

facMean <- rep(NA, 3)
facMeanVal <- c(0.5, 5, 2)
AL <- simVector(facMean, facMeanVal)

facVar <- rep(NA, 3)
facVarVal <- c(0.25, 1, 0.25)
VPS <- simVector(facVar, facVarVal)

facCor <- diag(3)
facCor[2,3] <- NA
facCor[3,2] <- NA
RPS <- binds(facCor, 0.5)

VTE <- simVector(c(0, rep(NA, 4)), 1.2)

RTE <- binds(diag(5))

TY <- simVector(rep(0, 5))

path <- matrix(0, 3, 3)
path[2,1] <- NA
path[3,1] <- NA
pathVal <- matrix(0, 3, 3)
pathVal[2,1] <- 0.5
pathVal[3,1] <- 0.1
BE <- bind(path, pathVal)

LCA.Model <- simSetSEM(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY, BE=BE)

u1 <- simUnif(-0.1, 0.1)

loading.trivial <- matrix(0, 5, 3)
loading.trivial[3:4, 3] <- NA
loading.mis <- bind(loading.trivial, "u1")

LCA.Mis <- simMisspecSEM(LY = loading.mis)

group <- simBinom(1, 0.5)
n01 <- simNorm(0, 1)
facDist <- simDataDist(group, n01, n01, keepScale=c(FALSE, TRUE, TRUE))

datTemplate <- simData(LCA.Model, 300, LCA.Mis, sequential=TRUE, facDist=facDist)
model <- simModel(LCA.Model)

Output <- sim(NULL, datTemplate, model, n=seq(50, 500, 5), pmMCAR=seq(0, 0.4, 0.1))

plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200, pmMCARval=0)
getCutoff(Output, 0.05, nVal = 300, pmMCARval=0.33)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200, pmMCARval=0.35)
findPower(Cpow, "N", 0.80)
findPower(Cpow, "MCAR", 0.80)
plotPower(Output, powerParam=c("BE2_1", "BE3_1"))




#Output <- sim(1000, datTemplate, model, n=simUnif(50, 500), pmMCAR=simUnif(0, 0.5))

###################################### Example 19 #################################

#library(simsem)
u39 <- simUnif(0.3, 0.9)
u09 <- simUnif(0, 0.9)
loading <- matrix(0, 10, 2)
loading[1:6, 1] <- NA
loading[7:10, 2] <- NA
LY <- bind(loading, "u39")

RPS <- binds(diag(2))

RTE <- binds(diag(10))

path <- matrix(0, 2, 2)
path[2, 1] <- NA
BE <- bind(path, "u09")

latentReg <- simSetSEM(LY = LY, RPS = RPS, RTE = RTE, BE = BE)

SimData <- simData(latentReg, 500)
SimModel <- simModel(latentReg)
Output <- sim(NULL, SimData, SimModel, n=25:500)
summary(Output)
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, n = 200)	

Cpow <- getPower(Output, contParam="BE2_1")
Cpow2 <- getPower(Output, contParam="BE2_1", nVal = 200, paramVal=seq(0.1, 0.9, 0.1))

targetVal <- list("BE2_1" = seq(0.1, 0.9, 0.1), "LY1_1" = c(0.5, 0.7))
Cpow3 <- getPower(Output, contParam=c("BE2_1", "LY1_1"), nVal = 200, paramVal=targetVal)

findPower(Cpow, 1, 0.80)
findPower(Cpow, 2, 0.80)

plotPower(Output, powerParam=c("BE2_1", "LY10_2"), contParam="BE2_1")

###################################### Example 20 Get power fit continutous N ################################# 

#library(simsem)

n1 <- simNorm(0, 0.1)

loading.null <- matrix(0, 8, 2)
loading.null[1:5, 1] <- NA
loading.null[6:8, 2] <- NA
LY.NULL <- bind(loading.null, 0.7)
latent.cor.null <- matrix(NA, 2, 2)
diag(latent.cor.null) <- 1
RPS <- binds(latent.cor.null, 0.5)
RTE <- binds(diag(8))
CFA.Model.NULL <- simSetCFA(LY = LY.NULL, RPS = RPS, RTE = RTE)

error.cor.mis <- matrix(NA, 8, 8)
diag(error.cor.mis) <- 1
RTE.Mis <- binds(error.cor.mis, "n1")
CFA.Model.Mis <- simMisspecCFA(RTE = RTE.Mis)

loading.alt <- matrix(0, 8, 2)
loading.alt[1:4, 1] <- NA
loading.alt[5:8, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LY.ALT, RPS = RPS, RTE = RTE)

SimData.NULL <- simData(CFA.Model.NULL, 500, misspec=CFA.Model.Mis)
SimData.ALT <- simData(CFA.Model.ALT, 500, misspec=CFA.Model.Mis)

SimModel <- simModel(CFA.Model.NULL)

Output.NULL <- sim(NULL, SimData.NULL, SimModel, n=25:500)
Output.ALT <- sim(NULL, SimData.ALT, SimModel, n=25:500)

cutoff <- getCutoff(Output.NULL, alpha=0.05, nVal=250)
plotCutoff(Output.NULL, alpha=0.05)
getPowerFit(Output.ALT, nullObject=Output.NULL, alpha=0.05, nVal=250)
getPowerFit(Output.ALT, cutoff=cutoff, nVal=250, condCutoff=TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, logistic=FALSE)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff=cutoff2, nVal=250, condCutoff=FALSE)
plotPowerFit(Output.ALT, cutoff=cutoff2)
plotPowerFit(Output.ALT, cutoff=cutoff2, logistic=FALSE)


###################################### Example 21 Get power continutous N and pmMCAR ################################# 

#library(simsem)

n05 <- simNorm(0, 0.05)

path.null <- matrix(0, 5, 5)
path.null[2, 1] <- NA
path.null[3, 2] <- NA
path.null[4, 3] <- NA
path.null[5, 4] <- NA
BE.null <- bind(path.null, 0.4)

residual <- diag(5)
RPS <- binds(residual)

path.model.null <- simSetPath(RPS = RPS, BE = BE.null)

path.null.mis <- matrix(0, 5, 5)
path.null.mis[3:5, 1] <- NA
path.null.mis[4:5, 2] <- NA
path.null.mis[5, 3] <- NA
BE.null.mis <- bind(path.null.mis, "n05")

path.model.null.mis <- simMisspecPath(BE = BE.null.mis)

path.alt <- matrix(0, 5, 5)
path.alt[2:3, 1] <- NA
path.alt[4, 2:3] <- NA
path.alt[5, 4] <- NA
BE.alt <- bind(path.alt, 0.4)
path.model.alt <- simSetPath(RPS = RPS, BE = BE.alt)

path.alt.mis <- matrix(0, 5, 5)
path.alt.mis[4:5, 1] <- NA
path.alt.mis[5, 2:3] <- NA
BE.alt.mis <- bind(path.alt.mis, "n05")

path.model.alt.mis <- simMisspecPath(BE = BE.alt.mis)

SimData.NULL <- simData(path.model.null, 500, misspec=path.model.null.mis)
SimData.ALT <- simData(path.model.alt, 500, misspec=path.model.alt.mis)

SimModel <- simModel(path.model.null)

Output.NULL <- sim(NULL, SimData.NULL, SimModel, n=25:500, pmMCAR=seq(0, 0.3, 0.1))
Output.ALT <- sim(NULL, SimData.ALT, SimModel, n=25:500, pmMCAR=seq(0, 0.3, 0.1))

cutoff <- getCutoff(Output.NULL, alpha=0.05, nVal=250, pmMCARval = 0.2)
plotCutoff(Output.NULL, alpha=0.05)
getPowerFit(Output.ALT, nullObject=Output.NULL, alpha=0.05, nVal=250, pmMCARval = 0.2)
getPowerFit(Output.ALT, cutoff=cutoff, nVal=250, pmMCARval = 0.2, condCutoff=TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff=cutoff2, nVal=250, pmMCARval = 0.2, condCutoff=FALSE)
plotPowerFit(Output.ALT, cutoff=cutoff2)

###################################### Example 22 Specifying misspecification ################################# 

#library(simsem)
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
model <- simParamCFA(LY=loading)
analyzeModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
out <- run(analyzeModel, HolzingerSwineford1939)

simOut1 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000)
getCutoff(simOut1, alpha=0.05)
pValue(out, simOut1)

loadingMis2 <- matrix(0, 9, 3)
loadingMis2[1,2] <- NA
loadingMis2[4,3] <- NA
LYMis2 <- bind(loadingMis2, 0.3)
misspec2 <- simMisspecCFA(LY=LYMis2, misBeforeFill=FALSE)
simOut2 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec2) 
getCutoff(simOut2, alpha=0.05)
pValue(out, simOut2)

loadingMis3 <- matrix(0, 9, 3)
loadingMis3[6,1] <- NA
loadingMis3[9,2] <- NA
LYMis3 <- bind(loadingMis3, 0.3)
misspec3 <- simMisspecCFA(LY=LYMis3, misBeforeFill=FALSE)
simOut3 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec3) 
getCutoff(simOut3, alpha=0.05)
pValue(out, simOut3)

u3 <- simUnif(-0.3, 0.3)
loadingMis4 <- matrix(0, 9, 3)
loadingMis4[4:9, 1] <- NA
loadingMis4[c(1:3, 7:9),2] <- NA
loadingMis4[1:6,3] <- NA
LYMis4 <- bind(loadingMis4, "u3")
misspec4 <- simMisspecCFA(LY=LYMis4, misBeforeFill=FALSE)
simOut4 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec4) 
getCutoff(simOut4, alpha=0.05)
pValue(out, simOut4)

n3 <- simNorm(0, 0.15)
loadingMis5 <- matrix(0, 9, 3)
loadingMis5[4:9, 1] <- NA
loadingMis5[c(1:3, 7:9),2] <- NA
loadingMis5[1:6,3] <- NA
LYMis5 <- bind(loadingMis5, "n3")
misspec5 <- simMisspecCFA(LY=LYMis5, misBeforeFill=FALSE)
simOut5 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec5) 
getCutoff(simOut5, alpha=0.05)
pValue(out, simOut5)

u3 <- simUnif(-0.3, 0.3)
loadingMis6 <- matrix(0, 9, 3)
loadingMis6[4:9, 1] <- NA
loadingMis6[c(1:3, 7:9),2] <- NA
loadingMis6[1:6,3] <- NA
LYMis6 <- bind(loadingMis6, "u3")
misspec6 <- simMisspecCFA(LY=LYMis6, optMisfit="max", numIter=100, misBeforeFill=FALSE)
simOut6 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec6) 
getCutoff(simOut6, alpha=0.05)
pValue(out, simOut6)

u1 <- simUnif(-0.1, 0.1)
loadingMis7 <- matrix(0, 9, 3)
loadingMis7[4:9, 1] <- NA
loadingMis7[c(1:3, 7:9),2] <- NA
loadingMis7[1:6,3] <- NA
LYMis7 <- bind(loadingMis7, "u1")
misspec7 <- simMisspecCFA(LY=LYMis7, misfitBound=c(0.02, 0.05), numIter=200, misBeforeFill=FALSE)

simOut7 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec7) 
getCutoff(simOut7, alpha=0.05)
pValue(out, simOut7)

# To be rejected
u69 <- simUnif(0.6, 0.9)
loadingMisAlt <- matrix(0, 9, 3)
loadingMisAlt[4, 1] <- NA
loadingMisAlt[7, 2] <- NA
loadingMisAlt[1, 3] <- NA
LYMisAlt <- bind(loadingMisAlt, "u69")
misspecAlt <- simMisspecCFA(LY=LYMisAlt, optMisfit="min", numIter=100, misBeforeFill=FALSE)
simOutAlt <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspecAlt) 
getPowerFit(simOutAlt, nullObject=simOut1) 
getPowerFit(simOutAlt, nullObject=simOut2) 
getPowerFit(simOutAlt, nullObject=simOut3) 
getPowerFit(simOutAlt, nullObject=simOut4) 
getPowerFit(simOutAlt, nullObject=simOut5) 
getPowerFit(simOutAlt, nullObject=simOut6) 
getPowerFit(simOutAlt, nullObject=simOut7) 

# Population Misfit Investigation

param1 <- runFitParam(analyzeModel, data=HolzingerSwineford1939)
param2 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec2)
param3 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec3)
param4 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec4)
param5 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec5)
param6 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec6)
param7 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec7)
paramAlt <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspecAlt)

summary(param4)
summaryParam(param4)
summaryMisspec(param4)
summaryFit(param4)
plotMisfit(param4, misParam="LY9_1")

###################################### Example 23 nested model comparison and power ################################# 

# longitudinal weak invariance
#library(simsem)

loading <- matrix(0, 9, 3)
loading[1, 1] <- 1
loading[2:3, 1] <- NA
loading[4, 2] <- 1
loading[5:6, 2] <- NA
loading[7, 3] <- 1
loading[8:9, 3] <- NA
LY <- bind(loading, "runif(1, 0.5, 1.5)")

facCor <- matrix(NA, 3, 3)
diag(facCor) <- 1
facCorVal <- diag(3)
facCorVal[1, 2] <- facCorVal[2, 1] <- 0.7
facCorVal[2, 3] <- facCorVal[3, 2] <- 0.7
facCorVal[1, 3] <- facCorVal[3, 1] <- 0.49
RPS <- binds(facCor, facCorVal)

VE <- simVector(rep(NA, 3), c(1, 1.2, 1.4))

error <- diag(9)
error[1, 4] <- error[4, 7] <- error[4, 1] <- error[7, 4] <- NA
error[2, 5] <- error[5, 8] <- error[5, 2] <- error[8, 5] <- NA
error[3, 6] <- error[6, 9] <- error[6, 3] <- error[9, 6] <- NA
error[1, 7] <- error[7, 1] <- NA
error[2, 8] <- error[8, 2] <- NA
error[3, 9] <- error[9, 3] <- NA
errorVal <- diag(9)
errorVal[1, 4] <- errorVal[4, 7] <- errorVal[4, 1] <- errorVal[7, 4] <- 0.2
errorVal[2, 5] <- errorVal[5, 8] <- errorVal[5, 2] <- errorVal[8, 5] <- 0.2
errorVal[3, 6] <- errorVal[6, 9] <- errorVal[6, 3] <- errorVal[9, 6] <- 0.2
errorVal[1, 7] <- errorVal[7, 1] <- 0.04
errorVal[2, 8] <- errorVal[8, 2] <- 0.04
errorVal[3, 9] <- errorVal[9, 3] <- 0.04
RTE <- binds(error, errorVal)

VTE <- simVector(rep(NA, 9), 0.4)

longCFA <- simSetCFA(LY=LY, RPS=RPS, VE=VE, RTE=RTE, VTE=VTE)

con1 <- matrix(0, 3, 2)
con1[1,] <- c(2, 1)
con1[2,] <- c(5, 2)
con1[3,] <- c(8, 3)
rownames(con1) <- rep("LY", 3)
con2 <- matrix(0, 3, 2)
con2[1,] <- c(3, 1)
con2[2,] <- c(6, 2)
con2[3,] <- c(9, 3)
rownames(con2) <- rep("LY", 3)
equalCon <- simEqualCon(con1, con2, modelType="CFA")

# Trivial misspecification
loadingMis <- matrix(0, 9, 3)
loadingMis[2:3, 1] <- NA
loadingMis[5:6, 2] <- NA
loadingMis[8:9, 3] <- NA
LYMis <- bind(loadingMis, "runif(1, -0.1, 0.1)") 

longCFAMis <- simMisspecCFA(LY=LYMis)

datNested <- simData(longCFA, 200, misspec=longCFAMis, equalCon=equalCon)
datParent <- simData(longCFA, 200, misspec=longCFAMis)

modNested <- simModel(longCFA, equalCon=equalCon)
modParent <- simModel(longCFA)

outDatNestedModNested <- sim(1000, datNested, modNested)
outDatNestedModParent <- sim(1000, datNested, modParent)

anova(outDatNestedModNested, outDatNestedModParent)
cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- sim(1000, datParent, modNested)
outDatParentModParent <- sim(1000, datParent, modParent)

anova(outDatParentModNested, outDatParentModParent)

getPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, usedFit="RMSEA")

cutoff2 <- c(Chi=3.84, CFI=-0.01)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, cutoff=cutoff2)



###################################### Example 24 nested model comparison and power continuous N ################################# 

# longitudinal strong invariance

#library(simsem)

loading <- matrix(0, 9, 3)
loading[1, 1] <- 1
loading[2:3, 1] <- NA
loading[4, 2] <- 1
loading[5:6, 2] <- NA
loading[7, 3] <- 1
loading[8:9, 3] <- NA
LY <- bind(loading, "runif(1, 0.5, 1.5)")

facCor <- matrix(NA, 3, 3)
diag(facCor) <- 1
facCorVal <- diag(3)
facCorVal[1, 2] <- facCorVal[2, 1] <- 0.7
facCorVal[2, 3] <- facCorVal[3, 2] <- 0.7
facCorVal[1, 3] <- facCorVal[3, 1] <- 0.49
RPS <- binds(facCor, facCorVal)

VE <- simVector(rep(NA, 3), c(1, 1.2, 1.4))

error <- diag(9)
error[1, 4] <- error[4, 7] <- error[4, 1] <- error[7, 4] <- NA
error[2, 5] <- error[5, 8] <- error[5, 2] <- error[8, 5] <- NA
error[3, 6] <- error[6, 9] <- error[6, 3] <- error[9, 6] <- NA
error[1, 7] <- error[7, 1] <- NA
error[2, 8] <- error[8, 2] <- NA
error[3, 9] <- error[9, 3] <- NA
errorVal <- diag(9)
errorVal[1, 4] <- errorVal[4, 7] <- errorVal[4, 1] <- errorVal[7, 4] <- 0.2
errorVal[2, 5] <- errorVal[5, 8] <- errorVal[5, 2] <- errorVal[8, 5] <- 0.2
errorVal[3, 6] <- errorVal[6, 9] <- errorVal[6, 3] <- errorVal[9, 6] <- 0.2
errorVal[1, 7] <- errorVal[7, 1] <- 0.04
errorVal[2, 8] <- errorVal[8, 2] <- 0.04
errorVal[3, 9] <- errorVal[9, 3] <- 0.04
RTE <- binds(error, errorVal)

VTE <- simVector(rep(NA, 9), 0.4)

TY <- simVector(c(0, NA, NA, 0, NA, NA, 0, NA, NA), "runif(1, -0.5, 0.5)")

AL <- simVector(rep(NA, 3), c(0, 0.5, 1))

longCFA <- simSetCFA(LY=LY, RPS=RPS, VE=VE, RTE=RTE, VTE=VTE, TY=TY, AL=AL)

con1 <- matrix(0, 3, 2)
con1[1,] <- c(2, 1)
con1[2,] <- c(5, 2)
con1[3,] <- c(8, 3)
rownames(con1) <- rep("LY", 3)
con2 <- matrix(0, 3, 2)
con2[1,] <- c(3, 1)
con2[2,] <- c(6, 2)
con2[3,] <- c(9, 3)
rownames(con2) <- rep("LY", 3)
con3 <- matrix(c(2, 5, 8), ncol=1)
rownames(con3) <- rep("TY", 3)
con4 <- matrix(c(3, 6, 9), ncol=1)
rownames(con4) <- rep("TY", 3)

equalCon1 <- simEqualCon(con1, con2, modelType="CFA")
equalCon2 <- simEqualCon(con1, con2, con3, con4, modelType="CFA")

# Trivial misspecification
loadingMis <- matrix(0, 9, 3)
loadingMis[2:3, 1] <- NA
loadingMis[5:6, 2] <- NA
loadingMis[8:9, 3] <- NA
LYMis <- bind(loadingMis, "runif(1, -0.1, 0.1)") 

TYMis <- simVector(c(0, NA, NA, 0, NA, NA, 0, NA, NA), "runif(1, -0.1, 0.1)")

longCFAMis <- simMisspecCFA(LY=LYMis, TY=TYMis)

datNested <- simData(longCFA, 200, misspec=longCFAMis, equalCon=equalCon2)
datParent <- simData(longCFA, 200, misspec=longCFAMis, equalCon=equalCon1)

modNested <- simModel(longCFA, equalCon=equalCon2)
modParent <- simModel(longCFA, equalCon=equalCon1)

outDatNestedModNested <- sim(NULL, datNested, modNested, n=50:500)
outDatNestedModParent <- sim(NULL, datNested, modParent, n=50:500)

anova(outDatNestedModNested, outDatNestedModParent)

cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent, nVal=250)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- sim(NULL, datParent, modNested, n=50:500)
outDatParentModParent <- sim(NULL, datParent, modParent, n=50:500)

anova(outDatParentModNested, outDatParentModParent)

getPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, nVal=250)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff, nVal=250)

plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, usedFit="RMSEA")
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, logistic=FALSE)

cutoff2 <- c(Chi=3.84, CFI=-0.01)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, nVal=250, condCutoff=FALSE)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, logistic=FALSE)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, cutoff=cutoff2, logistic=FALSE)

###################################### Example 25 nested model comparison and power continuous N and pmMCAR ######################

# Equal first-order effect
#library(simsem)

path <- matrix(0, 5, 5)
path[2, 1] <- NA
path[3, 2] <- NA
path[4, 3] <- NA
path[5, 4] <- NA
BE <- bind(path, "runif(1, 0.3, 0.7)")

residual <- diag(5)
RPS <- binds(residual)

pathModel <- simSetPath(RPS = RPS, BE = BE)

con <- matrix(0, 4, 2)
con[1,] <- c(2, 1)
con[2,] <- c(3, 2)
con[3,] <- c(4, 3)
con[4,] <- c(5, 4)
rownames(con) <- rep("BE", 4)
equalCon <- simEqualCon(con, modelType="Path")

pathMis <- matrix(0, 5, 5)
pathMis[2:5, 1] <- NA
pathMis[3:5, 2] <- NA
pathMis[4:5, 3] <- NA
pathMis[5, 4] <- NA
BEMis <- bind(pathMis, "rnorm(1, 0, 0.05)")

pathModelMis <- simMisspecPath(BE = BEMis)

datNested <- simData(pathModel, 200, misspec=pathModelMis, equalCon=equalCon)
datParent <- simData(pathModel, 200, misspec=pathModelMis)

modNested <- simModel(pathModel, equalCon=equalCon)
modParent <- simModel(pathModel)

outDatNestedModNested <- sim(NULL, datNested, modNested, n=50:500, pmMCAR=seq(0, 0.3, 0.1))
outDatNestedModParent <- sim(NULL, datNested, modParent, n=50:500, pmMCAR=seq(0, 0.3, 0.1))

anova(outDatNestedModNested, outDatNestedModParent)

cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent, nVal=250, pmMCARval=0.2)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- sim(NULL, datParent, modNested, n=50:500, pmMCAR=seq(0, 0.3, 0.1))
outDatParentModParent <- sim(NULL, datParent, modParent, n=50:500, pmMCAR=seq(0, 0.3, 0.1))

anova(outDatParentModNested, outDatParentModParent)

getPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, nVal=250, pmMCARval=0.2)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff, nVal=250, pmMCARval=0.2)

plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, usedFit="RMSEA")
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, useContour=FALSE)

cutoff2 <- c(Chi=3.84, CFI=-0.01)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, nVal=250, pmMCARval=0.2, condCutoff=FALSE)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, useContour=FALSE)

###################################### Example 26 Analyzing Real Data with Nested Model Comparison ###################################

#library(simsem)
library(lavaan)

LY <- matrix(1, 4, 2)
LY[,2] <- 0:3

PS <- matrix(NA, 2, 2)

TY <- rep(0, 4)

AL <- rep(NA, 2)

TE <- diag(NA, 4)

linearModel <- simParamCFA(LY=LY, PS=PS, TY=TY, AL=AL, TE=TE)

LY2 <- matrix(1, 4, 2)
LY2[,2] <- c(0, NA, NA, 3)

unconstrainModel <- simParamCFA(LY=LY2, PS=PS, TY=TY, AL=AL, TE=TE)

nested <- simModel(linearModel, indLab=paste("t", 1:4, sep=""))
parent <- simModel(unconstrainModel, indLab=paste("t", 1:4, sep=""))

outNested <- run(nested, Demo.growth)
outParent <- run(parent, Demo.growth)

loadingMis <- matrix(0, 4, 2)
loadingMis[2:3, 2] <- NA
LYmis <- bind(loadingMis, "runif(1, -0.1, 0.1)")


linearMis <- simMisspecCFA(LY=LYmis)

simNestedNested <- runFit(model=nested, data=Demo.growth, nRep=1000, misspec=linearMis)
simNestedParent <- runFit(model=nested, data=Demo.growth, nRep=1000, misspec=linearMis, analyzeModel=parent)
simParentNested <- runFit(model=parent, data=Demo.growth, nRep=1000, analyzeModel=nested)
simParentParent <- runFit(model=parent, data=Demo.growth, nRep=1000)

pValueNested(outNested, outParent, simNestedNested, simNestedParent)
getPowerFitNested(simParentNested, simParentParent, nullNested=simNestedNested, nullParent=simNestedParent)

###################################### Example 27 nonnested model comparison ################################# 

#library(simsem)

# Greg Hancock Paper pp.58-59.

# Model 1

loading <- matrix(0, 8, 4)
loading[1:4, 1] <- 1
loading[1:4, 2] <- 0:3
loading[5:8, 3] <- 1
loading[5:8, 4] <- 0:3
LY <- bind(loading)

RTE <- binds(diag(8))
VTE <- bind(rep(NA, 8), 0.5)
TY <- bind(rep(0, 8))

AL <- bind(rep(NA, 4), c(5, 2, 5, 2))
VPS <- bind(rep(NA, 4), c(1, 0.25, 1, 0.25))
facCorA <- diag(4)
facCorA[1, 3] <- facCorA[3, 1] <- NA
RPSA <- binds(facCorA, 0.3)

facCorB <- diag(4)
facCorB[2, 4] <- facCorB[4, 2] <- NA
RPSB <- binds(facCorB, 0.3)

modelA <- simSetCFA(LY=LY, TY=TY, RTE=RTE, VTE=VTE, AL=AL, VPS=VPS, RPS=RPSA)
modelB <- simSetCFA(LY=LY, TY=TY, RTE=RTE, VTE=VTE, AL=AL, VPS=VPS, RPS=RPSB)

dataA <- simData(modelA, 200)
dataB <- simData(modelB, 200)

analyzeA <- simModel(modelA)
analyzeB <- simModel(modelB)


outAA <- sim(1000, dataA, analyzeA)
outBA <- sim(1000, dataB, analyzeA)
outAB <- sim(1000, dataA, analyzeB)
outBB <- sim(1000, dataB, analyzeB)

# anova for nonnested model comparison
anova(outAA, outAB)
anova(outBB, outBA)

getCutoffNonNested(outAA, outAB, outBA, outBB)
getCutoffNonNested(outAA, outAB)
getCutoffNonNested(outBB, outBA)
getCutoffNonNested(outAA, outAB, outBA, outBB, onetailed=TRUE)
plotCutoffNonNested(outAA, outAB, outBA, outBB, alpha=0.05)
plotCutoffNonNested(outAA, outAB, outBA, outBB, alpha=0.05, onetailed=TRUE)

getPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB, usedFit="RMSEA")

cutoff <- c(AIC=0, BIC=0)
cutoff2 <- c(AIC=2, BIC=2)
getPowerFitNonNested(outBA, outBB, cutoff=cutoff)
getPowerFitNonNested(outBA, outBB, cutoff=cutoff2)
plotPowerFitNonNested(outBA, outBB, cutoff=cutoff2)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB, cutoff=cutoff2)

###################################### Example 28 nonnested model comparison and power continuous N #################################

# Quadratic and unconstrained change
#library(simsem)

loadingA <- matrix(0, 5, 2)
loadingA[1:5, 1] <- 1
loadingA[1:5, 2] <- c(0, NA, NA, NA, 4)
loadingValA <- matrix(0, 5, 2)
loadingValA[1:5, 2] <- c(0, 2, 3, 3.67, 4)
LYA <- bind(loadingA, loadingValA)

loadingB <- matrix(0, 5, 3)
loadingB[1:5, 1] <- 1
loadingB[1:5, 2] <- 0:4
loadingB[1:5, 3] <- (0:4)^2
LYB <- bind(loadingB)

RTE <- binds(diag(5))
VTE <- bind(rep(NA, 5), 1.2)
TY <- bind(rep(0, 5))

ALA <- bind(rep(NA, 2), c(5, 2))
VPSA <- bind(rep(NA, 2), c(1, 0.25))
facCorA <- matrix(NA, 2, 2)
diag(facCorA) <- 1
RPSA <- binds(facCorA, -0.4)

ALB <- bind(rep(NA, 3), c(5, 2, -0.15))
VPSB <- bind(rep(NA, 3), c(1, 0.25, 0.1))
facCorB <- matrix(NA, 3, 3)
diag(facCorB) <- 1
facCorValB <- diag(3)
facCorValB[1, 2] <- facCorValB[2, 1] <- -0.4
facCorValB[1, 3] <- facCorValB[3, 1] <- -0.4
facCorValB[2, 3] <- facCorValB[3, 2] <- 0.5
RPSB <- binds(facCorB, facCorValB)

modelA <- simSetCFA(LY=LYA, TY=TY, RTE=RTE, VTE=VTE, AL=ALA, VPS=VPSA, RPS=RPSA)
modelB <- simSetCFA(LY=LYB, TY=TY, RTE=RTE, VTE=VTE, AL=ALB, VPS=VPSB, RPS=RPSB)

dataA <- simData(modelA, 200)
dataB <- simData(modelB, 200)

analyzeA <- simModel(modelA)
analyzeB <- simModel(modelB)


outAA <- sim(NULL, dataA, analyzeA, n = 50:500)
outBA <- sim(NULL, dataB, analyzeA, n = 50:500)
outAB <- sim(NULL, dataA, analyzeB, n = 50:500)
outBB <- sim(NULL, dataB, analyzeB, n = 50:500)

###################################### Example 29 nonnested model comparison and power continuous N and pmMCAR ####################

# This example is simple. Should be in N and pmMCAR

path <- matrix(0, 3, 3)
path[2, 1] <- NA
path[3, 2] <- NA
BE <- bind(path, 0.5)
RPS <- binds(diag(3))
model1 <- simSetPath(BE=BE, RPS=RPS)

path2 <- matrix(0, 3, 3)
path2[3, 1] <- NA
path2[2, 3] <- NA
BE2 <- bind(path2, 0.5)
model2 <- simSetPath(BE=BE2, RPS=RPS)

data1 <- simData(model1, 60)
data2 <- simData(model2, 60)

analyze1 <- simModel(model1)
analyze2 <- simModel(model2)


out11 <- sim(1000, data1, analyze1)
out21 <- sim(1000, data2, analyze1)
out12 <- sim(1000, data1, analyze2)
out22 <- sim(1000, data2, analyze2)


dat1 <- run(data1)
dat2 <- run(data2)
result11 <- run(analyze1, dat1)
result12 <- run(analyze2, dat1)
result21 <- run(analyze1, dat2)
result22 <- run(analyze2, dat2)


likRatioFit(result11, result12, out11, out12, out21, out22)
likRatioFit(result21, result22, out11, out12, out21, out22) 

getCutoffNonNested(out11, out12, out21, out22)

getPowerFitNonNested(out21, out22, dat1Mod1=out11, dat1Mod2=out12)
getPowerFitNonNested(out21, out22, cutoff=c(AIC=0, BIC=0))

plotCutoffNonNested(out11, out12, onetailed=TRUE)
plotCutoffNonNested(out22, out21, onetailed=TRUE)
plotCutoffNonNested(out11, out12, out21, out22)
plotCutoffNonNested(out11, out12, out21, out22, onetailed=TRUE)

plotPowerFitNonNested(out21, out22, out11, out12)

pValueNonNested(result11, result12, out11, out12, out21, out22)

###################################### Example 30 Nonnested model by the real data ####################

#library(simsem)
library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path.A <- matrix(0, 3, 3)
path.A[2:3, 1] <- NA
path.A[3, 2] <- NA
param.A <- simParamSEM(LY=loading, BE=path.A)

model.A <- simModel(param.A, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
out.A <- run(model.A, PoliticalDemocracy)

path.B <- matrix(0, 3, 3)
path.B[1:2, 3] <- NA
path.B[1, 2] <- NA
param.B <- simParamSEM(LY=loading, BE=path.B)

model.B <- simModel(param.B, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
out.B <- run(model.B, PoliticalDemocracy)

u2 <- simUnif(-0.2, 0.2)
loading.mis <- matrix(NA, 11, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- bind(loading.mis, "u2")
misspec <- simMisspecSEM(LY=LY.mis)

output.A.A <- runFit(model.A, PoliticalDemocracy, 10, misspec=misspec)
output.A.B <- runFit(model.A, PoliticalDemocracy, 10, misspec=misspec, analyzeModel=model.B)
output.B.A <- runFit(model.B, PoliticalDemocracy, 10, misspec=misspec, analyzeModel=model.A)
output.B.B <- runFit(model.B, PoliticalDemocracy, 10, misspec=misspec)
pValueNonNested(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)

##################################### Add the simParam in the Example 4? ################################
##################################### Time slot in SimResult and SimResultParam ################################
##################################### Add the nonconvergence paramValue ########################################
###################################### Double bootstrap, Bollen-Stine boot, double bollen-stine boot, residual bootstrap


#library(simsem)

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTE.Mis <- binds(error.cor.mis, "rnorm(1,0,0.1)")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTE.Mis)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE)

# loading.alt.mis <- matrix(NA, 6, 2)
# loading.alt.mis[is.na(loading.alt)] <- 0
# LY.alt.mis <- bind(loading.alt.mis, "runif(1,-.2,.2)")
# CFA.Model.alt.mis <- simMisspecCFA(LY = LY.alt.mis, RTE=RTE.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500)
SimData.ALT <- simData(CFA.Model.ALT, 500)

SimModel.NULL <- simModel(CFA.Model.NULL)
SimModel.ALT <- simModel(CFA.Model.ALT)

Output.NULL.NULL <- sim(1000, SimData.NULL, SimModel.NULL)
Output.ALT.NULL <- sim(1000, SimData.ALT, SimModel.NULL)
Output.NULL.ALT <- sim(1000, SimData.NULL, SimModel.ALT)
Output.ALT.ALT <- sim(1000, SimData.ALT, SimModel.ALT)

anova(Output.NULL.NULL, Output.NULL.ALT)
anova(Output.ALT.NULL, Output.ALT.ALT)
getCutoffNested(Output.NULL.NULL, Output.NULL.ALT)
getCutoffNested(Output.ALT.NULL, Output.ALT.ALT)

plotCutoffNested(Output.ALT.NULL, Output.ALT.ALT, alpha=0.05)

getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10))

plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, usedFit="CFI")

# Continuous N


Output.NULL.NULL <- sim(NULL, SimData.NULL, SimModel.NULL, n=50:500)
Output.ALT.NULL <- sim(NULL, SimData.ALT, SimModel.NULL, n=50:500)
Output.NULL.ALT <- sim(NULL, SimData.NULL, SimModel.ALT, n=50:500)
Output.ALT.ALT <- sim(NULL, SimData.ALT, SimModel.ALT, n=50:500)

anova(Output.NULL.NULL, Output.NULL.ALT)
anova(Output.ALT.NULL, Output.ALT.ALT)
getCutoffNested(Output.NULL.NULL, Output.NULL.ALT, nVal = 100)
getCutoffNested(Output.ALT.NULL, Output.ALT.ALT, nVal = 100)

plotCutoffNested(Output.ALT.NULL, Output.ALT.ALT, alpha=0.05)

getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, nVal = 250)
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10), nVal = 250)

plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, usedFit="CFI")

plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, logistic=FALSE)
plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, usedFit="CFI", logistic=FALSE)

###################### Continuous getPower ################################

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTE.Mis <- binds(error.cor.mis, "rnorm(1,0,0.1)")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTE.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500, misspec = CFA.Model.NULL.Mis)
SimModel <- simModel(CFA.Model.NULL)
Output.NULL <- sim(NULL, SimData.NULL, SimModel, n=50:500)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE)

loading.alt.mis <- matrix(NA, 6, 2)
loading.alt.mis[is.na(loading.alt)] <- 0
LY.alt.mis <- bind(loading.alt.mis, "runif(1,-.2,.2)")
CFA.Model.alt.mis <- simMisspecCFA(LY = LY.alt.mis, RTE=RTE.Mis)

SimData.ALT <- simData(CFA.Model.ALT, 500, misspec = CFA.Model.alt.mis)
Output.ALT <- sim(NULL, SimData.ALT, SimModel, n=50:500)

getPowerFit(Output.ALT, nullObject=Output.NULL, nVal=250)

cutoff <- getCutoff(Output.NULL, 0.05, nVal=250)
getPowerFit(Output.ALT, cutoff, nVal=250)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff, nVal=250, condCutoff=FALSE)

plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "SRMR", "CFI"))
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, logistic = FALSE)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "SRMR", "CFI"), logistic = FALSE)

plotPowerFit(Output.ALT, Output.NULL, cutoff=cutoff2, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, cutoff=cutoff2, alpha=0.05, logistic = FALSE)

###################### Continuous getPower ################################

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTE.Mis <- binds(error.cor.mis, "rnorm(1,0,0.1)")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTE.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500, misspec = CFA.Model.NULL.Mis)
SimModel <- simModel(CFA.Model.NULL)
Output.NULL <- sim(NULL, SimData.NULL, SimModel, n=50:500, pmMCAR=seq(0, 0.4, 0.1))

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE)

loading.alt.mis <- matrix(NA, 6, 2)
loading.alt.mis[is.na(loading.alt)] <- 0
LY.alt.mis <- bind(loading.alt.mis, "runif(1,-.2,.2)")
CFA.Model.alt.mis <- simMisspecCFA(LY = LY.alt.mis, RTE=RTE.Mis)

SimData.ALT <- simData(CFA.Model.ALT, 500, misspec = CFA.Model.alt.mis)
Output.ALT <- sim(NULL, SimData.ALT, SimModel, n=50:500, pmMCAR=seq(0, 0.4, 0.1))

getPowerFit(Output.ALT, Output.NULL, nVal=250)

cutoff <- getCutoff(Output.NULL, 0.05, nVal=250)
getPowerFit(Output.ALT, cutoff, nVal=250)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff, nVal=250, condCutoff=FALSE)

plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "SRMR", "CFI"))
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, logistic = FALSE)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "SRMR", "CFI"), logistic = FALSE)

plotPowerFit(Output.ALT, Output.NULL, cutoff=cutoff2, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, cutoff=cutoff2, alpha=0.05, logistic = FALSE)

# Fix Example 6 Wiki
# Make sim to save nonconvergent param value and # of replications
