#Include Gamma Hat and Adjusted Gamma Hat in Fit Indices
#TRUE and FALSE + Number of starting values\
#Call it population value instead of starting value
#Summary function put star for fixed parameters (Any nonzero values that is fixed is labelled as stars)

#####Result
# Bias behind each simAnalysis
# Coverage by confidence interval
# Bias from its expected value of random distribution
# Trimed means #

################################## Example 1 ##############################################
#library(simsem)

#install.packages("C:/Users/Sunthud/Desktop/My Dropbox/simsem/simsem_0.0-11.tar.gz", repos=NULL, type="source")
#install.packages("C:/Users/student/Dropbox/simsem/simsem_0.0-11.tar.gz", repos=NULL, type="source")

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

#dir <- "C:/Users/Sunthud/Desktop/My Dropbox/simsem/simsem/R/"
#dir <- "C:/Users/Sunthud/simsem_backup/simsem/R/"
dir <- "C:/Users/student/Dropbox/simsem/simsem/R/"
 source(paste(dir, "AllClass.R", sep=""))
 source(paste(dir, "AllGenerics.R", sep=""))
 sourceDir(dir)

 loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LY = LX, RPH = RPH, RTD = RTD)

SimData <- simData(200, CFA.Model)

SimModel <- simModel(CFA.Model)

#SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
Output <- simResult(200, SimData, SimModel)
#Output <- simResult(100, SimData, SimModel, SimMissing, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

Output2 <- simResult(NULL, SimData, SimModel, n=seq(50, 500, 10), pmMCAR=seq(0, 0.3, 0.05))



mis <- simUnif(0, 0.5)
n <- simUnif(100, 1000)
Output <- simResult(1000, SimData, SimModel, n=n)
plotCutoff(Output2, alpha=0.05, useContour=F)



x <- Output2@pmMCAR
y <- Output2@n
z <- Output2@fit[,"CFI"]
plot3DQtile(x, y, z, qtile=0.95, useContour=F)

#################################### Example 2 #######################

library(simsem)

loading <- matrix(0, 9, 3)
loading[1:3, 1] <- c(1, NA, NA)
loading[4:6, 2] <- c(1, NA, NA)
loading[7:9, 3] <- c(1, NA, NA)
loadingVal <- matrix(0, 9, 3)
loadingVal[2:3, 1] <- c(0.6, 0.7)
loadingVal[5:6, 2] <- c(1.1, 0.9)
loadingVal[8:9, 3] <- c(1.2, 1.1)
LY <- simMatrix(loading, loadingVal)

facCov <- matrix(NA, 3, 3)
facCovVal <- diag(c(0.8, 0.9, 0.4))
facCovVal[lower.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
facCovVal[upper.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
PS <- symMatrix(facCov, facCovVal)

errorCov <- diag(NA, 9)
errorCovVal <- diag(c(0.5, 1.1, 0.8, 0.4, 0.4, 0.8, 0.8, 0.5, 0.6))
TE <- symMatrix(errorCov, errorCovVal)

AL <- simVector(rep(NA, 3), 0)
TY <- simVector(c(0, NA, NA, 0, NA, NA, 0, NA, NA), 0)

HS.Model <- simSetCFA(LY=LY, PS=PS, TE=TE, AL=AL, TY=TY)

SimData <- simData(200, HS.Model)
SimModel <- simModel(HS.Model)
Output <- simResult(200, SimData, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

########################## Example 3 ##########################################

library(simsem)

factor.loading <- matrix(NA, 4, 2)
factor.loading[,1] <- 1
factor.loading[,2] <- 0:3
LY <- simMatrix(factor.loading)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- simVector(factor.mean, factor.mean.starting)

factor.var <- rep(NA, 2)
factor.var.starting <- c(1, 0.25)
VPS <- simVector(factor.var, factor.var.starting)

factor.cor <- matrix(NA, 2, 2)
diag(factor.cor) <- 1
RPS <- symMatrix(factor.cor, 0.5)

VTE <- simVector(rep(NA, 4), 1.2)

RTE <- symMatrix(diag(4))

TY <- simVector(rep(0, 4))

LCA.Model <- simSetCFA(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY)

Data.True <- simData(300, LCA.Model)
SimModel <- simModel(LCA.Model)
#Output <- simResult(100, Data.True, SimModel)
#getCutoff(Output, 0.05)
#plotCutoff(Output, 0.05)

u1 <- simUnif(-0.1, 0.1)

loading.trivial <- matrix(0, 4, 2)
loading.trivial[2:3, 2] <- NA
loading.mis <- simMatrix(loading.trivial, "u1")

LCA.Mis <- simMisspecCFA(LY = loading.mis)

Data.Mis <- simData(300, LCA.Model, LCA.Mis, sequential=TRUE)

Output.Mis <- simResult(100, Data.Mis, SimModel)#, multicore=TRUE)
getCutoff(Output.Mis, 0.05)
plotCutoff(Output.Mis, 0.05)
summaryParam(Output.Mis)

################################# Example 4 ##################################
library(simsem)

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE)

Data <- simData(500, Path.Model)
Data.Mis <- simData(500, Path.Model, Path.Mis.Model)
SimModel <- simModel(Path.Model)


#Output <- simResult(100, Data, SimModel)
Output <- simResult(100, Data.Mis, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# Example 4 with X sides #
library(simsem)

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.GA <- matrix(0, 2, 2)
path.GA[1, 1:2] <- NA
GA <- simMatrix(path.GA, "u35")

path.BE <- matrix(0, 2, 2)
path.BE[2, 1] <- NA
BE <- simMatrix(path.BE, "u57")

exo.cor <- matrix(NA, 2, 2)
diag(exo.cor) <- 1
RPH <- symMatrix(exo.cor, "n31")

RPS <- symMatrix(diag(2))

Path.Model <- simSetPath(RPS = RPS, BE = BE, RPH = RPH, GA = GA, exo=TRUE)

mis.path.GA <- matrix(0, 2, 2)
mis.path.GA[2, 1:2] <- NA
mis.GA <- simMatrix(mis.path.GA, "u1")
Path.Mis.Model <- simMisspecPath(GA = mis.GA, exo=TRUE)

Data.Mis <- simData(500, Path.Model, Path.Mis.Model)
SimModel <- simModel(Path.Model)


Output <- simResult(100, Data.Mis, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

############# Example 5 ################################
library(simsem)

n65 <- simNorm(0.6, 0.05)
u35 <- simUnif(0.3, 0.5)
u68 <- simUnif(0.6, 0.8)
u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)

loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "u68"
LY <- simMatrix(loading, loading.start)

RTE <- symMatrix(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- symMatrix(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "n65"
path.start[3, 2] <- "u35"
BE <- simMatrix(path, path.start)

SEM.model <- simSetSEM(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

loading.trivial <- matrix(NA, 8, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "u2")

error.cor.trivial <- matrix(NA, 8, 8)
diag(error.cor.trivial) <- 0
RTE.trivial <- symMatrix(error.cor.trivial, "n1")

SEM.Mis.Model <- simMisspecSEM(LY = LY.trivial, RTE = RTE.trivial)

constraint <- matrix(0, 2, 2)
constraint[1,] <- c(7, 3)
constraint[2,] <- c(8, 3)
rownames(constraint) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint, modelType="SEM")

Data.Original <- simData(300, SEM.model)
Data.Mis <- simData(300, SEM.model, misspec=SEM.Mis.Model)
Data.Con <- simData(300, SEM.model, equalCon=equal.loading)
Data.Mis.Con <- simData(300, SEM.model, misspec=SEM.Mis.Model, equalCon=equal.loading)

Model.Original <- simModel(SEM.model)
Model.Con <- simModel(SEM.model, equalCon=equal.loading)


Output <- simResult(200, Data.Mis.Con, Model.Con, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# Example 5 Extension: Kernel Regression
ss <- seq(50, 300, 2)
ss <- ss[order(ss)]
Data <- lapply(ss, run, object=Data.Mis.Con)
Result <- lapply(Data, run, object=Model.Con)
converged <- sapply(Result, fun <- function(object) { object@converged } )
fit <- sapply(Result, fun <- function(object) { object@fit } )
fit.Converged <- as.data.frame(t(fit)[converged,])
Estimates <- sapply(Result, fun <- function(object) { c(object@coef@LY[1,1], object@coef@BE[3,1], object@coef@BE[3,2], object@coef@PS[2,1]) } )
Estimates.Converged <- as.data.frame(t(Estimates)[converged,])
SE <- sapply(Result, fun <- function(object) { c(object@se@LY[1,1], object@se@BE[3,1], object@se@BE[3,2], object@se@PS[2,1]) } )
SE.Converged <- as.data.frame(t(SE)[converged,])
ss.Converged <- ss[converged]
z <- Estimates.Converged / SE.Converged
alpha <- 0.05
sig.agg <- abs(z) > pnorm(1 - alpha/2)
library(KernSmooth)
x <- c("LY1_1", "BE3_1", "BE3_2", "PS2_1")
colnames(sig.agg) <- x
obj <- par(mfrow=c(2,2))
for(i in 1:length(x)) {
sig <- sig.agg[,x[i]]
plot(ss.Converged, sig, main=x[i],ylim=c(0,1),xlab="Sample Size", ylab="Power")
lines(lowess(ss.Converged, sig), col="red")
if(mean(sig) > 0.01 && mean(sig) < 0.99) {
h <- dpill(ss.Converged, sig)
fit <- locpoly(ss.Converged, sig, bandwidth = h)
lines(fit)
Model <- glm(sig~ss.Converged,family=binomial(link="logit"))
plot.data <- data.frame(ss.Converged, Model$fitted.values)
plot.data <- plot.data[order(ss.Converged),]
plot.data <- unique(plot.data)
lines(plot.data, col="blue")
}
abline(h = 0.8,col="darkgreen",lwd=3)
}
par(obj)

# Example 5 with X side and stringent constraints #
library(simsem)

n65 <- simNorm(0.6, 0.05)
u35 <- simUnif(0.3, 0.5)
u68 <- simUnif(0.6, 0.8)
u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)

loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
LX <- simMatrix(loading.X, 0.7)

loading.Y <- matrix(NA, 2, 1)
LY <- simMatrix(loading.Y, "u68")

RTD <- symMatrix(diag(6))

RTE <- symMatrix(diag(2))

factor.K.cor <- matrix(NA, 2, 2)
diag(factor.K.cor) <- 1
RPH <- symMatrix(factor.K.cor, 0.5)

RPS <- symMatrix(as.matrix(1))

path.GA <- matrix(NA, 1, 2)
path.GA.start <- matrix(c("n65", "u35"), ncol=2)
GA <- simMatrix(path.GA, path.GA.start)

BE <- simMatrix(as.matrix(0))

SEM.model <- simSetSEM(GA=GA, BE=BE, LX=LX, LY=LY, RPH=RPH, RPS=RPS, RTD=RTD, RTE=RTE, exo=TRUE)

loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X)] <- 0
LX.trivial <- simMatrix(loading.X.trivial, "u2")

error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
RTD.trivial <- symMatrix(error.cor.X.trivial, "n1")

error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
RTE.trivial <- symMatrix(error.cor.Y.trivial, "n1")

RTH.trivial <- simMatrix(matrix(NA, 6, 2), "n1")

SEM.Mis.Model <- simMisspecSEM(LX = LX.trivial, RTE = RTE.trivial, RTD = RTD.trivial, RTH = 
RTH.trivial, exo=TRUE)

constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LX", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LX", 3)
constraint3 <- matrix(1, 2, 2)
constraint3[,1] <- 1:2
rownames(constraint3) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM.exo")

Data.Original <- simData(300, SEM.model)
Data.Mis <- simData(300, SEM.model, misspec=SEM.Mis.Model)
Data.Con <- simData(300, SEM.model, equalCon=equal.loading)
Data.Mis.Con <- simData(300, SEM.model, misspec=SEM.Mis.Model, 
	equalCon=equal.loading)

Model.Original <- simModel(SEM.model)
Model.Con <- simModel(SEM.model, equalCon=equal.loading)

Output <- simResult(100, Data.Mis.Con, Model.Con)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

################### Example 6 ##################################

library(simsem)

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTD.Mis)

SimData.NULL <- simData(500, CFA.Model.NULL, misspec = CFA.Model.NULL.Mis)
SimModel <- simModel(CFA.Model.NULL)
Output.NULL <- simResult(300, SimData.NULL, SimModel)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)

loading.alt.mis <- matrix(NA, 6, 2)
loading.alt.mis[is.na(loading.alt)] <- 0
LX.alt.mis <- simMatrix(loading.alt.mis, "u2")
CFA.Model.alt.mis <- simMisspecCFA(LY = LX.alt.mis, RTE=RTD.Mis)

SimData.ALT <- simData(500, CFA.Model.ALT, misspec = CFA.Model.alt.mis)
Output.ALT <- simResult(300, SimData.ALT, SimModel)

cutoff <- getCutoff(Output.NULL, 0.05)
getPower(Output.ALT, cutoff)
plotPower(Output.ALT, Output.NULL, 0.05)
plotPower(Output.ALT, Output.NULL, 0.05, usedFit=c("RMSEA", "SRMR", "CFI"))

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPower(Output.ALT, cutoff2)
plotPower(Output.ALT, cutoff2)
plotPower(Output.ALT, cutoff2, usedFit=c("RMSEA", "SRMR", "CFI"))

################### Example 6 Extenstion: Kernel Regression ########################
########Null model should vary N too.

ss <- seq(30, 200, 2)
Data <- lapply(ss, run, object=SimData.ALT)
Result <- lapply(Data, run, object=SimModel)
Fit <- sapply(Result, fun <- function(object) { object@fit } )
Convergence <- sapply(Result, fun <- function(object) { object@converged } )
Fit.Converged <- as.data.frame(t(Fit)[Convergence,])
ss.Converged <- ss[Convergence]
cutoff2 <- c(RMSEA = 0.08, CFI = 0.90, TLI = 0.90, SRMR = 0.08)
x <- names(cutoff2)
obj <- par(mfrow=c(2,2))
for(i in 1:length(x)) {
ifelse(i == 2 || i == 3, sig <- Fit.Converged[,x[i]] < cutoff2[x[i]], sig <- Fit.Converged[,x[i]] > cutoff2[x[i]])
library(KernSmooth)
plot(ss.Converged, sig, main=x[i])
h <- dpill(ss.Converged, sig)
fit <- locpoly(ss.Converged, sig, bandwidth = h)
lines(fit)
lines(lowess(ss.Converged, sig), col="red")
Model <- glm(sig~ss.Converged,family=binomial(link="logit"))
plot.data <- data.frame(ss.Converged, Model$fitted.values)
plot.data <- plot.data[order(ss.Converged),]
plot.data <- unique(plot.data)
lines(plot.data, col="blue")
abline(h = 0.8,col="darkgreen",lwd=3)
}
par(obj)

cutoff2 <- find.cutoff(Output.NULL, 0.05, usedFit=x)


################################## Example 7 ##########################################
library(simsem)

u2 <- simUnif(-0.2, 0.2)
u49 <- simUnif(0.4, 0.9)
u36 <- simUnif(0.3, 0.6)
n1 <- simNorm(0, 0.1)
n21 <- simNorm(0.2, 0.1)
n31 <- simNorm(0.3, 0.1)
n41 <- simNorm(0.4, 0.1)

loading <- matrix(0, 9, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[c(1, 4, 7), 4] <- NA
loading.v <- matrix(0, 9, 4)
loading.v[1:3, 1] <- "u49"
loading.v[4:6, 2] <- "u49"
loading.v[7:9, 3] <- "u49"
loading.v[c(1, 4, 7), 4] <- "u36"
LY <- simMatrix(loading, loading.v)

faccor <- diag(4)
faccor[1, 2] <- faccor[2, 1] <- NA
faccor[1, 3] <- faccor[3, 1] <- NA
faccor[2, 3] <- faccor[3, 2] <- NA
faccor.v <- diag(4)
faccor.v[1, 2] <- faccor.v[2, 1] <- "n41"
faccor.v[1, 3] <- faccor.v[3, 1] <- "n21"
faccor.v[2, 3] <- faccor.v[3, 2] <- "n31"
RPS <- symMatrix(faccor, faccor.v)

RTE <- symMatrix(diag(9))

mtmm.model <- simSetCFA(LY=LY, RPS=RPS, RTE=RTE)

error.cor.mis <- matrix(NA, 9, 9)
diag(error.cor.mis) <- 1
RTE.mis <- symMatrix(error.cor.mis, "n1")
loading.mis <- matrix(NA, 9, 4)
loading.mis[is.na(loading)] <- 0
loading.mis[,4] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
mtmm.model.mis <- simMisspecCFA(RTE = RTE.mis, LY=LY.mis)

SimMissing <- simMissing(pmMCAR=0.2, numImps=5)

SimData <- simData(500, mtmm.model, misspec = mtmm.model.mis)
SimModel <- simModel(mtmm.model)

Output <- simResult(10, SimData, SimModel, SimMissing)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

################################## Example 8 ##########################################
library(simsem)

u2 <- simUnif(-0.2, 0.2)
u49 <- simUnif(0.4, 0.9)
u16 <- simUnif(0.1, 0.6)

loading <- matrix(0, 48, 4)
loading[1:12, 1] <- NA
loading[13:24, 2] <- NA
loading[25:36, 3] <- NA
loading[37:48, 4] <- NA
LY <- simMatrix(loading, "u49")

faccor <- matrix(NA, 4, 4)
diag(faccor) <- 1
RPS <- symMatrix(faccor, "u16")

RTE <- symMatrix(diag(48))

CFA.model <- simSetCFA(LY=LY, RPS=RPS, RTE=RTE)

loading.mis <- matrix(NA, 48, 4)
loading.mis[is.na(loading)] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
CFA.model.mis <- simMisspecCFA(LY=LY.mis)

setx <- c(1:3, 13:15, 25:27, 37:39)
set1 <- setx + 3
set2 <- set1 + 3
set3 <- set2 + 3
itemGroups <- list(setx, set1, set2, set3)

SimMissing <- simMissing(nforms=3, itemGroups=itemGroups, numImps=5)

SimData <- simData(1000, CFA.model) #, misspec = CFA.model.mis)
SimModel <- simModel(CFA.model)

dat <- run(SimData)
dat <- run(SimMissing, dat)
out <- run(SimModel, dat, SimMissing)

Output <- simResult(20, SimData, SimModel, SimMissing, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)


############################# Example 9 #############################

library(simsem)

u2 <- simUnif(-0.2, 0.2)
u5 <- simUnif(-0.5, 0.5)
t2 <- simT(2)
t3 <- simT(3)
t4 <- simT(4)
t5 <- simT(5)
chi3 <- simChisq(3)
chi4 <- simChisq(4)
chi5 <- simChisq(5)
chi6 <- simChisq(6)

loading <- matrix(0, 12, 3)
loading[1:4, 1] <- NA
loading[5:8, 2] <- NA
loading[9:12, 3] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u5")

error.cor <- matrix(0, 12, 12)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD) 

loading.mis <- matrix(NA, 12, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
CFA.model.mis <- simMisspecCFA(LY=LY.mis)

SimDataDist <- simDataDist(t2, t3, t4, t5, chi3, chi4, chi5, chi6, chi3, chi4, chi5, chi6, reverse=c(rep(FALSE, 8), rep(TRUE, 4)))
SimData <- simData(200, CFA.Model, misspec=CFA.model.mis, indDist=SimDataDist)
SimModel <- simModel(CFA.Model, estimator="mlm")
Output <- simResult(1000, SimData, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

g21 <- simGamma(2, 1)
n01 <- simNorm(0, 1)
object <- simDataDist(g21)
plotDist(object)
object2 <- simDataDist(g21, n01)
plotDist(object2, r=0.5)

g21 <- simGamma(2, 1)
n01 <- simNorm(0, 1)
chi2 <- simChisq(2)
obj <- simDataDist(g21, n01, chi2)
plotDist(obj, var=c(2,3))

##################################### Example 10 #######################

library(simsem)

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
u3 <- simUnif(-0.3, 0.3)
n1 <- simNorm(0, 0.1)
n31 <- simNorm(0.3, 0.1)
u79 <- simUnif(0.7, 0.9)
chi5 <- simChisq(5)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA
LY <- simMatrix(loading, "u79")

RTE <- symMatrix(diag(12))

SEM.Model <- simSetSEM(RPS = RPS, BE = BE, LY=LY, RTE=RTE)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")

mis.loading <- matrix(NA, 12, 4)
mis.loading[is.na(loading)] <- 0
mis.LY <- simMatrix(mis.loading, "u3")

mis.error.cor <- matrix(NA, 12, 12)
diag(mis.error.cor) <- 0
mis.RTE <- symMatrix(mis.error.cor, "n1")

SEM.Mis.Model <- simMisspecSEM(BE = mis.BE, LY = mis.LY, RTE = mis.RTE)

facDist <- simDataDist(chi5, chi5, n1, n1)

dataTemplate <- simData(500, SEM.Model, SEM.Mis.Model, sequential=TRUE, facDist=facDist)
modelTemplate <- simModel(SEM.Model, estimator="mlr")


simOut <- simResult(100, dataTemplate, modelTemplate, multicore=TRUE)
getCutoff(simOut, 0.05)
plotCutoff(simOut, 0.05)
summaryParam(simOut)

####################################### Example 11 ############################

library(simsem)

u79 <- simUnif(0.7, 0.9)
u5 <- simUnif(-0.5, 0.5)
n01 <- simNorm(0, 1)
c5 <- simChisq(5)

loading <- matrix(0, 5, 3)
loading[1:3, 1] <- NA
loading[4, 2] <- NA
loading[5, 3] <- NA
loadingVal <- matrix(0, 5, 3)
loadingVal[1:3, 1] <- "u79"
loadingVal[4, 2] <- 1
loadingVal[5, 3] <- 1
LY <- simMatrix(loading, loadingVal)

facCor <- diag(3)
facCor[2, 1] <- NA
facCor[1, 2] <- NA
RPS <- symMatrix(facCor, "u5")

path <- matrix(0, 3, 3)
path[3, 1] <- NA
path[3, 2] <- NA
BE <- simMatrix(path, "u5")

RTE <- symMatrix(diag(5))

VY <- simVector(c(NA, NA, NA, 0, 0), 1)

SEM.Model <- simSetSEM(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY)

errorCorMis <- diag(5)
errorCorMis[1:3, 1:3] <- NA
errorCorMis <- diag(5)
RTE.mis <- symMatrix(errorCorMis, n01)

SEM.Model.Mis <- simMisspecSEM(RTE=RTE.mis)

facDist <- simDataDist(n01, c5, n01)

SimData <- simData(200, SEM.Model, misspec=SEM.Model.Mis, sequential=TRUE, facDist=facDist)
SimModel <- simModel(SEM.Model, estimator="mlm")
Output <- simResult(100, SimData, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

###########


u79 <- simUnif(0.7, 0.9)
u5 <- simUnif(-0.5, 0.5)
n01 <- simNorm(0, 1)
c5 <- simChisq(5)

loading <- matrix(0, 5, 3)
loading[1:3, 1] <- NA
loading[4, 2] <- 1
loading[5, 3] <- 1
loadingVal <- matrix(0, 5, 3)
loadingVal[1:3, 1] <- "u79"
LY <- simMatrix(loading, loadingVal)

facCor <- diag(3)
facCor[2, 1] <- NA
facCor[1, 2] <- NA
RPS <- symMatrix(facCor, "u5")


path <- matrix(0, 3, 3)
path[3, 1] <- NA
path[3, 2] <- NA
BE <- simMatrix(path, "u5")

VY <- simVector(c(NA, NA, NA, 0, 0), 1)

VE <- simVector(c(1, NA, NA), c(0, 1, 1))

ME <- simVector(c(0, NA, NA), c(0, 0, 0))

TY <- simVector(c(NA, NA, NA, 0, 0), rep(0, 5))

RTE <- symMatrix(diag(5))

SEM.Model <- simSetSEM(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY, VE=VE, ME=ME, TY=TY)

errorCorMis <- diag(5)
errorCorMis[1:3, 1:3] <- NA
errorCorMis <- diag(5)
RTE.mis <- symMatrix(errorCorMis, n01)

SEM.Model.Mis <- simMisspecSEM(RTE=RTE.mis)


facDist <- simDataDist(n01, c5, n01)

SimData <- simData(200, SEM.Model, misspec=SEM.Model.Mis, sequential=TRUE, facDist=facDist)
 
SimModel <- simModel(SEM.Model, estimator="mlm")

Output <- simResult(100, SimData, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

################################### Example 12 ######################################

library(simsem)

u57 <- simUnif(0.5, 0.7)
u4 <- simUnif(-0.4, 0.4)
u35 <- simUnif(0.3, 0.5)
u2 <- simUnif(-0.2, 0.2)

loading <- matrix(0, 7, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, "u57")

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u35")

error.cor <- diag(7)
error.cor[1:6, 7] <- NA
error.cor[7, 1:6] <- NA
RTD <- symMatrix(error.cor, "u4")

VX <- simVector(rep(NA, 7), 1)

CFA.Model.Aux <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD, VX = VX) 

mis.loading <- matrix(0, 7, 2)
mis.loading[1:3, 2] <- NA
mis.loading[4:6, 1] <- NA
mis.LY <- simMatrix(mis.loading, "u2")

CFA.Mis.Model <- simMisspecCFA(LY = mis.LY)#, RTD = mis.RTD)

SimData <- simData(200, CFA.Model.Aux, misspec = CFA.Mis.Model)

CFA.Model <- extract(CFA.Model.Aux, y=1:6)

data <- run(SimData, dataOnly=F)

SimMissing <- simMissing(pmMAR=0.1, cov=7, numImps=5, threshold = 0.5)

data <- run(SimMissing, data)

SimModel <- simModel(CFA.Model)


out <- run(SimModel, data, simMissing=SimMissing)

Output <- simResult(100, SimData, SimModel, SimMissing)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)


########################################## Example 13 ###################

library(simsem)

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
n01 <- simNorm(0, 1)

loading <- matrix(0, 7, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[1:7, 3] <- NA
loadingVal <- matrix(0, 7, 3)
loadingVal[1:3, 1] <- "u57"
loadingVal[4:6, 2] <- "u57"
loadingVal[1:6, 3] <- "u35"
loadingVal[7, 3] <- 1
LY <- simMatrix(loading, loadingVal)

RPS <- symMatrix(diag(3))

path <- matrix(0, 3, 3)
path[2, 1] <- NA
BE <- simMatrix(path, "u35")

RTE <- symMatrix(diag(7))

VY <- simVector(c(rep(NA, 6), 0), rep(1, 7))

Cov.Model <- simSetSEM(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY)

errorCorMis <- diag(7)
errorCorMis[1:6, 1:6] <- NA
errorCorMis <- diag(7)
RTE.mis <- symMatrix(errorCorMis, n01)

Cov.Model.Mis <- simMisspecSEM(RTE=RTE.mis)

SimData <- simData(200, Cov.Model, misspec=Cov.Model.Mis)
dat <- run(SimData)

model <- "
e1 =~ NA*y1 + NA*y2 + NA*y3
e2 =~ NA*y4 + NA*y5 + NA*y6
e2 ~ NA*e1
e1 ~~ 1*e1
e2 ~~ 1*e2
y1 ~ y7
y2 ~ y7
y3 ~ y7
y4 ~ y7
y5 ~ y7
y6 ~ y7

"

fit <- sem(model, data=dat, meanstructure=TRUE, fixed.x=FALSE)
summary(fit)

# This code is wrong in the write.lavaan.code!!!
model2 <- "
e1 =~ NA*y1 + NA*y2 + NA*y3
e2 =~ NA*y4 + NA*y5 + NA*y6
e3 =~ NA*y1 + NA*y2 + NA*y3 + NA*y4 + NA*y5 + NA*y6 + NA*y7
y7 ~~ 0*y7
e2 ~ NA*e1
e1 ~~ 1*e1
e2 ~~ 1*e2
e3 ~~ 1*e3
e1 ~~ 0*e3
e2 ~~ 0*e3
e1 ~~ 0*e2
"
fit2 <- sem(model2, data=dat, fixed.x=TRUE, meanstructure=TRUE)
summary(fit2)

SimModel <- simModel(Cov.Model)
out <- run(SimModel, dat)

Output <- simResult(100, SimData, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# Add simTransform to provide a new data to residual centering
# Add LH matrix to make a covariate
# Make the default that if LX and TD are not specified in SEM.exo, make them as single-indicator factors.
# find.recursive.set if a row is all 0, give it to 1. And if the column is also 0 too. What should we do? Hide it in comment? !Just check it in the lavaan code
# Then if any factors are covariate, explicitly put the PS to them! See the previous comment




























############################### Single Indicator ###########################

# Allow Factor Variance

loading <- matrix(0, 13, 2)
 
loading[1:12, 1] <- NA
loading[13, 2] <- 1
  
loadingValues <- matrix(0, 13, 2)
loadingValues[1:12, 1] <- 0.6
loadingValues[13, 2] <- 0
LX <- simMatrix(loading, loadingValues)
 
RTD <- symMatrix(diag(13))

VTD <- simVector(c(rep(NA, 12), 0), c(rep(1 - 0.6^2, 12), 0)) 
  
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
  
VPH <- simVector(c(1, NA), c(0, 1))
  
AL <- simVector(c(0, NA), c(0, 0)) 
  
TY <- simVector(c(rep(NA, 12), 0), rep(0, 13))
  
CFA.Full.Model <- simSetCFA(LY=LX, RPS=RPH, RTE=RTD, VTE = VTD, VPS=VPH, AL=AL, TY=TY)
  
SimData <- simData(200, CFA.Full.Model)
run(SimData)

# Allow Factor Loading

# Auxiliary correlates only measurement errors


loading <- matrix(0, 13, 1)
 
loading[1:12, 1] <- NA
  
loadingValues <- matrix(0, 13, 1)
loadingValues[1:12, 1] <- 0.6
LX <- simMatrix(loading, loadingValues)
 

VTE <- simVector(rep(NA, 13), c(rep(0.64, 12), 1))

error.cor <- diag(13)
error.cor[13, 1:12] <- NA
error.cor[1:12, 13] <- NA  
error.val <- diag(13)
error.val[13, 1:12] <- c(rep(0.1, 11), 0.6)
error.val[1:12, 13] <- c(rep(0.1, 11), 0.6)
RTD <- symMatrix(error.cor, error.val)
  
RPH <- symMatrix(diag(1))
 
  
CFA.Full.Model <- simSetCFA(LY=LX, RPS=RPH, RTE=RTD, VTE=VTE)
  
SimData <- simData(200, CFA.Full.Model)
run(SimData)

# Model total variance instead
VY <- simVector(rep(NA, 13), rep(1, 13))
CFA.Full.Model <- simSetCFA(LY=LX, RPS=RPH, RTE=RTD, VY=VY)
SimData <- simData(200, CFA.Full.Model, sequential=TRUE)
run(SimData)


##############################################################################

############# Before Yves Comes ###############

# SimModel run only a part of variables
# Auxiliary variable
# Example of using auxiliary variable

#### Plan

# Extract model that is not related to auxiliary variables --> external (SimSet) (SimRSet)
# SimModel:FIML --> object is the real model and put the auxiliary variable
# SimModel:MI --> use real model only

# Real data --> SimModelOut --> put it in the SimData --> template model for data generation
# Real data --> SimModelBootOut --> put it in the SimData --> template model for data generation




################ To be discussed ############

# Multiple Group
# Categorical Data Analysis

############# Waiting List ####################

# Simulation with parceling
# Sonya's parceling approach


##################################### Auxiliary Variable ############################

sourceDir(path)

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)


loading2 <- matrix(0, 8, 2)
loading2[1:4, 1] <- NA
loading2[5:8, 2] <- NA
LX2 <- simMatrix(loading2, 0.7)

latent.cor2 <- matrix(NA, 2, 2)
diag(latent.cor2) <- 1
RPH2 <- symMatrix(latent.cor2, 0.5)

error.cor2 <- matrix(0, 8, 8)
diag(error.cor2) <- 1
RTD2 <- symMatrix(error.cor2)

CFA.Model2 <- simSetCFA(LX = LX2, RPH = RPH2, RTD = RTD2) #, VX = VX, MX=MX)
CFA.Model <- extract(CFA.Model2, y=c(1:3, 5:7))

SimData <- simData(200, CFA.Model2)

data <- run(SimData)

data[rbinom(200, 1, 0.2)==1,1] <- NA
data[rbinom(200, 1, 0.2)==1,2] <- NA
data[rbinom(200, 1, 0.2)==1,3] <- NA
data[rbinom(200, 1, 0.2)==1,5] <- NA
data[rbinom(200, 1, 0.2)==1,6] <- NA
SimMissing <- simMissing(pmMCAR=0.1, numImps=5)

SimModel <- simModel(CFA.Model, auxiliary=c(4, 8))

out <- run(SimModel, data, simMissing=SimMissing)



Output <- simResult(200, SimData, SimModel)
#Output <- simResult(100, SimData, SimModel, SimMissing, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

############################# Auxiliary 2 #########################################


u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE)

Data <- simData(500, Path.Model)
Data.Mis <- simData(500, Path.Model, Path.Mis.Model)

z <- rnorm(500, 0, 1)
dat <- data.frame(run(Data.Mis), z) 
SimModel <- simModel(Path.Model, auxiliary="z")
out <- run(SimModel, dat)


##################################### Auxiliary Variable 3 ############################

sourceDir(path)

loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7, 3] <- NA
LX <- simMatrix(loading, 0.7)
LX <- adjust(LX, 1, c(7,3), FALSE)

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

error.cor <- diag(8)
error.cor[1:7, 8] <- NA
error.cor[8, 1:7] <- NA
RTD <- symMatrix(error.cor, 0.2)

VTD <- simVector(c(rep(NA, 6), 0, NA), c(rep(0.51, 6), 0, 1))

CFA.Model2 <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD, VTD = VTD)
CFA.Model <- extract(CFA.Model2, y=1:7)

SimData <- simData(200, CFA.Model2)

data <- run(SimData)

SimMissing <- simMissing(pmMCAR=0.1, covs=8, numImps=40)

data <- run(SimMissing, data)

SimModel <- simModel(CFA.Model, auxiliary=8)

out <- run(SimModel, data) #, simMissing=SimMissing)
out2 <- run(SimModel, data, simMissing=SimMissing)

############################## Extract

# Find Single Indicator or Factor with one no covariance --> Then estimate the path from factor instead of fixing as 0 --> make the covariance from the single indicator as 0.

# Single Indicator need an extra DV approach

# Check with rbinom, qbinom, pbinom, dbinom

rordinal <- function(n, prob, start=1) {
	if(sum(prob) != 1) stop("The sum of the probabilities is not 1")
	result <- sample(start:(start + length(prob) - 1), n, replace=TRUE, prob=prob)
	return(result)
}

qordinal <- function(p, prob, start=1) {
	cumProb <- cumsum(prob)
	result <- which(cumProb > p)[1]
	if(p == 1) result <- length(cumProb)
	result <- result - 1 + start
	return(result)
}

pordinal <- function(q, prob, start=1) {
	q <- ceiling(q - 1 + start)
	cumProb <- cumsum(prob)
	if(q < 1) {
		return(0)
	} else if (q > length(prob)) {
		return(1)
	} else {
		return(cumProb[q])
	}
}

dordinal <- function(x, prob, start=1) {
	if(x%%1 != 0) {
		warnings("x must be integer")
		return(0)
	} else {
		q <- q - 1 + start
		return(prob[q])
	}
}

######################### Modeling simParam Only ###############

# What about only one example of auxiliary variable first!
# e.g., simParamCFA(LY=LY, PS=PS, ...)
# SimSet will add the feature of group by imposing it as an argument inside the SimSet. For example, SimSetCFA(group = 2, LY= , LY2 = , ...

# Double definition VTE, VY
# New matrix CPS, CTE, CTD, CPH
# Change to RPS, PTE, RTD, RPH
# Make LKY 

# 

# use it in making simVector, simMatrix
myfun <- function(x) deparse(substitute(x)) 
exists(x) to check whether the funciton exist


# Try


loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
LX <- simMatrix(loading.X, 0.7)

loading.Y <- matrix(NA, 2, 1)
LY <- simMatrix(loading.Y, 0.7)

TD <- symMatrix(diag(rep(NA, 6)), 0.51)

TE <- symMatrix(diag(rep(NA, 2)), 0.51)

factor.K.cor <- matrix(NA, 2, 2)
diag(factor.K.cor) <- 1
PH <- symMatrix(factor.K.cor, 0.5)

PS <- symMatrix(as.matrix(1))

path.GA <- matrix(NA, 1, 2)
GA <- simMatrix(path.GA, 0.4)

BE <- simMatrix(as.matrix(0))

SEM.model <- simSetSEM(GA=GA, BE=BE, LX=LX, LY=LY, PH=PH, PS=PS, TD=TD, TE=TE, exo=TRUE)

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)

loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X)] <- 0
LX.trivial <- simMatrix(loading.X.trivial, "u2")

error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
RTD.trivial <- symMatrix(error.cor.X.trivial, "n1")

error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
RTE.trivial <- symMatrix(error.cor.Y.trivial, "n1")

RTH.trivial <- simMatrix(matrix(NA, 6, 2), "n1")

SEM.Mis.Model <- simMisspecSEM(LX = LX.trivial, TE = RTE.trivial, TD = RTD.trivial, RTH = 
RTH.trivial, exo=TRUE)

constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LX", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LX", 3)
constraint3 <- matrix(1, 2, 2)
constraint3[,1] <- 1:2
rownames(constraint3) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM.exo")

Data.Original <- simData(300, SEM.model)
Data.Mis <- simData(300, SEM.model, misspec=SEM.Mis.Model)
Data.Con <- simData(300, SEM.model, equalCon=equal.loading)
Data.Mis.Con <- simData(300, SEM.model, misspec=SEM.Mis.Model, 
	equalCon=equal.loading)

Model.Original <- simModel(SEM.model)
Model.Con <- simModel(SEM.model, equalCon=equal.loading)

Output <- simResult(100, Data.Mis.Con, Model.Con)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)


####################################### try


 loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LY = LX, RPH = RPH, RTD = RTD)

SimData <- simData(200, CFA.Model)

SimModel <- simModel(CFA.Model)

SimMissing <- simMissing(pmMCAR=0.1, numImps=100)

dat <- run(SimData)
dat <- run(SimMissing, dat)
out <- run(SimModel, dat, SimMissing)




# Add new data
# simParamCFA
# simParamPath !!!!!!!!!!!!!!! Reverse simSetPath
# simParamSEM
# summary SimParam: Add number of parameters
# Do we need to check for identification
# simModel taking simParam
# simModelOut + simMisspec --> simData; Do we need to use standardized parameters?	
# simModelOut --> simModel for starting values
# simModel + Data + simMisspec --> simModelOut + simMisspec --> simResult
# Problem of fixed.x