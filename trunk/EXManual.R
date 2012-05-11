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
#install.packages("C:/Users/student/Dropbox/simsem/simsem_0.1-1.tar.gz", repos=NULL, type="source")

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

library(formatR)
tidy.dir(dir)
 
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

SimData <- simData(CFA.Model, 200)

data <- run(SimData)

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

SimData <- simData(HS.Model, 200)
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

Data.True <- simData(LCA.Model, 300)
SimModel <- simModel(LCA.Model)
#Output <- simResult(100, Data.True, SimModel)
#getCutoff(Output, 0.05)
#plotCutoff(Output, 0.05)

u1 <- simUnif(-0.1, 0.1)

loading.trivial <- matrix(0, 4, 2)
loading.trivial[2:3, 2] <- NA
loading.mis <- simMatrix(loading.trivial, "u1")

LCA.Mis <- simMisspecCFA(LY = loading.mis)

Data.Mis <- simData(LCA.Model, 300, LCA.Mis, sequential=TRUE)

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
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea") #, misfitBound=c(0.05, 0.08))

Data <- simData(Path.Model, 500, maxDraw=1000)
Data.Mis <- simData(Path.Model, 500, Path.Mis.Model)

dat <- run(Data.Mis)
x <- drawParametersMisspec(Path.Model, Path.Mis.Model)
y <- simResultParam(1000, Path.Model, Path.Mis.Model)
plot(y@misspec[,2], y@fit[,2])
lines(loess.smooth(y@misspec[,2], y@fit[,2]), col="red")

SimModel <- simModel(Path.Model)
popMisfit(Path.Model, Path.Mis.Model, fit.measures="rmsea")

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

Data.Mis <- simData(Path.Model, 500, Path.Mis.Model)
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

SEM.Mis.Model <- simMisspecSEM(LY = LY.trivial, RTE = RTE.trivial, conBeforeMis=FALSE, misBeforeFill=TRUE)

constraint <- matrix(0, 2, 2)
constraint[1,] <- c(7, 3)
constraint[2,] <- c(8, 3)
rownames(constraint) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint, modelType="SEM", conBeforeFill=FALSE)

Data.Original <- simData(SEM.model, 300)
Data.Mis <- simData(SEM.model, 300, misspec=SEM.Mis.Model)
Data.Con <- simData(SEM.model, 300, equalCon=equal.loading)
Data.Mis.Con <- simData(SEM.model, 300, misspec=SEM.Mis.Model, equalCon=equal.loading)

dat <- run(Data.Mis.Con)

Model.Original <- simModel(SEM.model)
Model.Con <- simModel(SEM.model, equalCon=equal.loading)


Output <- simResult(200, Data.Mis.Con, Model.Con) #, multicore=TRUE)
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

Data.Original <- simData(SEM.model, 300)
Data.Mis <- simData(SEM.model, 300, misspec=SEM.Mis.Model)
Data.Con <- simData(SEM.model, 300, equalCon=equal.loading)
Data.Mis.Con <- simData(SEM.model, 300, misspec=SEM.Mis.Model, 
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

SimData.NULL <- simData(CFA.Model.NULL, 500, misspec = CFA.Model.NULL.Mis)
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

SimData.ALT <- simData(CFA.Model.ALT, 500, misspec = CFA.Model.alt.mis)
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

SimData <- simData(mtmm.model, 500, misspec = mtmm.model.mis)
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

SimData <- simData(CFA.model, 1000) #, misspec = CFA.model.mis)
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
SimData <- simData(CFA.Model, 200, misspec=CFA.model.mis, indDist=SimDataDist)
SimModel <- simModel(CFA.Model, estimator="mlm")

dat <- run(SimData)
SimData2 <- simData(CFA.Model, 200, misspec=CFA.model.mis, indDist=SimDataDist, modelBoot=TRUE, realData=dat)


Output <- simResult(1000, SimData2, SimModel)
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

dataTemplate <- simData(SEM.Model, 500, SEM.Mis.Model, sequential=TRUE, facDist=facDist)
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

SimData <- simData(SEM.Model, 200, misspec=SEM.Model.Mis, sequential=TRUE, facDist=facDist)
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

SimData <- simData(SEM.Model, 200, misspec=SEM.Model.Mis, sequential=TRUE, facDist=facDist)
 
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

SimData <- simData(CFA.Model.Aux, 200, misspec = CFA.Mis.Model)

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


SimMissing <- simMissing(pmMAR=0.1, cov=7, numImps=5, threshold = 0.5, covAsAux=FALSE)
SimModel <- simModel(CFA.Model, indLab=1:6)

data <- run(SimMissing, data)

out <- run(SimModel, data, simMissing=SimMissing)

Output <- simResult(100, SimData, SimModel, SimMissing)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

########################################## Example 13 ###################
library(simsem)
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
model <- simParamCFA(LY=loading)
SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
out <- run(SimModel, HolzingerSwineford1939)

### Making result object without trivial model misspecification
#output <- runFit(SimModel, HolzingerSwineford1939, 1000)
#pValue(out, output)

u2 <- simUnif(-0.2, 0.2)
loading.mis <- matrix(NA, 9, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
misspec <- simMisspecCFA(LY=LY.mis)
output2 <- runFit(SimModel, HolzingerSwineford1939, 1000, misspec=misspec)
pValue(out, output2)


facCov <- matrix(NA, 3, 3)
diag(facCov) <- 1
errorCov <- diag(NA, 9)
intercept <- rep(NA, 9)
facMean <- rep(0, 3)
model <- simParamCFA(LY=loading, PS=facCov, TE=errorCov, TY=intercept, AL=facMean)

############################### Example 14 #######################

library(simsem)
library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path <- matrix(0, 3, 3)
path[2:3, 1] <- NA
path[3, 2] <- NA
param <- simParamSEM(LY=loading, BE=path)

usedData <- imposeMissing(PoliticalDemocracy, pmMCAR=0.03)

model <- simModel(param, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
miss <- simMissing(numImps=5)
out <- run(model, usedData, miss)

u2 <- simUnif(-0.2, 0.2)
loading.mis <- matrix(NA, 11, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
misspec <- simMisspecSEM(LY=LY.mis)
output <- runFit(model, usedData, 200, misspec=misspec, missModel=miss)
pValue(out, output)

############################# Example 15 ############################################

library(simsem)
u35 <- simUnif(0.1, 0.3)
u57 <- simUnif(0.5, 0.7)
u2 <- simUnif(-0.2, 0.2)

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

loading.mis <- matrix(NA, 7, 3)
loading.mis[is.na(loading)] <- 0
loading.mis[,3] <- 0
loading.mis[7,] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
misspec <- simMisspecSEM(LY=LY.mis)

SimData <- simData(Cov.Model, 200, misspec=misspec)

# First analysis model: Model without covariate
No.Cov.Model <- extract(Cov.Model, y=1:6, e=1:2)
model1 <- simModel(No.Cov.Model, indLab=paste("y", 1:6, sep=""))
Output1 <- simResult(100, SimData, model1)
param <- getPopulation(Output1)
param <- extract(param, y=1:6, e=1:2)
Output1 <- setPopulation(Output1, param) 
summary(Output1)

# Second analysis model: Model accounting for covariate in the indicator level
model2 <- simModel(Cov.Model)
Output2 <- simResult(100, SimData, model2)
summary(Output2)

# Third analysis model: Model accounting for covariate with orthogonalization
ortho <- simFunction(residualCovariate, targetVar=1:6, covVar=7)
model3 <- model1
Output3 <- simResult(100, SimData, model3, objFunction=ortho)
param <- getPopulation(Output3)
param <- extract(param, y=1:6, e=1:2)
Output3 <- setPopulation(Output3, param) 
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
Fac.Cov.Model <- simParamSEM(LY=loading, BE=path, TE=errorCov, PS=facCov)
model4 <- simModel(Fac.Cov.Model)
Output4 <- simResult(100, SimData, model4)

loadingVal <- matrix(0, 7, 3)
loadingVal[1:3, 1] <- 0.6
loadingVal[4:6, 2] <- 0.6
loadingVal[7, 3] <- 1
LY <- simMatrix(loading, loadingVal)
pathVal <- matrix(0, 3, 3)
pathVal[2, 1] <- 0.4
pathVal[1, 3] <- 0.4
pathVal[2, 3] <- 0.4
BE <- simMatrix(path, pathVal)
PS <- symMatrix(facCov)
errorCovVal <- diag(0.64, 7)
errorCovVal[7, 7] <- 0
TE <- symMatrix(errorCov, errorCovVal)
Fac.Cov.Model.Full <- simSetSEM(LY=LY, PS=PS, BE=BE, TE=TE)
Output4 <- setPopulation(Output4, Fac.Cov.Model.Full) 
summary(Output4)

############################# Example 16 ############################################

library(simsem)
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
pathVal[4, 1] <- pathVal[7, 4] <- "u57"
pathVal[5, 2] <- pathVal[8, 5] <- "u57"
pathVal[6, 3] <- pathVal[9, 6] <- "u57"
pathVal[5, 1] <- pathVal[8, 4] <- "u35"
pathVal[6, 2] <- pathVal[9, 5] <- "u35"
BE <- simMatrix(path, pathVal)
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- NA
facCor[1, 3] <- facCor[3, 1] <- NA
facCor[2, 3] <- facCor[3, 2] <- NA
RPS <- symMatrix(facCor, "u35")
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
LY <- simMatrix(loading, "u57")
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
TE <- symMatrix(errorCor, errorCorVal)
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
dat <- run(datModel, dataOnly=FALSE)
SimModel <- simModel(longMed, equalCon=con)
out <- run(SimModel, dat)

LY2 <- matrix(0, 9, 3)
LY2[1:3, 1] <- NA
LY2[4:6, 2] <- NA
LY2[7:9, 3] <- NA
BE2 <- matrix(0, 3, 3)
BE2[2,1] <- NA
BE2[3,2] <- NA
crossMed <- simParamSEM(LY=LY2, BE=BE2)
SimModel2 <- simModel(crossMed, indLab=19:27)
out2 <- run(SimModel2, dat)

output <- simResult(1000, datModel, SimModel)


mis <- seq(0.01, 0.2, 0.01)

wrap <- function(mis) {
loading <- matrix(0, 10, 2)
loading[1:5, 1] <- NA
loading[6:10, 2] <- NA
LY <- simMatrix(loading, 0.7)

facCor <- matrix(NA, 2, 2)
diag(facCor) <- 1
PS <- symMatrix(facCor, 0.5)

TE <- symMatrix(diag(10))

realPop <- simSetCFA(LY=LY, RPS=PS, RTE=TE)

loadingMis <- matrix(0, 10, 2)
loadingMis[1, 2] <- NA
LYmis <- simMatrix(loadingMis, mis)

misPop <- simMisspecCFA(LY=LYmis)

popMisfit(realPop, misPop)
}

out <- sapply(mis, wrap)



