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

#install.packages("C:/Users/Sunthud/Desktop/My Dropbox/simsem/simsem_0.0-6.tar.gz", repos=NULL, type="source")
#install.packages("C:/Users/student/Dropbox/simsem/simsem_0.0-6.tar.gz", repos=NULL, type="source")

sourceDir <- function(path, trace = TRUE, ...) {
     for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
        if(trace) cat(nm,":")           
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
     }
}

#path <- "C:/Users/Sunthud/Desktop/My Dropbox/simsem/simsem/R/"
#path <- "C:/Users/Sunthud/simsem_backup/simsem/R/"
path <- "C:/Users/student/Dropbox/simsem/simsem/R/"
 source(paste(path, "AllClass.R", sep=""))
 source(paste(path, "AllGenerics.R", sep=""))
 sourceDir(path)

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)

indicator.mean <- rep(NA, 6)
MX <- simVector(indicator.mean, 0)
indicator.var <- rep(NA, 6)
VX <- simVector(indicator.var, 1)

CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD) #, VX = VX, MX=MX)

SimData <- simData(200, CFA.Model)

SimModel <- simModel(CFA.Model)

SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
Output <- simResult(200, SimData, SimModel)
#Output <- simResult(100, SimData, SimModel, SimMissing, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

#################################### Example 2 #######################

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
PS <- symMatrix(factor.cor, 0.5)

VTE <- simVector(rep(NA, 4), 1.2)

TE <- symMatrix(diag(4))

TY <- simVector(rep(0, 4))

LCA.Model <- simSetCFA(LY=LY, PS=PS, VPS=VPS, AL=AL, VTE=VTE, TE=TE, TY=TY)

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

Data.Mis <- simData(300, LCA.Model, LCA.Mis)

Output.Mis <- simResult(100, Data.Mis, SimModel, multicore=TRUE)
getCutoff(Output.Mis, 0.05)
plotCutoff(Output.Mis, 0.05)
summaryParam(Output.Mis)

################################# Example 3 ##################################
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
PS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(PS = PS, BE = BE, ME = ME)

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

# Example 3 with X sides #
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
PH <- symMatrix(exo.cor, "n31")

PS <- symMatrix(diag(2))

Path.Model <- simSetPath(PS = PS, BE = BE, PH = PH, GA = GA, exo=TRUE)

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

############# Example 4 ################################
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

TE <- symMatrix(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
PS <- symMatrix(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "n65"
path.start[3, 2] <- "u35"
BE <- simMatrix(path, path.start)

SEM.model <- simSetSEM(BE=BE, LY=LY, PS=PS, TE=TE)

loading.trivial <- matrix(NA, 8, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "u2")

error.cor.trivial <- matrix(NA, 8, 8)
diag(error.cor.trivial) <- 0
TE.trivial <- symMatrix(error.cor.trivial, "n1")

SEM.Mis.Model <- simMisspecSEM(LY = LY.trivial, TE = TE.trivial)

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

# Example 4 Extension: Kernel Regression
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

# Example 4 with X side and stringent constraints #
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

TD <- symMatrix(diag(6))

TE <- symMatrix(diag(2))

factor.K.cor <- matrix(NA, 2, 2)
diag(factor.K.cor) <- 1
PH <- symMatrix(factor.K.cor, 0.5)

PS <- symMatrix(as.matrix(1))

path.GA <- matrix(NA, 1, 2)
path.GA.start <- matrix(c("n65", "u35"), ncol=2)
GA <- simMatrix(path.GA, path.GA.start)

BE <- simMatrix(as.matrix(0))

SEM.model <- simSetSEM(GA=GA, BE=BE, LX=LX, LY=LY, PH=PH, PS=PS, TD=TD, TE=TE, exo=TRUE)

loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X)] <- 0
LX.trivial <- simMatrix(loading.X.trivial, "u2")

error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
TD.trivial <- symMatrix(error.cor.X.trivial, "n1")

error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
TE.trivial <- symMatrix(error.cor.Y.trivial, "n1")

TH.trivial <- simMatrix(matrix(NA, 6, 2), "n1")

SEM.Mis.Model <- simMisspecSEM(LX = LX.trivial, TE = TE.trivial, TD = TD.trivial, TH = 
TH.trivial, exo=TRUE)

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

################### Example 5 ##################################

library(simsem)

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
PH.NULL <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, PS = PH.NULL, TE = TD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
TD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.NULL.Mis <- simMisspecCFA(TE = TD.Mis)

SimData.NULL <- simData(500, CFA.Model.NULL, misspec = CFA.Model.NULL.Mis)
SimModel <- simModel(CFA.Model.NULL)
Output.NULL <- simResult(300, SimData.NULL, SimModel)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
PH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, PS = PH.ALT, TE = TD)

loading.alt.mis <- matrix(NA, 6, 2)
loading.alt.mis[is.na(loading.alt)] <- 0
LX.alt.mis <- simMatrix(loading.alt.mis, "u2")
CFA.Model.alt.mis <- simMisspecCFA(LY = LX.alt.mis, TE=TD.Mis)

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

################### Example 5 Extenstion: Kernel Regression ########################
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


################################## Example 6 ##########################################
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
PS <- symMatrix(faccor, faccor.v)

TE <- symMatrix(diag(9))

mtmm.model <- simSetCFA(LY=LY, PS=PS, TE=TE)

error.cor.mis <- matrix(NA, 9, 9)
diag(error.cor.mis) <- 1
TE.mis <- symMatrix(error.cor.mis, "n1")
loading.mis <- matrix(NA, 9, 4)
loading.mis[is.na(loading)] <- 0
loading.mis[,4] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
mtmm.model.mis <- simMisspecCFA(TE = TE.mis, LY=LY.mis)

SimMissing <- simMissing(pmMCAR=0.2, numImps=5)

SimData <- simData(500, mtmm.model, misspec = mtmm.model.mis)
SimModel <- simModel(mtmm.model)

Output <- simResult(10, SimData, SimModel, SimMissing)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

################################## Example 7 ##########################################
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
PS <- symMatrix(faccor, "u16")

TE <- symMatrix(diag(48))

CFA.model <- simSetCFA(LY=LY, PS=PS, TE=TE)

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

SimData <- simData(1000, CFA.model, misspec = CFA.model.mis)
SimModel <- simModel(CFA.model)
Output <- simResult(20, SimData, SimModel, SimMissing, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
