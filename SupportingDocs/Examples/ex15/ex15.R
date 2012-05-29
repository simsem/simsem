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
Output1 <- simResult(1000, SimData, model1)
param <- getPopulation(Output1)
param <- extract(param, y=1:6, e=1:2)
Output1 <- setPopulation(Output1, param) 
summary(Output1)

# Second analysis model: Model accounting for covariate in the indicator level
model2 <- simModel(Cov.Model)
Output2 <- simResult(1000, SimData, model2)
summary(Output2)

# Third analysis model: Model accounting for covariate with orthogonalization
ortho <- simFunction(residualCovariate, targetVar=1:6, covVar=7)
model3 <- model1
Output3 <- simResult(1000, SimData, model3, objFunction=ortho)
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
Output4 <- simResult(1000, SimData, model4)

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
