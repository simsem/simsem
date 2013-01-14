library(simsem)

loading <- matrix(0, 5, 3)
loading[1,1] <- 1
loading[2:5,2] <- 1
loading[2:5,3] <- 0:3
loading.trivial <- matrix(0, 5, 3)
loading.trivial[3:4, 3] <- "runif(1, -0.1, 0.1)"
LY <- bind(loading, misspec=loading.trivial)

facMean <- rep(NA, 3)
facMeanVal <- c(0.5, 5, 2)
AL <- bind(facMean, facMeanVal)

facVar <- rep(NA, 3)
facVarVal <- c(0.25, 1, 0.25)
VPS <- bind(facVar, facVarVal)

facCor <- diag(3)
facCor[2,3] <- NA
facCor[3,2] <- NA
RPS <- binds(facCor, 0.5)

VTE <- bind(c(0, rep(NA, 4)), 1.2)

RTE <- binds(diag(5))

TY <- bind(rep(0, 5))

path <- matrix(0, 3, 3)
path[2,1] <- NA
path[3,1] <- NA
pathVal <- matrix(0, 3, 3)
pathVal[2,1] <- 0.5
pathVal[3,1] <- 0.1
BE <- bind(path, pathVal)

LCA.Model <- model(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY, BE=BE, modelType="SEM")

group <- list(size=1, prob=0.5)
n01 <- list(mean=0, sd=1)
facDist <- bindDist(c("binom", "norm", "norm"), group, n01, n01, keepScale=c(FALSE, TRUE, TRUE))

Output <- sim(NULL, n=50:500, LCA.Model, pmMCAR=seq(0, 0.4, 0.1), sequential=TRUE, facDist=facDist)

plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200, pmMCARval = 0)
getCutoff(Output, 0.05, nVal = 300, pmMCARval = 0.33)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200, pmMCARval = 0.35)
findPower(Cpow, "N", 0.80)
findPower(Cpow, "MCAR", 0.80)
plotPower(Output, powerParam=c("f2~f1", "f3~f1"))
