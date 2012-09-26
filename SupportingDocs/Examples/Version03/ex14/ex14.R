library(simsem)

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
diag(errorCorMis) <- 1
RTE <- binds(diag(5), misspec=errorCorMis)

VY <- bind(c(NA, NA, NA, 0, 0), 1)

SEM.Model <- model(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY, modelType="SEM")

dist <- c("norm", "chisq", "norm")
n01 <- list(mean=0, sd=1)
c5 <- list(df=5)
facDist <- bindDist(dist, n01, c5, n01)

Output <- sim(1000, n=200, SEM.Model, sequential=TRUE, facDist=facDist, estimator="mlm")
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
