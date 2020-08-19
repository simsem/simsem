#library(devtools)
#load_all("../../../simsem")

library(simsem)
library(testthat)


# context("Examples")

# context("CFA")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(NA,6),1)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

Output <- sim(10, CFA.Model,n=300)


loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:5, 2] <- "a1"
loading[6,2] <- "a2"
lmis <- matrix("",6,2)
lmis[4:5,2] <- "runif(1,.01,.02)"
LY <- bind(loading, "runif(1,.6,.8)",lmis)

loading[1,1] <- "a3"
LY2 <- bind(loading, "runif(1,.6,.8)",lmis)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- bind(latent.cor, 0.5,symmetric=TRUE)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- NA
RTE <- bind(error.cor,popParam=1,"runif(1,0.01,0.015)",symmetric=TRUE)

tcfa <- model(LY=LY,RPS=RPS,RTE=RTE, modelType="CFA")
tcfamg <- model(LY=c(LY,LY2),RPS=RPS,RTE=RTE, modelType="CFA")
tcfamg2 <- model(LY=list(LY,LY),RPS=list(RPS,RPS),RTE=RTE, modelType="CFA")

out <- sim(10,tcfa,200)
out <- sim(10,tcfamg,200)
out <- sim(10,tcfamg2,200)

expect_is(out,"SimResult")

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

tcfa2 <- model(LY=LY,PS=PS,TE=TE,AL=AL,TY=TY, modelType="CFA")

Output <- sim(20, tcfa, n=300)
## getCutoff(Output, 0.05)
## plotCutoff(Output, 0.05)
## summaryParam(Output)

expect_is(Output,"SimResult")


