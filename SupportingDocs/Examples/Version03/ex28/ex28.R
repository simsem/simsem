library(simsem)

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

modelA <- model(LY=LY, TY=TY, RTE=RTE, VTE=VTE, AL=AL, VPS=VPS, RPS=RPSA, modelType="CFA")
modelB <- model(LY=LY, TY=TY, RTE=RTE, VTE=VTE, AL=AL, VPS=VPS, RPS=RPSB, modelType="CFA")

outAA <- sim(10, n = 200, model = modelA, generate = modelA)
outAB <- sim(10, n = 200, model = modelB, generate = modelA)
outBA <- sim(10, n = 200, model = modelA, generate = modelB)
outBB <- sim(10, n = 200, model = modelB, generate = modelB)

getCutoffNonNested(outAA, outAB, outBA, outBB)
getCutoffNonNested(outAA, outAB)
getCutoffNonNested(outBB, outBA)
getCutoffNonNested(outAA, outAB, outBA, outBB, onetailed=TRUE)
plotCutoffNonNested(outAA, outAB, outBA, outBB, alpha=0.05)
plotCutoffNonNested(outAA, outAB, outBA, outBB, alpha=0.05, onetailed=TRUE)

getPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB, usedFit="AIC")

cutoff <- c(AIC=0, BIC=0)
cutoff2 <- c(AIC=2, BIC=2)
getPowerFitNonNested(outBA, outBB, cutoff=cutoff)
getPowerFitNonNested(outBA, outBB, cutoff=cutoff2)
plotPowerFitNonNested(outBA, outBB, cutoff=cutoff2)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB, cutoff=cutoff2)
