library(simsem)

LY <- matrix(1, 4, 2)
LY[,2] <- 0:3

PS <- matrix(NA, 2, 2)

TY <- rep(0, 4)

AL <- rep(NA, 2)

TE <- diag(NA, 4)

linearModel <- estmodel(LY=LY, PS=PS, TY=TY, AL=AL, TE=TE, modelType="CFA", indLab=paste("t", 1:4, sep=""))

LY2 <- matrix(1, 4, 2)
LY2[,2] <- c(0, NA, NA, 3)

unconstrainModel <- estmodel(LY=LY2, PS=PS, TY=TY, AL=AL, TE=TE, modelType="CFA", indLab=paste("t", 1:4, sep=""))

outNested <- analyze(linearModel, Demo.growth)
outParent <- analyze(unconstrainModel, Demo.growth)

loadingMis <- matrix(0, 4, 2)
loadingMis[2:3, 2] <- "runif(1, -0.1, 0.1)"

nested <- model.lavaan(outNested, LY = loadingMis)
parent <- model.lavaan(outParent)

samplesize <- nrow(Demo.growth)

simNestedNested <- sim(1000, n = samplesize, model=nested, generate=nested)
simNestedParent <- sim(1000, n = samplesize, model=parent, generate=nested)
simParentNested <- sim(1000, n = samplesize, model=nested, generate=parent)
simParentParent <- sim(1000, n = samplesize, model=parent, generate=parent)

pValueNested(outNested, outParent, simNestedNested, simNestedParent)
getPowerFitNested(simParentNested, simParentParent, nullNested=simNestedNested, nullParent=simNestedParent)
