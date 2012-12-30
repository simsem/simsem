library(simsem)
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
targetmodel <- estmodel(LY = loading, modelType = "CFA", indLab = paste("x", 1:9, sep=""))
out <- analyze(targetmodel, HolzingerSwineford1939)

samplesize <- nrow(HolzingerSwineford1939)

template1 <- model.lavaan(out, std = TRUE)
simOut1 <- sim(1000, n = samplesize, template1)
getCutoff(simOut1, alpha = 0.05)
pValue(out, simOut1)

loadingMis2 <- matrix(0, 9, 3)
loadingMis2[1,2] <- 0.3
loadingMis2[4,3] <- 0.3
template2 <- model.lavaan(out, LY = loadingMis2, std = TRUE)
simOut2 <- sim(1000, n = samplesize, template2, createOrder = c(1, 3, 2))
getCutoff(simOut2, alpha = 0.05)
pValue(out, simOut2)

loadingMis3 <- matrix(0, 9, 3)
loadingMis3[6,1] <- 0.3
loadingMis3[9,2] <- 0.3
template3 <- model.lavaan(out, LY = loadingMis3, std = TRUE)
simOut3 <- sim(1000, n = samplesize, template3, createOrder = c(1, 3, 2))
getCutoff(simOut3, alpha = 0.05)
pValue(out, simOut3)

loadingMis4 <- matrix(0, 9, 3)
loadingMis4[4:9, 1] <- "runif(1, -0.3, 0.3)"
loadingMis4[c(1:3, 7:9),2] <- "runif(1, -0.3, 0.3)"
loadingMis4[1:6,3] <- "runif(1, -0.3, 0.3)"
template4 <- model.lavaan(out, LY = loadingMis4, std = TRUE)
simOut4 <- sim(1000, n = samplesize, template4, createOrder = c(1, 3, 2))
getCutoff(simOut4, alpha = 0.05)
pValue(out, simOut4)

loadingMis5 <- matrix(0, 9, 3)
loadingMis5[4:9, 1] <- "rnorm(1, 0, 0.15)"
loadingMis5[c(1:3, 7:9),2] <- "rnorm(1, 0, 0.15)"
loadingMis5[1:6,3] <- "rnorm(1, 0, 0.15)"
template5 <- model.lavaan(out, LY = loadingMis5, std = TRUE)
simOut5 <- sim(1000, n = samplesize, template5, createOrder = c(1, 3, 2))
getCutoff(simOut5, alpha = 0.05)
pValue(out, simOut5)

loadingMis6 <- matrix(0, 9, 3)
loadingMis6[4:9, 1] <- "runif(1, -0.3, 0.3)"
loadingMis6[c(1:3, 7:9),2] <- "runif(1, -0.3, 0.3)"
loadingMis6[1:6,3] <- "runif(1, -0.3, 0.3)"
template6 <- model.lavaan(out, LY = loadingMis6, std = TRUE)
simOut6 <- sim(1000, n = samplesize, template6, createOrder = c(1, 3, 2), optMisfit = "max", optDraws = 100)
getCutoff(simOut6, alpha = 0.05)
pValue(out, simOut6)

loadingMis7 <- matrix(0, 9, 3)
loadingMis7[4:9, 1] <- "runif(1, -0.1, 0.1)"
loadingMis7[c(1:3, 7:9),2] <- "runif(1, -0.1, 0.1)"
loadingMis7[1:6,3] <- "runif(1, -0.1, 0.1)"
template7 <- model.lavaan(out, LY = loadingMis7, std = TRUE)
simOut7 <- sim(1000, n = samplesize, template7, createOrder = c(1, 3, 2), misfitType = "rmsea", misfitBounds = c(0.02, 0.05), maxDraw = 200)
getCutoff(simOut7, alpha = 0.05)
pValue(out, simOut7)

loadingMisAlt <- matrix(0, 9, 3)
loadingMisAlt[4, 1] <- "runif(1, 0.6, 0.9)"
loadingMisAlt[7, 2] <- "runif(1, 0.6, 0.9)"
loadingMisAlt[1, 3] <- "runif(1, 0.6, 0.9)"
templateAlt <- model.lavaan(out, LY = loadingMisAlt, std = TRUE)
simOutAlt <- sim(1000, n = samplesize, templateAlt, createOrder = c(1, 3, 2), optMisfit = "min", optDraws = 100)
getCutoff(simOutAlt, alpha = 0.05)
pValue(out, simOutAlt)

getPowerFit(simOutAlt, nullObject = simOut1) 
getPowerFit(simOutAlt, nullObject = simOut2) 
getPowerFit(simOutAlt, nullObject = simOut3) 
getPowerFit(simOutAlt, nullObject = simOut4) 
getPowerFit(simOutAlt, nullObject = simOut5) 
getPowerFit(simOutAlt, nullObject = simOut6) 
getPowerFit(simOutAlt, nullObject = simOut7) 

summaryPopulation(simOut4)
summaryMisspec(simOut4)
summaryFit(simOut4)
plotMisfit(simOut4, misParam = "1.f1=~x9")
