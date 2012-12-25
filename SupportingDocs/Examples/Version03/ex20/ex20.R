library(simsem)

loading <- matrix(0, 10, 2)
loading[1:6, 1] <- NA
loading[7:10, 2] <- NA
LY <- bind(loading, "runif(1, 0.3, 0.9)")

RPS <- binds(diag(2))

RTE <- binds(diag(10))

path <- matrix(0, 2, 2)
path[2, 1] <- NA
BE <- bind(path, "runif(1, 0, 0.9)")

latentReg <- model(LY = LY, RPS = RPS, RTE = RTE, BE = BE, modelType = "SEM")

Output <- sim(NULL, n=25:500, latentReg)
summary(Output)
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, n = 200)	

Cpow <- getPower(Output, contParam = "1.f2~f1")
Cpow2 <- getPower(Output, contParam = "1.f2~f1", nVal = 200, paramVal = seq(0.1, 0.9, 0.1))

targetVal <- list("1.f2~f1" = seq(0.1, 0.9, 0.1), "1.f1=~y1" = c(0.5, 0.7))
Cpow3 <- getPower(Output, contParam = c("1.f2~f1", "1.f1=~y1"), nVal = 200, paramVal = targetVal)

findPower(Cpow, 1, 0.80)
findPower(Cpow, 2, 0.80)

plotPower(Output, powerParam = c("1.f2~f1", "1.f2=~y10"), contParam = "1.f2~f1")
