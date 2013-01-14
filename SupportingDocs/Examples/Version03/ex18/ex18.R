library(simsem)
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTE <- binds(error.cor)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

Output <- sim(NULL, n=50:1000, CFA.Model)
summary(Output)
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200)
findPower(Cpow, "N", 0.80)
plotPower(Output, powerParam=c("f1=~y1", "f1~~f2"))
