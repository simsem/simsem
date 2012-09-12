library(simsem)

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

Output <- sim(1000, n=200, CFA.Model)

getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
summaryFit(Output)
summaryParam(Output)
summaryParam(Output, detail=TRUE)
