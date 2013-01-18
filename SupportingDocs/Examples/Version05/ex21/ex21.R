library(simsem)

loading.null <- matrix(0, 8, 2)
loading.null[1:5, 1] <- NA
loading.null[6:8, 2] <- NA
LY.NULL <- bind(loading.null, 0.7)
latent.cor.null <- matrix(NA, 2, 2)
diag(latent.cor.null) <- 1
RPS <- binds(latent.cor.null, 0.5)
error.cor.mis <- matrix("rnorm(1, 0, 0.1)", 8, 8)
diag(error.cor.mis) <- 1
RTE <- binds(diag(8), misspec = error.cor.mis)
CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS, RTE = RTE, modelType = "CFA")

loading.alt <- matrix(0, 8, 2)
loading.alt[1:4, 1] <- NA
loading.alt[5:8, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS, RTE = RTE, modelType="CFA")

Output.NULL <- sim(NULL, n = 25:500, CFA.Model.NULL)
Output.ALT <- sim(NULL, n = 25:500, CFA.Model.NULL, generate = CFA.Model.ALT)

cutoff <- getCutoff(Output.NULL, alpha = 0.05, nVal = 250)
plotCutoff(Output.NULL, alpha = 0.05)
getPowerFit(Output.ALT, nullObject = Output.NULL, alpha = 0.05, nVal = 250)
getPowerFit(Output.ALT, cutoff = cutoff, nVal = 250, condCutoff = TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha = 0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha = 0.05, logistic = FALSE)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff = cutoff2, nVal = 250, condCutoff = FALSE)
plotPowerFit(Output.ALT, cutoff = cutoff2)
plotPowerFit(Output.ALT, cutoff = cutoff2, logistic = FALSE)
