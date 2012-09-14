library(simsem)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6), misspec=matrix("rnorm(1,0,0.1)", 6, 6))

CFA.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")

Output.NULL <- sim(1000, n=500, CFA.NULL)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
loading.alt.mis <- matrix("runif(1,-.2,.2)", 6, 2)
loading.alt.mis[is.na(loading.alt)] <- 0
LY.ALT <- bind(loading.alt, 0.7, misspec=loading.alt.mis)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, "runif(1,0.7,0.9)")
CFA.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")

Output.ALT <- sim(1000, n=500, model=CFA.NULL, generate=CFA.ALT)

cutoff <- getCutoff(Output.NULL, 0.05)
getPowerFit(Output.ALT, cutoff)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "SRMR", "CFI"))

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff2)
plotPowerFit(Output.ALT, cutoff=cutoff2)
plotPowerFit(Output.ALT, cutoff=cutoff2, usedFit=c("RMSEA", "SRMR", "CFI"))

plotPowerFit(Output.ALT, Output.NULL, cutoff=cutoff2, usedFit=c("RMSEA", "SRMR", "CFI"))
