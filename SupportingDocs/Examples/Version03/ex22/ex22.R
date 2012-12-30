library(simsem)

path.null <- matrix(0, 5, 5)
path.null[2, 1] <- NA
path.null[3, 2] <- NA
path.null[4, 3] <- NA
path.null[5, 4] <- NA
path.null.mis <- matrix(0, 5, 5)
path.null.mis[3:5, 1] <- "rnorm(1, 0, 0.05)"
path.null.mis[4:5, 2] <- "rnorm(1, 0, 0.05)"
path.null.mis[5, 3] <- "rnorm(1, 0, 0.05)"
BE.null <- bind(path.null, 0.4, misspec = path.null.mis)

residual <- diag(5)
RPS <- binds(residual)

path.model.null <- model(RPS = RPS, BE = BE.null, modelType = "Path")

path.alt <- matrix(0, 5, 5)
path.alt[2:3, 1] <- NA
path.alt[4, 2:3] <- NA
path.alt[5, 4] <- NA
path.alt.mis <- matrix(0, 5, 5)
path.alt.mis[4:5, 1] <- "rnorm(1, 0, 0.05)"
path.alt.mis[5, 2:3] <- "rnorm(1, 0, 0.05)"
BE.alt <- bind(path.alt, 0.4, misspec = path.alt.mis)

path.model.alt <- model(RPS = RPS, BE = BE.alt, modelType="Path")

Output.NULL <- sim(NULL, n = 25:500, path.model.null, pmMCAR = seq(0, 0.3, 0.1))
Output.ALT <- sim(NULL, n = 25:500, path.model.null, generate = path.model.alt, pmMCAR = seq(0, 0.3, 0.1))

cutoff <- getCutoff(Output.NULL, alpha = 0.05, nVal = 250, pmMCARval = 0.2)
plotCutoff(Output.NULL, alpha = 0.05)
getPowerFit(Output.ALT, nullObject = Output.NULL, alpha = 0.05, nVal = 250, pmMCARval = 0.2)
getPowerFit(Output.ALT, cutoff = cutoff, nVal = 250, pmMCARval = 0.2, condCutoff = TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha = 0.05)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff = cutoff2, nVal = 250, pmMCARval = 0.2, condCutoff = FALSE)
plotPowerFit(Output.ALT, cutoff = cutoff2)
