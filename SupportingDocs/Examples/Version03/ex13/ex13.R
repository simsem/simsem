library(simsem)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1, 0.5, 0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1, -0.1, 0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")

loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA
mis.loading <- matrix("runif(1, -0.3, 0.3)", 12, 4)
mis.loading[is.na(loading)] <- 0
LY <- bind(loading, "runif(1, 0.7, 0.9)", misspec=mis.loading)

mis.error.cor <- matrix("rnorm(1, 0, 0.1)", 12, 12)
diag(mis.error.cor) <- 0
RTE <- binds(diag(12), misspec=mis.error.cor)

SEM.Model <- model(RPS = RPS, BE = BE, LY=LY, RTE=RTE, modelType="SEM")

n1 <- list(mean = 0, sd = 0.1)
chi5 <- list(df = 5)

facDist <- bindDist(c("chisq", "chisq", "norm", "norm"), chi5, chi5, n1, n1)

dat <- generate(SEM.Model, n=500, sequential=TRUE, facDist=facDist)
out <- analyze(SEM.Model, dat, estimator="mlr")

simOut <- sim(1000, n=500, SEM.Model, sequential=TRUE, facDist=facDist, estimator="mlr")
getCutoff(simOut, 0.05)
plotCutoff(simOut, 0.05)
summary(simOut)
