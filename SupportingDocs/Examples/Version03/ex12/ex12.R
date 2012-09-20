library(simsem)

loading <- matrix(0, 12, 3)
loading[1:4, 1] <- NA
loading[5:8, 2] <- NA
loading[9:12, 3] <- NA
loading.mis <- matrix("runif(1, -0.2, 0.2)", 12, 3)
loading.mis[is.na(loading)] <- 0
LY <- bind(loading, 0.7, misspec=loading.mis)

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, "runif(1, -0.5, 0.5)")

error.cor <- matrix(0, 12, 12)
diag(error.cor) <- 1
RTE <- binds(error.cor)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA") 

distname <- c(rep("t", 4), rep("chisq", 8))

d1 <- list(df=2)
d2 <- list(df=3)
d3 <- list(df=4)
d4 <- list(df=5)
d5 <- list(df=3)
d6 <- list(df=4)
d7 <- list(df=5)
d8 <- list(df=6)
d9 <- list(df=3)
d10 <- list(df=4)
d11 <- list(df=5)
d12 <- list(df=6)

dist <- bindDist(distname, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, reverse=c(rep(FALSE, 8), rep(TRUE, 4)))

dat <- generate(CFA.Model, n=200, indDist=dist)
out <- analyze(CFA.Model, dat, estimator="mlm")

Output <- sim(1000, n=200, CFA.Model, indDist=dist, estimator="mlm")
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
