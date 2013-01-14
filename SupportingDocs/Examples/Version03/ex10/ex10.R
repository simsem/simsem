library(simsem)

loading <- matrix(0, 7, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
mis.loading <- matrix(0, 7, 2)
mis.loading[1:3, 2] <- "runif(1, -0.2, 0.2)"
mis.loading[4:6, 1] <- "runif(1, -0.2, 0.2)"
LY <- bind(loading, "runif(1, 0.5, 0.7)", misspec=mis.loading)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, "runif(1, 0.3, 0.5)")

error.cor <- diag(7)
error.cor[1:6, 7] <- NA
error.cor[7, 1:6] <- NA
RTE <- binds(error.cor, "runif(1, -0.4, 0.4)")

VX <- bind(rep(NA, 7), 1)

CFA.Model.Aux <- model(LY = LY, RPS = RPS, RTE = RTE, VY = VX, modelType="CFA") 

missmodel <- miss(pmMAR=0.1, cov="y7", ignoreCols=8, threshold = 0.5)

loading2 <- matrix(0, 6, 2)
loading2[1:3, 1] <- NA
loading2[4:6, 2] <- NA

latent.cor2 <- matrix(NA, 2, 2)
diag(latent.cor2) <- 1

error.cor2 <- diag(NA, 6)

CFA.Model <- estmodel(LY = loading2, PS = latent.cor2, TE = error.cor2, modelType="CFA", indLab=paste0("y", 1:6))


dat <- generate(CFA.Model.Aux, n = 200)
dat <- impose(missmodel, dat)
out <- analyze(CFA.Model, dat, aux="y7")

Output <- sim(1000, n=200, model=CFA.Model, generate=CFA.Model.Aux, miss=missmodel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
