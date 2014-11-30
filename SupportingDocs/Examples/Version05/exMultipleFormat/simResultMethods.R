library(simsem)

loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading.start <- matrix("", 9, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:9, 3] <- "rnorm(1,0.6,0.05)"
loading.trivial <- matrix("runif(1, -0.2, 0.2)", 9, 3)
loading.trivial[is.na(loading)] <- 0
LY <- bind(loading, loading.start, misspec=loading.trivial)

error.cor.trivial <- matrix("rnorm(1, 0, 0.1)", 9, 9)
diag(error.cor.trivial) <- 1
RTE <- binds(diag(9), misspec=error.cor.trivial)

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- binds(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"
BE <- bind(path, path.start)

SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

dat <- generate(SEM.model, n=300)
out <- analyze(SEM.model, dat)

outfun <- function(out) {
	inspect(out, "rsquare")
}

Output <- sim(20, n=300, SEM.model, silent=TRUE, pmMCAR = 0.1, outfun = outfun) 

inspect(Output, "modeltype")
inspect(Output, "nrep")
inspect(Output, "param")
inspect(Output, "coef")
inspect(Output, "se")
inspect(Output, "fit")
inspect(Output, "misspec")
inspect(Output, "popfit")
inspect(Output, "fmi1")
inspect(Output, "fmi2")
inspect(Output, "cilower")
inspect(Output, "ciupper")
inspect(Output, "ciwidth")
inspect(Output, "seed")
inspect(Output, "ngroup")
inspect(Output, "ntotal")
inspect(Output, "mcar")
inspect(Output, "mar")
inspect(Output, "extra")
inspect(Output, "time")
inspect(Output, "converged")
summaryParam(Output, detail = TRUE)
summaryParam(Output, std = TRUE, detail = TRUE)
summaryMisspec(Output)
summaryPopulation(Output)
summaryFit(Output)
summaryConverge(Output)
getPopulation(Output)
getExtraOutput(Output)
coef(Output)
summaryTime(Output)
summarySeed(Output)

Output2 <- sim(NULL, n=50:500, SEM.model, silent=TRUE, pmMCAR = 0.1, outfun = outfun) 

inspect(Output2, "modeltype")
inspect(Output2, "nrep")
inspect(Output2, "param")
inspect(Output2, "coef")
inspect(Output2, "se")
inspect(Output2, "fit")
inspect(Output2, "misspec")
inspect(Output2, "popfit")
inspect(Output2, "fmi1")
inspect(Output2, "fmi2")
inspect(Output2, "cilower")
inspect(Output2, "ciupper")
inspect(Output2, "ciwidth")
inspect(Output2, "seed")
inspect(Output2, "ngroup")
inspect(Output2, "ntotal")
inspect(Output2, "mcar")
inspect(Output2, "mar")
inspect(Output2, "extra")
inspect(Output2, "time")
inspect(Output2, "converged")
summaryParam(Output2, detail = TRUE)
summaryParam(Output2, std = TRUE, detail = TRUE)
summaryMisspec(Output2)
summaryPopulation(Output2)
summaryFit(Output2)
summaryConverge(Output2)
getPopulation(Output2)
getExtraOutput(Output2)
coef(Output2)
summaryTime(Output2)
summarySeed(Output2)
