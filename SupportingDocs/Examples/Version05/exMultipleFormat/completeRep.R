library(simsem)

loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- "con1"
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
loading.trivial <- matrix("runif(1, -0.2, 0.2)", 8, 3)
loading.trivial[is.na(loading)] <- 0
LY <- bind(loading, loading.start, misspec=loading.trivial)

error.cor.trivial <- matrix("rnorm(1, 0, 0.1)", 8, 8)
diag(error.cor.trivial) <- 1
RTE <- binds(diag(8), misspec=error.cor.trivial)

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

# The number of convergent replications is at least 10

Output <- sim(10, n=300, SEM.model, completeRep = TRUE) 
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

# The number of convergent replications is at least 50

Output2 <- sim(50, n=300, SEM.model, previousSim = Output, completeRep = TRUE)
getCutoff(Output2, 0.05)
plotCutoff(Output2, 0.05)
summary(Output2)

# Adding ten more replications

Output3 <- sim(10, n=300, SEM.model, previousSim = Output2) 
getCutoff(Output3, 0.05)
plotCutoff(Output3, 0.05)
summary(Output3)
