library(simsem)

loading <- matrix(0, 9, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[c(1, 4, 7), 4] <- NA
loading.v <- matrix(0, 9, 4)
loading.v[1:3, 1] <- "runif(1,.4,.9)"
loading.v[4:6, 2] <- "runif(1,.4,.9)"
loading.v[7:9, 3] <- "runif(1,.4,.9)"
loading.v[c(1, 4, 7), 4] <- "runif(1,.3,.6)"
loading.mis <- matrix("runif(1,-.2,.2)", 9, 4)
loading.mis[is.na(loading)] <- 0
loading.mis[,4] <- 0
LY <- bind(loading, loading.v, misspec=loading.mis)

faccor <- diag(4)
faccor[1, 2] <- faccor[2, 1] <- NA
faccor[1, 3] <- faccor[3, 1] <- NA
faccor[2, 3] <- faccor[3, 2] <- NA
faccor.v <- diag(4)
faccor.v[1, 2] <- faccor.v[2, 1] <- "rnorm(1,.4,.1)"
faccor.v[1, 3] <- faccor.v[3, 1] <- "rnorm(1,.2,.1)"
faccor.v[2, 3] <- faccor.v[3, 2] <- "rnorm(1,.3,.1)"
RPS <- binds(faccor, faccor.v)

error.cor.mis <- matrix("rnorm(1,0,.1)", 9, 9)
diag(error.cor.mis) <- 1
RTE <- binds(diag(9), misspec=error.cor.mis)

mtmm.model <- model(LY=LY, RPS=RPS, RTE=RTE, modelType="CFA")

miss.model <- miss(pmMCAR=0.2, ignoreCols="group", m=5) 

dat <- generate(mtmm.model, 500)
dat <- impose(miss.model, dat)
out <- analyze(mtmm.model, dat, miss=miss.model)

Output <- sim(1000, n=500, mtmm.model, miss=miss.model)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
