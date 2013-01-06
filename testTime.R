install.packages("simsem_0.5-0.tar.gz", repos=NULL, type="source")
install.packages("simsem_0.4-6.tar.gz", repos=NULL, type="source")
library(simsem)

test1 <- function() {

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, VTE=VTE, modelType = "CFA", indLab=c("pos1", "pos2", "pos3", "neg1", "neg2", "neg3"), facLab=c("posaffect", "negaffect"))

#SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
Output <- sim(1000, CFA.Model,n=200)

}

test2 <- function() {
path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1,0.5,0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1,-0.1,0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1,0.3,0.1)")

ME <- bind(rep(NA, 4), 0)

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType="Path")

param <- draw(Path.Model, misfitBound = c(0.10, 0.11), misfitType="rmsea")
dat <- createData(param[[1]], n = 200)


dat <- generate(Path.Model, n=500, params=TRUE)


out <- analyze(Path.Model, dat)
# The VTE is still wrong. Check after the LCA comes in.


# Output is wrong. It contains some bugs.
Output <- sim(1000, n=500, Path.Model)
}

test3 <- function() {
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

dat <- generate(SEM.model, n=300)
out <- analyze(SEM.model, dat)

Output <- sim(1000, n=300, SEM.model, silent=TRUE) 
}

test4 <- function() {
loading1 <- matrix(NA, 6, 1)
LY1 <- bind(loading1, 0.7)
loading2 <- matrix(0, 6, 2)
loading2[1:3, 1] <- NA
loading2[4:6, 2] <- NA
LY2 <- bind(loading2, 0.7)

latent.cor2 <- matrix(NA, 2, 2)
diag(latent.cor2) <- 1
RPS1 <- binds(as.matrix(1))
RPS2 <- binds(latent.cor2, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

noninvariance <- model(LY = list(LY1, LY2), RPS = list(RPS1, RPS2), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

Output <- sim(1000, noninvariance, n=200) # Need to be fixed

# Configural Invariance
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

configural <- model(LY = list(LY, LY), RPS = list(RPS, RPS), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

dat <- generate(configural, 200)
out <- analyze(configural, dat)

Output <- sim(1000, configural, n=200) # 

# Weak Invariance
loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- c("load1", "load2", "load3")
loading.in[4:6, 2] <- c("load4", "load5", "load6")
mis <- matrix(0,6,2)
mis[loading.in == "0"] <- "runif(1, -0.1, 0.1)"
LY.in <- bind(loading.in, "runif(1, 0.7, 0.8)", mis)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

VPS1 <- bind(rep(1, 2))
VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))

weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA")

Output <- sim(1000, weak, n=200) 

# Strong Invariance
loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- paste0("load", 1:3)
loading.in[4:6, 2] <- paste0("load", 4:6)
LY.in <- bind(loading.in, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

TY.in <- bind(paste0("int", 1:6), 0)

VPS1 <- bind(rep(1, 2))
VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))

AL1 <- bind(rep(0, 2))
AL2 <- bind(rep(NA, 2), c(-0.5, 0.2))

strong <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, TY=TY.in, AL=list(AL1, AL2), ngroups=2, modelType = "CFA")

#SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
Output <- sim(1000, strong, n=200)
}

test5 <- function() {
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
miss.model <- miss(pmMCAR=0.2, ignoreCols="group") #, package="mice")
Output <- sim(30, n=500, mtmm.model, miss=miss.model)
}

system.time(test1())
system.time(test2())
system.time(test3())
system.time(test4())
system.time(test5())
