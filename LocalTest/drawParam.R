		
test.draw <- function() {
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

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType="path")

param1 <- draw(Path.Model, misfitBounds = c(0.10, 0.12), misfitType="rmsea")
param2 <- draw(Path.Model, misfitBounds = c(0.03, 0.05), misfitType="f0")
param3 <- draw(Path.Model, optMisfit = "max", misfitType="f0")
param4 <- draw(Path.Model, optMisfit = "min", misfitType="f0")
param5 <- draw(Path.Model, optMisfit = "max", misfitType="f0", optDraws = 10)

}

test.unequalCon <- function() {
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
script <- "
sth := load1 + load2 + load3
load4 == (load5 + load6) / 2
load4 > 0
load5 > 0
sth2 := load1 - load2
"
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA", con=script)
dat <- generate(weak, 200)
out <- analyze(weak, dat)
Output <- sim(2, weak, n=200) 
}

test.order <- function() {
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
script <- "
sth := load1 + load2 + load3
load4 == (load5 + load6) / 2
load4 > 0
load5 > 0
sth2 := load1 - load2
"
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA", con=script)
draw(weak, createOrder=c(1, 2, 3))
draw(weak, createOrder=c(1, 3, 2))
draw(weak, createOrder=c(2, 1, 3))
draw(weak, createOrder=c(2, 3, 1))
draw(weak, createOrder=c(3, 1, 2))
draw(weak, createOrder=c(3, 2, 1))
dat1 <- generate(weak, 200, createOrder=c(1, 2, 3))
dat2 <- generate(weak, 200, createOrder=c(1, 3, 2))
dat3 <- generate(weak, 200, createOrder=c(2, 1, 3))
dat4 <- generate(weak, 200, createOrder=c(2, 3, 1))
dat5 <- generate(weak, 200, createOrder=c(3, 1, 2))
dat6 <- generate(weak, 200, createOrder=c(3, 2, 1))
Output1 <- sim(3, weak, n=200, createOrder=c(1, 2, 3))
Output2 <- sim(3, weak, n=200, createOrder=c(1, 3, 2))
Output3 <- sim(3, weak, n=200, createOrder=c(2, 1, 3))
Output4 <- sim(3, weak, n=200, createOrder=c(2, 3, 1))
Output5 <- sim(3, weak, n=200, createOrder=c(3, 1, 2))
Output6 <- sim(3, weak, n=200, createOrder=c(3, 2, 1))
}
