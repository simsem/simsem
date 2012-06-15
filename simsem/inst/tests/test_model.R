source("../../R/model.R")
source("../../R/AllClass.R")
source("../../R/bind.R")
library(lavaan)


## Needed tests:
## 1. Symmetric matrices are correctly required




context("CFA")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- "a1"
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- bind(latent.cor, 0.5,symmetric=TRUE)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTE <- bind(error.cor,symmetric=TRUE)

# Needs model type
expect_error(model(LY=LY,RPS=RPS,RTE=RTE))
cfat <- model(LY=LY,RPS=RPS,RTE=RTE,modelType="CFA")

## # a useful test data structure
## paramSet <- list(LY=LY,RPS=RPS,PS=NULL,TE=NULL,RTE=RTE,BE=NULL,VTE=NULL,VY=NULL,
##                  VPS=NULL,TY=NULL,AL=NULL,MY=NULL,ME=NULL)

## paramSet2 <- list(LY=c(LY,LY),RPS=c(RPS,RPS),PS=NULL,TE=NULL,RTE=RTE,BE=NULL,VTE=NULL,VY=NULL,
##                  VPS=NULL,TY=NULL,AL=NULL,MY=NULL,ME=NULL)


context("CFA2")

loading <- matrix(0, 9, 3)
loading[1:3, 1] <- c(1, NA, NA)
loading[4:6, 2] <- c(1, NA, NA)
loading[7:9, 3] <- c(1, NA, NA)
loadingVal <- matrix(0, 9, 3)
loadingVal[2:3, 1] <- c(0.6, 0.7)
loadingVal[5:6, 2] <- c(1.1, 0.9)
loadingVal[8:9, 3] <- c(1.2, 1.1)
LY <- bind(loading, loadingVal)

facCov <- matrix(NA, 3, 3)
facCovVal <- diag(c(0.8, 0.9, 0.4))
facCovVal[lower.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
facCovVal[upper.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
PS <- bind(facCov, facCovVal,symmetric=TRUE)

errorCov <- diag(NA, 9)
errorCovVal <- diag(c(0.5, 1.1, 0.8, 0.4, 0.4, 0.8, 0.8, 0.5, 0.6))
TE <- bind(errorCov, errorCovVal, symmetric=TRUE)

AL <- bind(rep(NA, 3), 0)
TY <- bind(c(0, NA, NA, 0, NA, NA, 0, NA, NA), 0)

context("Path")

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1,0.3,0.5)"
starting.BE[4, 3] <- "runif(1,0.5,0.7)"
BE <- bind(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- bind(residual.error, "rnorm(1,0.3,0.1)", symmetric=TRUE)

ME <- bind(rep(NA, 4), 0)


context("SEM")

loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
LY <- bind(loading, loading.start)

RTE <- bind(diag(8), symmetric=TRUE)

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- bind(factor.cor, 0.5, symmetric=TRUE)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"
BE <- bind(path, path.start)

expect_error(model(LY=cfa$LY, RPS=cfa$RPS, RTE=cfa$RTE))

context("Lavaan Example")

loading <- matrix(0,9,3)
loading[2:3,1] <- NA
loading[5:6,2] <- NA
loading[8:9,3] <- NA
loading[1,1] <- 1
loading[4,2] <- 1
loading[7,3] <- 1
LY <- bind(loading)

factor.cor <- diag(3)
diag(factor.cor) <- NA
factor.cor[lower.tri(factor.cor)] <- factor.cor[upper.tri(factor.cor)] <- NA
RPS <- bind(factor.cor,symmetric=TRUE)

rte <- diag(9)
diag(rte) <- NA
RTE <- bind(rte, symmetric=TRUE)

lavtemp <- model(LY=LY,RPS=RPS,RTE=RTE,modelType="CFA")

fit1 <- lavaan(lavtemp@pt,data=HolzingerSwineford1939)
est1 <- parameterEstimates(fit1)[,1:5]
est1 <- est1[(est1[,4] != 0),]

HS.model <- '
  y1  =~ x1 + x2 + x3
  y2 =~ x4 + x5 + x6
  y3   =~ x7 + x8 + x9
  x1 ~ 1
  x2 ~ 1
  x3 ~ 1
  x4 ~ 1
  x5 ~ 1
  x6 ~ 1
  x7 ~ 1
  x8 ~ 1
  x9 ~ 1'

fit <- lavaan(HS.model, data=HolzingerSwineford1939,
              auto.var=TRUE, auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)

# Match up parameter estimates
est <- parameterEstimates(fit)[,1:5]
est <- est[(est[,4] != 0),]
est1 <- est1[order(est1[,2],est1[,1]),]
est <- est[order(est[,2],est[,1]),]

# Parameter Estimates & SE
expect_true(all.equal(est1[,4],est[,4],tolerance=1e-6))
expect_true(all.equal(est[,5],est[,5],tolerance=1e-6))
