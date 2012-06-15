source("../../R/AllClass.R")
source("../../R/model.R")
source("../../R/drawParam.R")
source("../../R/bind.R")
source("../../R/find.R")
source("../../R/validate.R")
source("../../R/createData.R")
source("../../R/simDist-constructor.R")
source("../../R/generate.R")

## Tests that check correct calculated parameter values?
## Tests that check matrices correctly reduced?
## - Check all model Types
## - Check mg for all model types
## - Check non-normal data distribution
## - Check misspecification

cfaT <- function() {
  loading <- matrix(0, 6, 2)
  loading[1:3, 1] <- NA
  loading[4:6, 2] <- "a1"
  lmis <- matrix("",6,2)
  lmis[4:5,2] <- "runif(1,.01,.02)"
  LY <- bind(loading, "runif(1,.6,.8)",lmis)

  latent.cor <- matrix(NA, 2, 2)
  diag(latent.cor) <- 1
  RPS <- bind(latent.cor, 0.5,symmetric=TRUE)

  error.cor <- matrix(0, 6, 6)
  diag(error.cor) <- NA
  RTE <- bind(error.cor,popParam=1,"runif(1,0.01,0.015)",symmetric=TRUE)

  return(list(LY=LY,RPS=RPS,RTE=RTE))
}

## CFA with more matrices
cfa2 <- function() {
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
  TE <- bind(errorCov, errorCovVal,symmetric=TRUE)

  AL <- bind(rep(NA, 3), 0, rep("runif(1,.01,.015)",3))
  TY <- bind(c(0, NA, NA, 0, NA, NA, 0, NA, NA), 0)

  return(list(LY=LY,PS=PS,TE=TE,AL=AL,TY=TY))
}
                                        # Path
path <- function() {
  path.BE <- matrix(0, 4, 4)
  path.BE[3, 1:2] <- NA
  path.BE[4, 3] <- NA
  starting.BE <- matrix("", 4, 4)
  starting.BE[3, 1:2] <- "runif(1,0.3,0.5)"
  starting.BE[4, 3] <- "runif(1,0.5,0.7)"
  BE <- bind(path.BE, starting.BE)

  residual.error <- diag(4)
  residual.error[1,2] <- residual.error[2,1] <- NA
  RPS <- bind(residual.error, "rnorm(1,0.3,0.1)",symmetric=TRUE)

  ME <- bind(rep(NA, 4), 0)
  
  return(list(BE=BE,RPS=RPS,ME=ME))
}
                                        # SEM
sem <- function() {
  loading <- matrix(0, 8, 3)
  loading[1:3, 1] <- NA
  loading[4:6, 2] <- NA
  loading[7:8, 3] <- NA
  loading.start <- matrix("", 8, 3)
  loading.start[1:3, 1] <- 0.7
  loading.start[4:6, 2] <- 0.7
  loading.start[7:8, 3] <- "rnorm(1,0.6,0.08)"
  LY <- bind(loading, loading.start)

  rte <- diag(8)
  diag(rte) <- NA
  RTE <- bind(rte,1,symmetric=TRUE)

  factor.cor <- diag(3)
  factor.cor[1, 2] <- factor.cor[2, 1] <- NA
  RPS <- bind(factor.cor, 0.5,symmetric=TRUE)

  path <- matrix(0, 3, 3)
  path[3, 1:2] <- NA
  path.start <- matrix(0, 3, 3)
  path.start[3, 1] <- "rnorm(1,0.6,0.05)"
  path.start[3, 2] <- "runif(1,0.3,0.5)"
  BE <- bind(path, path.start)

  
  return(list(LY=LY,RTE=RTE,RPS=RPS,BE=BE))
}

holz <- function() {
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
  factor.cor[lower.tri(factor.cor)] <- NA
  RPS <- bind(factor.cor,symmetric=TRUE)

  rte <- diag(9)
  diag(rte) <- NA
  RTE <- bind(rte,symmetric=TRUE)

  template <- model(LY=LY,RPS=RPS,RTE=RTE,modelType="CFA")
  
  fit <- lavaan(template@pt,data=HolzingerSwineford1939)
}

cfa <- cfaT()
cfa2 <- cfa2()
path <- path()
sem <- sem()

tcfa <- model(LY=cfa$LY,RPS=cfa$RPS,RTE=cfa$RTE, modelType="CFA")
tcfamg <- model(LY=cfa$LY,RPS=cfa$RPS,RTE=cfa$RTE, modelType="CFA",ngroups=2)
tcfamg2 <- model(LY=list(cfa$LY,cfa$LY),RPS=list(cfa$RPS,cfa$RPS),RTE=cfa$RTE, modelType="CFA")

## tcf
tcfa2 <- model(LY=cfa2$LY,PS=cfa2$PS,TE=cfa2$TE,AL=cfa2$AL,TY=cfa2$TY, modelType="CFA") 
tpath <- model(BE=path$BE, RPS=path$RPS, ME=path$ME, modelType="Path")
tsem <- model(LY=sem$LY, RTE=sem$RTE, RPS=sem$RPS, BE=sem$BE, modelType="SEM")

## cfa1d <- drawOnce(tcfa@dgen,modelType="CFA",misspec=TRUE,numFree=max(tcfa@pt$free))
## cfad <- drawOnce(tcfa2@dgen,modelType="CFA",misspec=FALSE,numFree=max(tcfa2@pt$free))
## pathd <- drawOnce(tpath@dgen,modelType="Path",misspec=FALSE,numFree=max(tpath@pt$free))
## semd <- drawOnce(tsem@dgen,modelType="Sem",misspec=FALSE,numFree=max(tsem@pt$free))
## cfa1do <- drawOnce(tcfa@dgen,modelType="CFA",misspec=TRUE,numFree=max(tcfa@pt$free),
##                   optMisfit="max",numIter=20)

drawParam(tcfa)
drawParam(tcfa2)
drawParam(tpath)
drawParam(tsem)

drawParam(tcfamg)
drawParam(tcfamg2)

## What should the scale be for misfitBounds?
drawParam(tcfa,misfitType="all") # does nothing
drawParam(tcfa,misfitBounds=c(.001,.1),maxDraw=100,misfitType="srmr")
drawParam(tcfa,misfitBounds=c(.001,.1),maxDraw=100,misfitType="rmsea")
drawParam(tcfa,misfitBounds=c(0,1),maxDraw=100,misfitType="f0")
drawParam(tcfa,misfitBounds=c(0,1),
          maxDraw=100,misfitType="rmsea",averageNumMisspec=TRUE)
drawParam(tcfa,misfitType="rmsea",optMisfit="max",numIter=20)

p <- drawParam(tcfa)

dat <- createData(p,100,"CFA")
indDist <- simDataDist(simNorm(10,2),p=6)

dat <- createData(p,100,"CFA",indDist=indDist)

dat <- generate(tcfa,100)
out <- analyze(tcfa,dat)

dat2 <- generate(tcfamg,1000)
out <- analyze(tcfamg,dat2)

dat3 <- generate(tcfamg2,500)
out <- analyze(tcfamg2,dat3)

dat4 <- generate(tpath,400)
outPath <- analyze(tpath, generate(tpath,400))

dat5 <- generate(tsem,400)
outSem <- analyze(tsem,dat5)
