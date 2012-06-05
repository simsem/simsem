source("../../R/AllClass.R")
source("../../R/bind.R")
source("../../R/model.R")

  cfa <- function() {
    loading <- matrix(0, 6, 2)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    LY <- bind(loading, 0.7)

    latent.cor <- matrix(NA, 2, 2)
    diag(latent.cor) <- 1
    RPS <- bind(latent.cor, 0.5)

    error.cor <- matrix(0, 6, 6)
    diag(error.cor) <- 1
    RTE <- bind(error.cor)

    return(list(LX,RPH,RTD))
  }
  # CFA with more matrices
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
    PS <- bind(facCov, facCovVal)

    errorCov <- diag(NA, 9)
    errorCovVal <- diag(c(0.5, 1.1, 0.8, 0.4, 0.4, 0.8, 0.8, 0.5, 0.6))
    TE <- bind(errorCov, errorCovVal)

    AL <- bind(rep(NA, 3), 0)
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
    RPS <- bind(residual.error, "rnorm(1,0.3,0.1)")

    ME <- bind(rep(NA, 4), 0)

    return(list(BE=BE,RPS=RPS,ME=ME))
  }

  sem <- function() {
    loading <- matrix(0, 8, 3)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    loading[7:8, 3] <- NA
    loading.start <- matrix("", 8, 3)
    loading.start[1:3, 1] <- 0.7
    loading.start[4:6, 2] <- 0.7
    loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
    LY <- bind(loading, loading.start)

    RTE <- bind(diag(8))

    factor.cor <- diag(3)
    factor.cor[1, 2] <- factor.cor[2, 1] <- NA
    RPS <- bind(factor.cor, 0.5)

    path <- matrix(0, 3, 3)
    path[3, 1:2] <- NA
    path.start <- matrix(0, 3, 3)
    path.start[3, 1] <- "rnorm(1,0.6,0.05)"
    path.start[3, 2] <- "runif(1,0.3,0.5)"
    BE <- bind(path, path.start)

    return(list(LY=LY,RTE=RTE,RPS=RPS,BE=BE))
  }

cfa <- cfa()
cfa2 <- cfa2()
path <- path()
sem <- sem()

expect_error(model(LY=cfa$LY, RPS=cfa$RPS, RTE=cfa$RTE))

