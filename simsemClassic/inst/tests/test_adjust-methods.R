source("../../R/AllClass.R")
source("../../R/AllGenerics.R")
source("../../R/simMatrix.R")
source("../../R/simUnif.R")
source("../../R/simVector.R")
source("../../R/adjust-methods.R")

context("adjust-methods: SimMatrix")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)
u34 <- simUnif(0.3, 0.4)

LX2 <- adjust(LX, "u34", c(2, 1))
expect_that(LX2@param[2,1],matches("u34"))
expect_true(class(LX2)[1] == "SimMatrix")

LX3 <- adjust(LX, 0, c(2,1))
expect_that(LX3@param[2,1],matches(""))

LX4 <- adjust(LX, 0.5, c(2,2), FALSE)
expect_that(LX4@param[2,2],matches("0.5"))
expect_true(is.na(LX4@free[2,2]))

context("adjust-methods: SimVector")

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- simVector(factor.mean, factor.mean.starting)
n01 <- simUnif(0, 1)
AL2 <- adjust(AL, "n01", 2)

expect_true(AL2@param[2] == "n01")
expect_true(class(AL2)[1] == "SimVector")
