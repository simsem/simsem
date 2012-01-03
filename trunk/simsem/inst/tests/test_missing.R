source("../../R/imposeMissing.R")

data <- matrix(rep(rnorm(1000,1,1),19),ncol=19)
datac <- cbind(data,rnorm(1000,0,1),rnorm(10,5,5))

context("imposeMissing")
expect_equal(sum(is.na(imposeMissing(data))),0)
expect_equal(sum(is.na(imposeMissing(data,covs=c(1,2)))),0)
expect_equal(sum(is.na(imposeMissing(data,pmMCAR=0))),0)
expect_equal(sum(is.na(imposeMissing(data,pmMAR=0))),0)
expect_equal(sum(is.na(imposeMissing(data,nforms=0))),0)

context("MCAR")
percentmis <- sum(is.na(imposeMissing(data,pmMCAR=.1)))/length(data)
expect_true((percentmis > .095) && (percentmis < .105))

context("MAR")
percentmis <- sum(is.na(imposeMissing(datac,covs=c(20,21),pmMAR=.1)))/length(datac[,1:19])
expect_true((percentmis > .09) && (percentmis < .11))
