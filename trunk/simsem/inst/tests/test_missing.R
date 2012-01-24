source("../../R/imposeMissing.R")

# Data Generation: 'C' denotes covariates, 'E' is an even number of columns.
data <- matrix(rep(rnorm(1000,1,1),19),ncol=19)
dataC <- cbind(data,rnorm(1000,0,1),rnorm(1000,5,5))
dataE <- matrix(rep(rnorm(1000,1,1),20),ncol=20)
dataCE <- cbind(dataE,rnorm(1000,0,1),rnorm(1000,5,5))

# Calls to the function that should generate no missing values
context("imposeMissing")
expect_equal(sum(is.na(imposeMissing(data))),0)
expect_equal(sum(is.na(imposeMissing(data,covs=c(1,2)))),0)
expect_equal(sum(is.na(imposeMissing(data,pmMCAR=0))),0)
expect_equal(sum(is.na(imposeMissing(data,pmMAR=0))),0)
expect_equal(sum(is.na(imposeMissing(data,nforms=0))),0)

context("MCAR")
percentmis <- mean(replicate(100,sum(is.na(imposeMissing(data,pmMCAR=.1)))/length(data)))
expect_true((percentmis > .099) && (percentmis < .101))

percentmis <- mean(replicate(100,sum(is.na(imposeMissing(dataE,pmMCAR=.1)))/length(dataE)))
expect_true((percentmis > .099) && (percentmis < .101))

percentmis <- mean(replicate(100,sum(is.na(imposeMissing(dataC,covs=c(20,21),pmMCAR=.1)))/length(dataC[,1:19])))
expect_true((percentmis > .099) && (percentmis < .101))


context("MAR")
# percentmis <- mean(replicate(100,sum(is.na(imposeMissing(dataC,covs=c(20,21),pmMAR=.1)))/length(dataC[,1:19])))
# expect_true((percentmis > .099) && (percentmis < .101))

# percentmis <- mean(replicate(100,sum(is.na(imposeMissing(dataCE,covs=c(21,22),pmMAR=.1)))/length(dataCE[,1:20])))
# expect_true((percentmis > .099) && (percentmis < .101))

context("planned")
percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataE,nforms=3)))/length(dataE)))
expect_equal(percentmis,.25)

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(data,nforms=3)))/length(data)))
expect_true((percentmis > .26) && (percentmis < .27))

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataC,covs=c(20,21),nforms=3)))/length(dataC[,1:19])))
expect_true((percentmis > .26) && (percentmis < .27))

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataCE,covs=c(21,22),nforms=3)))/length(dataCE[,1:20])))
expect_equal(percentmis,.25)

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(data,twoMethod=c(19,.8))))/length(data)))
expect_true((percentmis >.04) && (percentmis < .05))

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataCE,covs=c(21,22),nforms=3,timePoints=2)))
                             /length(dataCE[,1:20])))
expect_true(percentmis == .2667)

test.dat <- imposeMissing(dataCE,covs=c(21,22),nforms=3,timePoints=2)
expect_true(sum(is.na(test.dat[,c(1,2,11,12,21,22)])) == 0)

test.dat <- imposeMissing(dataCE,covs=c(4,8),nforms=3,timePoints=2)
expect_true(sum(is.na(test.dat[,c(1,2,4,8,13,14)])) == 0)

# percentmis <- mean(replicate(100,sum(is.na(imposeMissing(dataC,covs=c(20,21),pmMCAR=.1,pmMAR=.1,nforms=3)))/length(dataC)))
# expect_true((percentmis > .37) && (percentmis < .386))
