library(simsem)
library(testthat)

# Data Generation: 'C' denotes covariate, 'E' is an even number of columns.
data <- matrix(1,ncol=19,nrow=10000)
dataC <- cbind(data,rnorm(10000,0,1))
dataE <- matrix(1,ncol=20,nrow=10000)
dataCE <- cbind(dataE,rnorm(10000,0,1))
dfC <- as.data.frame(dataC)

# Calls to the function that should generate no missing values
#context("No Missing")
expect_equal(sum(is.na(imposeMissing(data))),0)
expect_equal(sum(is.na(imposeMissing(data,cov=1))),0)
expect_equal(sum(is.na(imposeMissing(data,pmMCAR=0))),0)
expect_equal(sum(is.na(imposeMissing(data,pmMAR=0))),0)
expect_equal(sum(is.na(imposeMissing(data,nforms=0))),0)
expect_equal(sum(is.na(imposeMissing(dfC,nforms=0))),0)
expect_equal(sum(is.na(imposeMissing(data,prAttr=0))),0)

#context("MCAR")
percentmis <- mean(replicate(10,sum(is.na(imposeMissing(data,pmMCAR=.1)))/length(data)))
expect_true((percentmis > .099) && (percentmis < .101))

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataE,pmMCAR=.1)))/length(dataE)))
expect_true((percentmis > .099) && (percentmis < .101))

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataC,cov=20,pmMCAR=.1)))/length(dataC)))
#expect_true((percentmis > .099) && (percentmis < .101))


#context("MAR")
percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataC,cov=20,pmMAR=.1)))/length(dataC)))
expect_true((percentmis > .099) && (percentmis < .101))

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataCE,cov=21,pmMAR=.1)))/length(dataCE)))
#expect_true((percentmis > .099) && (percentmis < .101))

#context("Planned")
percentmis <- sum(is.na(imposeMissing(dataE,nforms=3)))/length(dataE)
expect_equal(percentmis,.25)

percentmis <- sum(is.na(imposeMissing(data,nforms=3)))/length(data)
expect_true((percentmis > .26) && (percentmis < .27))

percentmis <- sum(is.na(imposeMissing(dataC,cov=20,nforms=3)))/length(dataC)
expect_equal(percentmis,.25)

percentmis <- sum(is.na(imposeMissing(dataCE,cov=21,nforms=3)))/length(dataCE[,1:20])
expect_equal(percentmis,.25)

test <- imposeMissing(data,twoMethod=c(19,.8))
totalmis <- sum(is.na(test))/length(test)
varmis <- sum(is.na(test[,19]))/length(test[,19])

expect_true((totalmis >.04) && (totalmis < .05))
expect_equal(varmis,.8)

percentmis <- sum(is.na(imposeMissing(dataCE,cov=21,nforms=3,timePoints=2)))/length(dataCE)
expect_true((percentmis > .253) && (percentmis <.254))

percentmis <- sum(is.na(imposeMissing(dataE,nforms=4)))/length(dataE)
expect_equal(percentmis,.20)

expect_error(imposeMissing(data,nforms=3,timePoints=5))
expect_error(imposeMissing(data,nforms=25))
expect_error(imposeMissing(data,nforms=3,itemGroups=4))

percentmis <- sum(is.na(imposeMissing(dataE,nforms=3,itemGroups=list(1:5,6:10,11:13,14:20))))/length(dataE)
expect_equal(percentmis,.25001)

percentmis <- sum(is.na(imposeMissing(dataE,nforms=3,itemGroups=list(1:10,11:20))))/length(dataE)
expect_equal(percentmis,.5)

#context("Attrition")
percentmis <- mean(replicate(100,sum(is.na(imposeMissing(data,prAttr=.1)))/length(data)))
expect_true((percentmis > .099) && (percentmis < .101))

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataE,prAttr=.1,timePoints=5)))/length(data)))
expect_true((percentmis > .270) && (percentmis < .280))

percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataC,cov=20,prAttr=.1,timePoints=5)))/length(data)))
expect_true((percentmis > .130) && (percentmis <.140))

#context("Ignored Variables")
test.dat <- imposeMissing(dataCE,cov=21,nforms=3,timePoints=2)
expect_true(sum(is.na(test.dat[,c(1,2,11,12,21)])) == 0)

test.dat <- imposeMissing(dataCE,cov=21,pmMCAR=.1,ignoreCols=c(9,14))
expect_true(sum(is.na(test.dat[,c(9,14,21)])) == 0)

test.dat <- imposeMissing(dataCE,cov=21,pmMAR=.1,ignoreCols=c(3,6,9))
expect_true(sum(is.na(test.dat[,c(3,6,9,21)]))== 0)

test.dat <- imposeMissing(dataCE,cov=21,nforms=3,ignoreCols=5)
expect_true(sum(is.na(test.dat[,c(1,2,3,4,5,21)]))== 0)

test.dat <- imposeMissing(dataCE,cov=21,prAttr=.1,ignoreCols=c(1,8,14))
expect_true(sum(is.na(test.dat[,c(1,8,14,21)])) == 0)

# context("Additivity")

# percentmis <- mean(replicate(10,sum(is.na(imposeMissing(dataC,covs=c(20,21),pmMCAR=.1,pmMAR=.1,nforms=3)))/length(dataC)))
# expect_true((percentmis > .37) && (percentmis < .386))


