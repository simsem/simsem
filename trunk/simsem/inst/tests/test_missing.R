source("../../R/imposeMissing.R")

data <- matrix(rep(rnorm(10,1,1),19),ncol=19)
datac <- cbind(data,rnorm(10,0,1),rnorm(10,5,5))

context("imposeMissing")
expect_equals( sum(is.na(imposeMissing(data))),0)
expect_equals( sum(is.na(imposeMissing(data,covs=c(1,2))),0)
expect_equals( sum(is.na(imposeMissing(data,pmMCAR=0))),0)
expect_equals( sum(is.na(imposeMissing(data,pmMAR=0))),0)
expect_equals( sum(is.na(imposeMissing(data,nforms=0))),0)
              
