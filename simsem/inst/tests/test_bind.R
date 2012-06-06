source("../../R/bind.R")
source("../../R/AllClass.R")
## Needs more thorough tests, but this will work for now.
## Mostly just checks to see if the function works.


context("bind - SimMatrix")
a <- matrix(0,2,2)
a[,1] <- NA
a[,2] <- "a1"
expect_true(class(bind(free=a)) == "SimMatrix")
expect_true(class(bind(free=a, popParam=.7, misspec=.01)) == "SimMatrix")
expect_true(class(bind(free=a, popParam="runif(1,0,1)", misspec=.01)) == "SimMatrix")
expect_true(class(bind(free=a, popParam="runif(1,0,1)", misspec="runif(1,0,1)")) == "SimMatrix")

  #Error - invalid expression
expect_error(bind(free=a, popParam="runif(1,0,1)", misspec="runif(1,0,1"))
expect_error(bind(free=a, popParam="runif(1,0,1", misspec="runif(1,0,1)"))

  ## Doesn't mean anything, but doesn't throw an error?
expect_true(class(bind(free=a, popParam=1)) == "SimMatrix")

  ## Error - different dimensions
expect_error(bind(free=a, popParam=matrix(0,3,3)))
expect_error(bind(free=a, misspec=matrix(0,3,3)))

context("Equality Constraints")
  a <- matrix(0,2,2)
  a[,1] <- 0
  a[,2] <- "a1"
expect_true(class(bind(free=a)) == "SimMatrix")

  b <- matrix(0,3,3)
  b[1:2,1] <- "b1"
  b[2:3,2] <- "b2"
expect_true(class(bind(free=b)) == "SimMatrix")

expect_true(class(bind(free=matrix(0,2,2))) == "SimMatrix")


###################################

context("bind - SimVector")

a <- c(NA,NA,"a1","a1")
expect_true(class(bind(free=a)) == "SimVector")
expect_true(class(bind(free=a, popParam=.7, misspec=.01)) == "SimVector")
expect_true(class(bind(free=a, popParam="runif(1,0,1)", misspec=.01)) == "SimVector")
expect_true(class(bind(free=a, popParam="runif(1,0,1)", misspec="runif(1,0,1)")) == "SimVector")

  #Error - invalid expression
expect_error(bind(free=a, popParam="runif(1,0,1)", misspec="runif(1,0,1"))
expect_error(bind(free=a, popParam="runif(1,0,1", misspec="runif(1,0,1)"))

  ## Does throw an error in this case.
expect_error(class(bind(free=a, popParam="a1")) == "SimVector")

  ## Error - different dimensions
expect_error(bind(free=a, popParam=c(0,3,3)))
expect_error(bind(free=a, misspec=c(0,3,3)))

context("Equality Constraints")

a <- c(0,0,"a1","a1")
expect_true(class(bind(free=a)) == "SimVector")

b <- c(0,0,"b1","b1",0,0,"b2","b2")
expect_true(class(bind(free=b)) == "SimVector")

expect_true(class(bind(free=c(0,2,2))) == "SimVector")

