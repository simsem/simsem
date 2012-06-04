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
expect_true(class(bind(free=a, popParam="a")) == "SimMatrix")

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
  bind(free=b)

expect_true(class(bind(free=matrix(0,2,2))) == "SimMatrix")

  ## Invalid label name. Throws error
  f <- matrix(0,3,3)
  f[1,1] <- "c"
  f[1,2] <- "c1"
expect_error(bind(free=f))
