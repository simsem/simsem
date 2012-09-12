# toFunction: Convert an x to string

# Distribution x: Provide a summary of each distribution x

setMethod("toFunction", signature(x = "SimNorm"), function(x) {
    paste("rnorm(1, ", x@mean, ", ", x@sd, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimUnif"), function(x) {
    paste("runif(1, ", x@min, ", ", x@max, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimBeta"), function(x) {
    paste("rbeta(1, ", x@shape1, ", ", x@shape2, ", ", x@ncp, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimBinom"), function(x) {
    paste("rbinom(1, ", x@size, ", ", x@prob, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimCauchy"), function(x) {
    paste("rcauchy(1, ", x@location, ", ", x@scale, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimChisq"), function(x) {
    paste("rchisq(1, ", x@df, ", ", x@ncp, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimExp"), function(x) {
    paste("rexp(1, ", x@rate, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimF"), function(x) {
    paste("rf(1, ", x@df1, ", ", x@df2, ", ", x@ncp, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimGamma"), function(x) {
    paste("rgamma(1, ", x@shape, ", ", x@rate, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimGeom"), function(x) {
    paste("rgeom(1, ", x@prob, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimHyper"), function(x) {
    paste("rhyper(1, ", x@m, ", ", x@n, ", ", x@k, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimLnorm"), function(x) {
    paste("rlnorm(1, ", x@meanlog, ", ", x@sdlog, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimLogis"), function(x) {
    paste("rlogis(1, ", x@location, ", ", x@scale, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimNbinom"), function(x) {
    paste("rnbinom(1, ", x@size, ", ", x@prob, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimPois"), function(x) {
    paste("rpois(1, ", x@lambda, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimT"), function(x) {
    paste("rt(1, ", x@df, ", ", x@ncp, ")", sep = "")
})

setMethod("toFunction", signature(x = "SimWeibull"), function(x) {
    paste("rweibull(1, ", x@shape, ", ", x@scale, ")", sep = "")
})
 
