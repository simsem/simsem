# Constructor of the distribution objects: Create all random distribution objects.

simNorm <- function(mean, sd) {
    if (sd <= 0) 
        stop("Normal distribution should have standard deviation greater than 0.")
    temp <- new("SimNorm", mean = mean, sd = sd)
}

simUnif <- function(min, max) {
    if (max < min) 
        stop("Uniform distribution should have max greater than min.")
    temp <- new("SimUnif", min = min, max = max)
}

simBeta <- function(shape1, shape2, ncp = 0) {
    if (shape1 < 0) 
        stop("Shape1 should be a positive number.")
    if (shape2 < 0) 
        stop("Shape2 should be a positive number.")
    if (ncp < 0) 
        stop("ncp should be a positive number.")
    temp <- new("SimBeta", shape1 = shape1, shape2 = shape2, ncp = ncp)
}

simBinom <- function(size, prob) {
    if (size < 0) 
        stop("size should be greater than 0.")
    if (prob > 1 | prob < 0) 
        stop("prob should be in between 0 and 1")
    temp <- new("SimBinom", size = size, prob = prob)
}

simCauchy <- function(location = 0, scale = 1) {
    if (scale <= 0) 
        stop("scale parameter should be greater than 0")
    temp <- new("SimCauchy", location = location, scale = scale)
}

simChisq <- function(df, ncp = 0) {
    if (df < 0) 
        stop("df should be non-negative")
    if (ncp < 0) 
        stop("ncp should be non-negative")
    temp <- new("SimChisq", df = df, ncp = ncp)
}

simExp <- function(rate = 1) {
    if (rate <= 0) 
        stop("rate parameter should be greater than 0")
    temp <- new("SimExp", rate = rate)
}

simF <- function(df1, df2, ncp = 0) {
    if (df1 <= 0) 
        stop("df1 should be greater than 0.")
    if (df2 <= 0) 
        stop("df2 should be greater than 0.")
    if (ncp < 0) 
        stop("ncp should be non-negative.")
    temp <- new("SimF", df1 = df1, df2 = df2, ncp = ncp)
}

simGamma <- function(shape, rate = 1) {
    if (shape <= 0) 
        stop("shape should be greater than 0.")
    if (rate <= 0) 
        stop("rate should be greater than 0.")
    temp <- new("SimGamma", shape = shape, rate = rate)
}

simGeom <- function(prob) {
    if (prob > 1 | prob < 0) 
        stop("prob should be in between 0 and 1")
    temp <- new("SimGeom", prob = prob)
}

simHyper <- function(m, n, k) {
    if (m <= 0) 
        stop("m should be positive integer.")
    if (n <= 0) 
        stop("n should be positive integer.")
    if (k <= 0) 
        stop("k should be positive integer.")
    temp <- new("SimHyper", m = m, n = n, k = k)
}

simLnorm <- function(meanlog = 0, sdlog = 1) {
    if (sdlog <= 0) 
        stop("sdlog should be positive.")
    temp <- new("SimLnorm", meanlog = meanlog, sdlog = sdlog)
}

simLogis <- function(location = 0, scale = 1) {
    if (scale <= 0) 
        stop("scale should be positive.")
    temp <- new("SimLogis", location = location, scale = scale)
}

simNbinom <- function(size, prob) {
    if (size <= 0) 
        stop("size should be positive.")
    if (prob > 1 | prob < 0) 
        stop("prob should be in between 0 and 1.")
    temp <- new("SimNbinom", size = size, prob = prob)
}

simPois <- function(lambda) {
    if (lambda <= 0) 
        stop("lambda should be positive.")
    temp <- new("SimPois", lambda = lambda)
}

simT <- function(df, ncp = 0) {
    if (df <= 0) 
        stop("df should be positive.")
    temp <- new("SimT", df = df, ncp = ncp)
}

simWeibull <- function(shape, scale = 1) {
    if (shape <= 0) 
        stop("shape should be positive.")
    if (scale <= 0) 
        stop("scale should be positive.")
    temp <- new("SimWeibull", shape = shape, scale = scale)
} 
