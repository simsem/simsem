# Constructor of the distribution objects
# Functions -- simsem package
# Description: Create all random distribution objects.  
# Return: 	Distribution class 
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 23, 2011

simNorm <- function(mean, sd) {
	if(sd <= 0) stop("Normal distribution should have standard deviation greater than 0.")
	temp <- new("SimNorm", mean=mean, sd=sd)
}
# Normal Distribution
# Argument: mean = population mean, sd = standard deviation

simUnif <- function(min, max) {
	if(max < min) stop("Uniform distribution should have max greater than min.")
	temp <- new("SimUnif", min=min, max=max)
}
# Uniform Distribution
# Argument: min = lower bound, max = upper bound

simBeta <- function(shape1, shape2, ncp=0) {
	if(shape1 < 0) stop("Shape1 should be a positive number.")
	if(shape2 < 0) stop("Shape2 should be a positive number.")
	if(ncp < 0) stop("ncp should be a positive number.")
	temp <- new("SimBeta", shape1=shape1, shape2=shape2, ncp=ncp)
}
# Beta Distribution
# Attributes: shape1, shape2 = positive numbers of beta distributions, ncp = non-centrality parameter (shape1, shape2 > 0)

simBinom <- function(size, prob) {
	if(size < 0) stop("size should be greater than 0.")
	if(prob > 1 | prob < 0) stop("prob should be in between 0 and 1")
	temp <- new("SimBinom", size=size, prob=prob)
}
# Binomial Distribution
# Attributes: size = Number of trials (zero or more), prob = probability of success on each trial (0 to 1)

simCauchy <- function(location = 0, scale = 1) {
	if(scale <= 0) stop("scale parameter should be greater than 0")
	temp <- new("SimCauchy", location=location, scale=scale)
}
# Cauchy Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

simChisq <- function(df, ncp=0) {
	if(df < 0) stop("df should be non-negative")
	if(ncp < 0) stop("ncp should be non-negative")
	temp <- new("SimChisq", df=df, ncp=ncp)
}
# Chi-squared Distribution
# Attributes: df = degrees of freedom (non-negative), ncp = non-centrality parameter (non-negative)

simExp <- function(rate=1) {
	if(rate <= 0) stop("rate parameter should be greater than 0")
	temp <- new("SimExp", rate=rate)
}
# Exponential Distribution
# Attributes: rate = rate parameter

simF <- function(df1, df2, ncp=0) {
	if(df1 <= 0) stop("df1 should be greater than 0.")
	if(df2 <= 0) stop("df2 should be greater than 0.")
	if(ncp < 0) stop("ncp should be non-negative.")
	temp <- new("SimF", df1=df1, df2=df2, ncp=ncp)
}
# F-distribution
# Attributes: df1, df2 = degrees of freedom (>0), ncp = non-centrality parameter (>=0)

simGamma <- function(shape, rate=1) {
	if(shape <= 0) stop("shape should be greater than 0.")
	if(rate <= 0) stop("rate should be greater than 0.")
	temp <- new("SimGamma", shape=shape, rate=rate)
}
# Gamma Distribution
# Attributes: shape = Shape parameter, scale = Scale parameter

simGeom <- function(prob) {
	if(prob > 1 | prob < 0) stop("prob should be in between 0 and 1")
	temp <- new("SimGeom", prob=prob)
}
# Geometric Distribution
# Attributes: prob = probability of successes

simHyper <- function(m, n, k) {
	if(m <= 0) stop("m should be positive integer.")
	if(n <= 0) stop("n should be positive integer.")
	if(k <= 0) stop("k should be positive integer.")
	temp <- new("SimHyper", m=m, n=n, k=k)
}
# Hypergeometric Distribution
# Attributes: m = The number of successes, n = The number of failures, k =  The number of drawns (All are integers)

simLnorm <- function(meanlog = 0, sdlog = 1) {
	if(sdlog <= 0) stop("sdlog should be positive.")
	temp <- new("SimLnorm", meanlog=meanlog, sdlog=sdlog)
}
# Log Normal Distribution
# Attributes: meanlog = mean of the distribution in log scale, sdlog = standard deviation of the distribution in log scale (sdlog > 0)

simLogis <- function(location = 0, scale = 1) {
	if(scale <= 0) stop("scale should be positive.")
	temp <- new("SimLogis", location=location, scale=scale)
}
# Logistic Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

simNbinom <- function(size, prob) {
	if(size <= 0) stop("size should be positive.")
	if(prob > 1 | prob < 0) stop("prob should be in between 0 and 1.")
	temp <- new("SimNbinom", size=size, prob=prob)
}
# Negative Binomial Distribution
# Attributes: size = Target for number of sucessful trials (> 0), prob = probability of each trials (0 < p < 1)

simPois <- function(lambda) {
	if(lambda <= 0) stop("lambda should be positive.")
	temp <- new("SimPois", lambda=lambda)
}
# Poisson Distribution
# Attributes: lambda = mean and variance (> 0)

simT <- function(df, ncp=0) {
	if(df <= 0) stop("df should be positive.")
	temp <- new("SimT", df=df, ncp=ncp)
}
# Student t Distribution
# Attributes: df = degree of freedom (> 0), ncp = non-centrality parameter

simWeibull <- function(shape, scale = 1) {
	if(shape <= 0) stop("shape should be positive.")
	if(scale <= 0) stop("scale should be positive.")
	temp <- new("SimWeibull", shape=shape, scale=scale)	
}
# Weibull Distribution
# Attributes: shape = shape parameter, scale = scale parameter (> 0)
