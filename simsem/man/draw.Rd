\name{draw}
\alias{draw}
\title{
	Draw parameters from a \code{\link{SimSem}} object.
}
\description{
	This function draws parameters from a \code{\link{SimSem}} template, for debugging or other use. Used internally to create data. Data can be created in one step from a \code{SimSem} object using \code{\link{generate}}.
}
\usage{
draw(model, maxDraw=50, misfitBounds=NULL, averageNumMisspec=FALSE, 
optMisfit = NULL, optDraws=50, misfitType="f0", createOrder = c(1, 2, 3))
}
\arguments{
  \item{model}{
	A \code{\link{SimSem}} object. 
}
  \item{maxDraw}{
	Integer specifying the maximum number of attempts to draw a valid set of parameters (no negative error variance, standardized coefficients over 1).
}
  \item{misfitBounds}{
	Vector that contains upper and lower bounds of the misfit measure. Sets of parameters drawn that are not within these bounds are rejected.
}
  \item{averageNumMisspec}{
	If \code{TRUE}, the provided fit will be divided by the number of misspecified parameters. 
}
  \item{optMisfit}{
	Character vector of either "min" or "max" indicating either maximum or minimum optimized misfit. If not null, the set of parameters out of the number of draws in "optDraws" that has either the maximum or minimum misfit of the given misfit type will be returned.
}
  \item{optDraws}{
	Number of parameter sets to draw if optMisfit is not null. The set of parameters with the maximum or minimum misfit will be returned.
}
  \item{misfitType}{
	Character vector indicating the fit measure used to assess the misfit of a set of parameters. Can be "f0", "rmsea", "srmr", or "all". 
}
  \item{createOrder}{
    The order of 1) applying equality/inequality constraints, 2) applying misspecification, and 3) fill unspecified parameters (e.g., residual variances when total variances are specified). The specification of this argument is a vector of different orders of 1 (constraint), 2 (misspecification), and 3 (filling parameters). For example, \code{c(1, 2, 3)} is to apply constraints first, then add the misspecification, and finally fill all parameters. 
}
}
\value{
	Nested list of drawn parameters in the form \code{[[Group]][[param,misspec,misOnly]][[SimMatrix]]}. E.g. The LY parameter matrix of the first group would be indexed as \code{obj[[1]]$param$LY}.
The values in $param are the raw parameter values with no misspecification. The values in $misspec are raw parameter values + misspecification. The values in $misOnly are only the misspecification values. 
}
\author{	
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu}), Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu}) 
}
\seealso{		
	 \code{\link{createData}} To generate random data using a set of parameters from \code{\link{draw}}
 }
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

# Draw a parameter set for data generation.
param <- draw(CFA.Model)

# Example of Multiple Group Model with Weak Invariance

loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- c("load1", "load2", "load3")
loading.in[4:6, 2] <- c("load4", "load5", "load6")
mis <- matrix(0,6,2)
mis[loading.in == "0"] <- "runif(1, -0.1, 0.1)"
LY.in <- bind(loading.in, "runif(1, 0.7, 0.8)", mis)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

VPS1 <- bind(rep(1, 2))

VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))

# Inequality constraint
script <- "
sth := load1 + load2 + load3
load4 == (load5 + load6) / 2
load4 > 0
load5 > 0
sth2 := load1 - load2
"

# Model Template
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA", con=script)

# Constraint --> Misspecification --> Fill Parameters
draw(weak, createOrder=c(1, 2, 3))

# Constraint --> Fill Parameters --> Misspecification 
draw(weak, createOrder=c(1, 3, 2))

# Misspecification --> Constraint --> Fill Parameters
draw(weak, createOrder=c(2, 1, 3))

# Misspecification --> Fill Parameters --> Constraint
draw(weak, createOrder=c(2, 3, 1))

# Fill Parameters --> Constraint --> Misspecification
draw(weak, createOrder=c(3, 1, 2))

# Fill Parameters --> Misspecification --> Constraint
draw(weak, createOrder=c(3, 2, 1))
}
