\name{bind}
\alias{bind}
\alias{binds}
\title{Specify matrices for Monte Carlo simulation of structural equation models}
\description{
Create \code{\linkS4class{SimMatrix}} or \code{\linkS4class{SimVector}} object that specifies
\enumerate{
\item{Pattern of fixed/freed parameters for analysis}
\item{Population parameter values for data generation}
\item{Any model misspecification (true population parameter is different than the one specified) for these parameters.} }
Each matrix in the Lisrel-style notation is specified in this way (e.g. LY, PS, and TE) and is used to create a model analysis template and a data generation template for simulation through the \code{model} function.
}
\usage{
bind(free = NULL, popParam = NULL, misspec = NULL, symmetric = FALSE)
binds(free = NULL, popParam = NULL, misspec = NULL, symmetric = TRUE)
}
\arguments{
  \item{free}{Required matrix or vector where each element represents a fixed or freed parameter used for analysis with structural equation models. Parameters can be freed by setting the corresponding element in the matrix to \code{NA}, and can be fixed by setting the value of the element to any number (e.g. 0). Parameters can be labeled using any character string. Any labeled parameter is considered to be free, and parameters with identical labels will be constrained to equality for analysis.}

  \item{popParam}{Optional matrix or vector of identical dimension to the free matrix whose elements contain population parameter values for data generation in simulation. For simlutation, each free parameter requires a population parameter value, which is a quoted numeric value. Parameters that don't have population values are left as empty strings.
Population parameters can also be drawn from a distribution. This is done by wrapping a call to create 1 value from an existing random generation function in quotes: e.g \code{"runif(1,0,1)","rnorm(1,0,.01)"} Every replication in the simulation will draw a parameter value from this distribution. The function checks that what is quoted is valid R.

If a random population parameter is constrained to equality in the free matrix, \emph{each drawn population parameter value will be the same.} More details on data generation is available in \code{?generate}, \code{?createData}, and \code{?draw}.

To simplify the most common case, \code{popParam} can take 1 value or distribution and create a matrix or vector that assigns that population parameter or distribution to all freed parameters. These population values are used as starting values for analysis by default.}

   \item{misspec}{Optional matrix or vector of identical dimension to the free matrix whose elements contain population parameter values for specifying misspecification. Elements of the misspec matrix contain population parameters that are added to parameters that are fixed or have an existing population value. These parameters are also quoted numeric strings, and can optionally be drawn from distributions as described above. To simplify the most common case, misspec can take 1 value or distribution and create a matrix or vector that assigns that value or distribution to all previously specified fixed parameters. Details about misspecification are included the data generation functions.}

   \item{symmetric}{Set as \code{TRUE} if the matrix created is symmetric (RPS/PS, RTE/TE). The function \code{binds} can also be used, which defaults to \code{symmetric = TRUE} }
}
\details{
Bind is the first step in the \code{bind} -> \code{model} -> \code{sim} workflow of \emph{simsem}, and this document outlines the user interface or language used to describe these simulations. This interface, while complex, enables a wide array of simulation specifications for structural equation models by building on LISREL-style parameter specifications.

In simulations supported by \emph{simsem}, a given parameter may be either fixed or freed for analysis, but may optionally also have a population value or distribution for data generation, or a value or distribution of misspecification. The purpose of bind is to stack these multiple meanings of a parameter into an object recognized by \emph{simsem}, a \code{\linkS4class{SimMatrix}}. Each matrix in the Lisrel notation (e.g. LY, PS, TE, BE) becomes a \code{\linkS4class{SimMatrix}}, and is passed to the function \code{model}, which builds the data generation template and an analysis template (a lavaan parameter table), collectively forming a \code{SimSem} object, which can be passed to the function \code{sim} for simulation.
}
\value{ \code{\linkS4class{SimMatrix}} or \code{\linkS4class{SimVector}} object that used for model specification for analysis and data generation in simsem.}

\author{
    Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu}),
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
\itemize{\item {\code{\link{model}} To combine simMatrix objects into a complete data analysis and data generation template, which is a \code{\linkS4class{SimSem}} object}
\item \code{\link{generate}} To generate data using the simsem template.
\item \code{\link{analyze}} To analyze real or generated data using the simsem template.
}
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LY <- bind(loading, loadingValues)
summary(LY)

# Set both factor correlations to .05
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

# Misspecify all error covarainces
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- NA
RTE <- binds(error.cor,1,"runif(1,-.05,.05)")
}