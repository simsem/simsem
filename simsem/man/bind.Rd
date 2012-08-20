\name{bind}
\name{binds}
\alias{bind}
\title{
	Bind together R matrices into a simMatrix for simulation and analysis of structural equation models, using a Lisrel-style notation. 
}
\description{
	Create \code{\linkS4class{SimMatrix}} object that save free parameters, as well as fixed values. 
	This will be used for model specification later, such as for factor loading matrix or regression coefficient matrix.
}
\usage{
simMatrix(free = NULL, value = NULL)
}
\arguments{
  \item{free}{
	Required matrix or vector where each element represents a fixed or freed parameter used for analysis with structural equation models. Parameters can be freed by setting the corresponding element in the matrix to \code{NA}, and can be fixed by setting the value of the element to any number (e.g. 0). Parameters can be labeled using any character string, and parameters with identical labels will be constrained to equality. 
}
  \item{popParam}{
	Optional matrix or vector of identical dimension to the free matrix whose elements contain population parameter values for data generation in simulation. For simlutation, each free parameter requires a population parameter value, which is a quoted numeric value. Parameters that don't have population values are left as empty strings.
Population parameters can also be drawn from a distribution. This is done by wrapping a call to create 1 value from an existing random generation function in quotes: e.g \code{"runif(1,0,1)","rnorm(1,0,.01)"} Every replication in the simulation will draw a parameter value from this distribution. The function checks that what is quoted is valid R. More details on data generation is available in \code{?generate}, \code{?createData}, and \code{drawParam}. 

To simplify the most common case, popParam can take 1 value or distribution and create a matrix or vector that assigns that population parameter or distribution to all freed parameters.

These population values are used as starting values for analysis by default.
}
\item{misspec} {
Optional matrix or vector of identical dimension to the free matrix whose elements contain population parameter values for specifying misspecification. That is, elements of this matrix contain population parameters that are added to parameters that are fixed or have an existing population value. These parameters are wrapped in quotes, and can optionally be drawn from distributions as described above. Again, to simplify the most common case, misspec can take 1 value or distribution and create a matrix or vector that assigns that value or distribution to all previously specified fixed parameters. Details about misspecification are included the data generation functions.
}
\item{symmetric} {
Set as \code{TRUE} if the matrix created is symmetric (RPS/PS, RTE/TE). The function \code{binds} can also be used, which defaults to \code{symmetric = TRUE}
}
\value{
	\code{\linkS4class{SimMatrix}} object that used for model specification for analysis and data generation in simsem.
}
\author{
    Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu})
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		
		\item {See \code{\link{model}} To combine simMatrix objects into a complete data analysis and data generation template, which is a \code{\linkS4class{simsem}} object}
		\item See \code{\link{generate}} To generate data using the simsem template.
		\item See \code{\link{analyze}} To analyze real or generated data using the simsem template.
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
