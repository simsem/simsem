\name{findPower}
\alias{findPower}
\title{
	Find a value of independent variables that provides a given value of power. 
}
\description{
	Find a value of independent variable that provides a given value of power. If there are more than one varying parameters, this function will find the value of the target varying parameters given the values of the other varying parameters.
}
\usage{
findPower(powerTable, iv, power)
}
\arguments{
\item{powerTable}{
	A \code{data.frame} providing varying parameters and powers of each parameter. This table is obtained by \code{\link{getPower}} or \code{\link{continuousPower}} function.
}
  \item{iv}{
	The target varying parameter that users would like to find the value providing a given power from. This argument can be specified as the index of the target column or the name of target column (i.e., \code{"iv.N"} or \code{"N"})
}
  \item{power}{
	A desired power.
}
}
\value{
	There are five possible types of values provided:
	\itemize{
		\item \emph{Value} The varying parameter value that provides the power just over the specified power value (the adjacent value of varying parameter provides lower power than the specified power value).
		\item \emph{Minimum value} The minimum value has already provided enough power (way over the specified power value). The value of varying parameters that provides exact desired power may be lower than the minimum value. The example of varying parameter that can provides the minimum value is sample size.
		\item \emph{Maximum value} The maximum value has already provided enough power (way over the specified power value). The value of varying parameters that provides exact desired power may be higher than the maximum value. The example of varying parameter that can provides the maximum value is percent missing.
		\item \code{NA} There is no value in the domain of varying parameters that provides the power greater than the desired power.
		\item \code{Inf} The power of all values in the varying parameters is 1 (specifically more than 0.9999) and any values of the varying parameters can be picked and still provide enough power.
	}
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item \code{\link{getPower}} to find the power of parameter estimates		
		\item \code{\link{continuousPower}} to find the power of parameter estimates for the result object (\code{linkS4class{SimResult}}) with varying parameters.
	}
}
\examples{
\dontrun{
# Specify Sample Size by n
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.4)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# Specify both sample size and percent missing completely at random. Note that more fine-grained 
# values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) and pmMCAR=seq(0, 0.2, 0.01)
Output <- sim(NULL, model=CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))

# Find the power of all possible combination of N and pmMCAR
pow <- getPower(Output)

# Find the sample size that provides the power of 0.8
findPower(pow, "N", 0.80)
}
}
