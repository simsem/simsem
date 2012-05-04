\name{averageMisfit}
\alias{averageMisfit}
\title{
	Find parameter RMSEA
}
\description{
	The discrepancy value (chi-square value) divided by degree of freedom, which is equal to population RMSEA
}
\usage{
averageMisfit(observed.M, observed.CM, implied.M, implied.CM, df)
}
\arguments{
  \item{observed.M}{
	\code{\linkS4class{SimResult}} that includes at least one randomly varying parameter (e.g. sample size, percent missing, model parameters)
}
  \item{observed.CM}{
	Logical indicating if N varies over replications.
}
  \item{implied.M}{
	Logical indicating if the percentage of missing data that is MCAR varies over replications.
}
\item{implied.CM}{
	Logical indicating if the percentage of missing data that is MAR varies over replications.
}
\item{df}{
	Vector of parameters names that vary over replications.
}
}
\value{
	Average discrepancy value (population RMSEA; Equation 12 in Browne & Cudeck, 1992)
}
\references{
Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model fit. \emph{Sociological Methods & Research, 21}, 230-258.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
