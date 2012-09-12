\name{miPoolVector}
\alias{miPoolVector}
\title{
Function to pool imputed results that saved in a matrix format
}
\description{
The function takes parameter estimates and standard errors of each imputed result and returns pooled parameter estimates and standard errors.
}
\usage{
miPoolVector(MI.param, MI.se, imps)
}
\arguments{
  \item{MI.param}{
A matrix of parameter estimates that the row represents parameter estimates from different imputations and the column represents parameter estimates of different target parameters.
}
  \item{MI.se}{
A matrix of standard errors that the row represents standard errors from different imputations and the column represents the standard errors of different target parameters.
}
  \item{imps}{
The number of imputations
}
}
\details{
Parameters and standard errors are combined using Rubin's Rules (Rubin, 1987). 
}
\value{
MIpool returns a list with pooled estimates, standard errors, fit indices and fraction missing information
\item{Estimates}{Pooled parameter estimates. }
\item{SE}{Pooled standard errors. }
\item{FMI.1}{Fraction of missing information for each parameter. }
\item{FMI.2}{Fraction of missing information for each parameter. }
}
\references{
Rubin, D.B. (1987) Multiple Imputation for Nonresponse in Surveys. J. Wiley & Sons, New York.
}
\author{
Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
Mijke Rhemtulla (University of Kansas; \email{mijke@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{runMI}} for imputing missing values by multiple imputation and analyzing the imputed datasets.
		\item \code{\link{miPool}} for combining results in the \code{\linkS4class{SimModelOut}} format.
	}
}
\examples{
param <- matrix(c(0.7, 0.1, 0.5,
					0.75, 0.12, 0.54,
					0.66, 0.11, 0.56,
					0.74, 0.09, 0.55), nrow=4, byrow=TRUE)
SE <- matrix(c(0.1, 0.01, 0.05,
				0.11, 0.023, 0.055,
				0.10, 0.005, 0.04,
				0.14, 0.012, 0.039), nrow=4, byrow=TRUE)
nimps <- 4
miPoolVector(param, SE, nimps)
}

