\name{miPool}
\alias{miPool}
\title{
Function to pool imputed results
}
\description{
The function takes a list of imputed results (parameters and standard errors)
and returns pooled parameter estimates and standard errors.
}
\usage{
miPool(Result.l)
}
\arguments{
  \item{Result.l}{
List of SimModelOut used for pooling based on Rubin's rule.
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
		\item \code{\link{miPool}} for combining results in the \code{matrix} format.
		\item \code{\link{runMI}} for imputing missing values by multiple imputation and analyzing the imputed datasets.
	}
}
\examples{
# No Example
}

