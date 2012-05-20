\name{residualCovariate}
\alias{residualCovariate}
\title{
	Residual centered all target indicators by covariates
}
\description{
This function will regress target variables on the covariate and replace the target variables by the residual of the regression analysis. This procedure is useful to control the covariate from the analysis model (Geldhof, Pornprasertmanit, Schoemann, & Little, in press).
}
\usage{
residualCovariate(data, targetVar, covVar)
}
\arguments{
  \item{data}{
	The desired data to be transformed.
}
  \item{targetVar}{
	Varible names or the position of indicators that users wish to be residual centered (as dependent variables)
}
  \item{covVar}{
	Covariate names or the position of the covariates using for residual centering (as independent variables) onto target variables
}
}
\value{
	The data that the target variables replaced by the residuals
}
\references{
	\itemize{
		\item Geldhof, G. J., Pornprasertmanit, S., Schoemann, A., & Little, T. D. (in press). Orthogonalizing through residual centering: Applications and caveats. \emph{Educational and Psychological Measurement.}
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{simFunction}} to use this function within a simulation study
	}
}
\examples{
dat <- residualCovariate(attitude, 2:7, 1)
}
