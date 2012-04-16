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
\value{
MIpool returns a list with pooled estimates, standard errors, fit indices and fraction missing information
\item{Estimates}{Pooled parameter estimates. }
\item{SE}{Pooled standard errors. }
\item{FMI.1}{Fraction of missing information for each parameter. }
\item{FMI.2}{Fraction of missing information for each parameter. }
}
\details{
All parameter estimates are pooled by Rubin's (1987) rule. The chi-square statistics are pooled by the procedure proposed by Li, Meng, Raghunathan, and Rubin (1991; Equations 2.1, 2.2, 2.16, and 2.17). The resulting value from the pooled chi-square is F-statistic. The F-statistics is multiplied by the numerator degree of feedom to provide the value equivalent to chi-square value. This chi-square value will be used to find other chi-squared related fit indices: RMSEA, CFI, and TLI. SRMR, AIC, and BIC are pooled by Rubin's (1987) rule. The function for pooling chi-squared statistics is adapted from Craig Enders' SAS code from \url{"http://psychology.clas.asu.edu/files/CombiningLikelihoodRatioChi-SquareStatisticsFromaMIAnalysis.sas"}.
}
\references{
Rubin, D.B. (1987) \emph{Multiple Imputation for Nonresponse in Surveys.} J. Wiley & Sons, New York.
Li, K. H., Meng, X. L., Raghunathan, T. E., & Rubin, D. B. (1991). Significance levels from repeated p-values with multiply-imputed data. \emph{Statistica Sinica, 1,} 65-92.
}
\author{
Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
Mijke Rhemtulla (University of Kansas; \email{mijke@ku.edu})
Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{miPoolVector}} for pooling multiple imputation results that providing in matrix or vector formats
		\item \code{\link{miPoolChi}} for pooling multiple imputated chi-square statistics
		\item \code{\link{runMI}} for imputing missing values by multiple imputation and analyzing the imputed datasets.
	}
}
\examples{
# No Example
}

