\name{getKeywords}
\alias{getKeywords}
\title{
	List of all keywords used in the \code{simsem} package
}
\description{
	List of all keywords used in the \code{simsem} package
}
\usage{
getKeywords()
}
\value{
	A list of all keywords used in this package
	\itemize{
		\item \code{LY} Factor loading matrix between endogenous factors and Y indicators
		\item \code{TE} Covariance matrix between Y measurement error
		\item \code{RTE} Correlation matrix between Y measurement error
		\item \code{PS} Residual covariance of endogenous factors
		\item \code{RPS} Residual correlation of endogenous factors
		\item \code{BE} Regression effect among endogenous factors
		\item \code{VY} Total variance of Y indicators
		\item \code{VPS} Residual variances of endogenous factors
		\item \code{VE} Total variance of endogenous factors
		\item \code{TY} Measurement intercepts of Y indicators
		\item \code{ME} Factor means of endogenous factors
		\item \code{VTE} Variance of Y measurement error
		\item \code{AL} Factor intercepts of endogenous factors
		\item \code{MY} Total Mean of Y indicators
		\item \code{LX} Factor loading matrix between exogenous factors and X indicators
		\item \code{TD} Covariance matrix between X measurement error
		\item \code{RTD} Correlation matrix between X measurement error
		\item \code{PH} Covariance among exogenous factors
		\item \code{RPH} Correlation among exogenous factors
		\item \code{GA} Regression effect from exogenous factors to endogenous factors	
		\item \code{VX} Total variance of X indicators
		\item \code{VPH} Variance of exogenous factors
		\item \code{TX} Measurement intercepts of X indicators
		\item \code{KA} Factor Mean of exogenous factors
		\item \code{VTD} Variance of X measurement error
		\item \code{MX} Total Mean of X indicators
		\item \code{TH} Measurement error covariance between X indicators and Y indicators
		\item \code{RTH} Measurement error correlation between X indicators and Y indicators
		\item \code{loading} Any factor loading matrix keywords
		\item \code{errorCov} Any measurement error covariance matrix keywords
		\item \code{errorCor} Any measurement error correlation matrix keywords
		\item \code{errorVar} Any measurement error variance vector keywords
		\item \code{indicatorVar} Any indicator variance vector keywords
		\item \code{indicatorMean} Any indicator mean vector keywords
		\item \code{facCov} Any factor covariance matrix keywords
		\item \code{facCor} Any factor correlation matrix keywords
		\item \code{intercept} Any intercept vector keywords 
		\item \code{facMean} Any factor mean vector keywords
		\item \code{facVar} Any factor variance vector keywords
		\item \code{usedFit} Fit indices used as the default for providing output
		\item \code{usedFitPop} Population fit indices used as the default for providing input
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# This function is not a public function.

# getKeywords()
}
