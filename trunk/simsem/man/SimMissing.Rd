\name{SimMissing}
\alias{SimMissing}
\title{ 
Construct a siMissing object to create data with missingness and analyze missing data.
}
\description{
Function creates a simMissing object that can be passed to SimResult for creating and analyzing data with missingness..
}
\usage{
SimMissing(covs=0, pmMCAR=0, pmMAR=0, nforms=0, itemGroups=list(0), timePoints=1, twoMethod=0, numImps=0)
}
\arguments{
\item{\code{covs}:}{Column indices of any normally distributed covariates used in the data set.}
    \item{\code{pmMCAR}:}{Decimal percent of missingness to introduce completely at random on all variables.}
    \item{\code{pmMAR}:}{Decimal percent of missingness to introduce using the listed covariates as predictors.}
    \item{\code{nforms}:}{The number of forms for planned missing data designs, not including the shared form.}
    \item{\code{itemGroups}:}{List of lists of item groupings for planned missing data forms. Without this, items will be divided into groups sequentially (e.g. 1-3,4-6,7-9,10-12)}
    \item{\code{twoMethod}:}{Vector of (percent missing, column index). Will put a given percent missing on that column in the matrix to simulate a two method
		planned missing data research design. }
    \item{\code{timepoints}:}{Number of timepoints items were measured over. For longitudinal data, planned missing designs will be implemented within each timepoint.}
    \item{\code{numImps}:}{The number of imputations to be used when multiply imputing missing data. Setting numImps to 0 will use FIML to handle missing data.}
}
\details{
Without specifying any any arguments, no missingness will be
introduced. Covariates are required to impose MAR missing. Imputations will be performed with Amelia
}
\value{
A simMissing object to be used with SimResult.
}
\author{
Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})  
Patrick Miller(University of Kansas; \email{patr1ckm@ku.edu})
Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimMissing}} for the alternative way to save missing data feature for using in the \code{\link{simResult}} function.
		\item \code{\link{runMI}} for imputing missing data by multiple imputation and analyze the imputed data.
	}
}
\examples{
  #Example to create simMissing object for 3 forms design at 3 timepoints with 10 imputations
  Missing<- SimMissing(nforms=3, timePoints=3, numImps=10)

}
