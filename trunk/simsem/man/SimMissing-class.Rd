\name{SimMissing-class}
\Rdversion{1.1}
\docType{class}
\alias{SimMissing-class}
\alias{summary,SimMissing-method}
\alias{run,SimMissing-method}
\title{Class \code{"SimMissing"}}
\description{
Missing information imposing on the complete dataset
}
\section{Objects from the Class}{
Objects can be created by \code{simMissing} function. It can also be called from the form \code{new("SimMissing", ...)}.
}
\section{Slots}{
  \describe{
  
    \item{\code{cov}:}{Column indices of any normally distributed covariates used in the data set.}
    \item{\code{pmMCAR}:}{Decimal percent of missingness to introduce completely at random on all variables.}
    \item{\code{pmMAR}:}{Decimal percent of missingness to introduce using the listed covariates as predictors.}
    \item{\code{nforms}:}{The number of forms for planned missing data designs, not including the shared form.}
    \item{\code{itemGroups}:}{List of lists of item groupings for planned missing data forms. Without this, items will be divided into groups sequentially (e.g. 1-3,4-6,7-9,10-12)}
    \item{\code{twoMethod}:}{Vector of (percent missing, column index). Will put a given percent
		missing on that column in the matrix to simulate a two method
		planned missing data research design. }
    \item{\code{timePoints}:}{Number of timepoints items were measured over. For longitudinal data, planned missing designs will be implemented within each timepoint.}
    \item{\code{numImps}:}{The number of imputations to be used when multiply imputing missing data. Setting numImps to 0 will use FIML to handle missing data.}
    \item{\code{impMethod}:}{Package that will be used for imputation. Currently only Amelia is supported.}
	\item{\code{ignoreCols}:}{The columns not imposed any missing values for any missing data patterns} 
	\item{\code{threshold}:}{The threshold of covariates that divide between the area to impose missing and the area not to impose missing. The default threshold is the mean of the covariate.}
	}  
}
\section{Methods}{
	\itemize{
		\item \code{\link{summary}} To summarize the object
		\item \code{\link{run}} To impose missing information into data
	}
}
\author{
	Patrick Miller(University of Kansas; \email{patr1ckm@ku.edu})
	Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})  
	Kyle Lang (University of Kansas; \email{kylelang@ku.edu})
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{imposeMissing}} for directly imposing missingness into a dataset.
	}
}
\examples{
# No Example
}
\keyword{classes}
