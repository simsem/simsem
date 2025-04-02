\name{SimMissing-class}
\Rdversion{1.1}
\docType{class}
\alias{SimMissing-class}
\alias{summary,SimMissing-method}
\title{Class \code{"SimMissing"}}
\description{
Missing information imposing on the complete dataset
}
\section{Objects from the Class}{
Objects can be created by \code{\link{miss}} function.
}
\section{Slots}{
  \describe{
    \item{\code{cov}:}{Column indices of any normally distributed covariates used in the data set.}
    \item{\code{pmMCAR}:}{Decimal percent of missingness to introduce completely at random on all variables.}
    \item{\code{pmMAR}:}{Decimal percent of missingness to introduce using the listed covariates as predictors.}
	\item{\code{logit}:}{The script used for imposing missing values by logistic regression. See \code{\link{miss}} for further details.}
    \item{\code{nforms}:}{The number of forms for planned missing data designs, not including the shared form.}
    \item{\code{itemGroups}:}{List of lists of item groupings for planned missing data forms. Without this, items will be divided into groups sequentially (e.g. 1-3,4-6,7-9,10-12)}
    \item{\code{twoMethod}:}{Vector of (percent missing, column index). Will put a given percent
		missing on that column in the matrix to simulate a two method
		planned missing data research design. }
	\item{\code{prAttr}:}{Probability (or vector of probabilities) of an entire case being removed due to attrition at a given time point. See \code{\link{imposeMissing}} for further details.}
	\item{\code{m}:}{DEPRECATED. Formerly, the number of imputations (no longer supported). The only accepted value is now 0, indicating full information maximum likelihood is used for incomplete data.}
	\item{\code{package}:}{DEPRECATED. The package to be used in multiple imputation (no longer supported).}
  \item{\code{convergentCutoff}:}{DEPRECATED. If the proportion of convergent results across imputations are greater than the specified value (the default is 80\%), the analysis on the dataset is considered as convergent. Otherwise, the analysis is considered as nonconvergent. This attribute is applied for multiple imputation only.}
    \item{\code{timePoints}:}{Number of timepoints items were measured over. For longitudinal data, planned missing designs will be implemented within each timepoint.}
	\item{\code{ignoreCols}:}{The columns not imposed any missing values for any missing data patterns}
	\item{\code{threshold}:}{The threshold of covariates that divide between the area to impose missing and the area not to impose missing. The default threshold is the mean of the covariate.}
	\item{\code{covAsAux}:}{If \code{TRUE}, the covariate listed in the object will be used as auxiliary variables when putting in the model object. If \code{FALSE}, the covariate will be included in the analysis.}
	\item{\code{logical}:}{A matrix of logical values (\code{TRUE/FALSE}). If a value in the dataset is corresponding to the \code{TRUE} in the logical matrix, the value will be missing.}
	\item{\code{args}:}{A list of additional options to be passed to the multiple impuatation function in each package.}
}
}
\section{Methods}{
	\itemize{
		\item \code{\link{summary}} To summarize the object
		\item \code{\link{impose}} To impose missing information into data
	}
}
\author{
	Patrick Miller (University of Notre Dame; \email{pmille13@nd.edu})
	Alexander M. Schoemann (East Carolina University; \email{schoemanna@ecu.edu})
	Kyle Lang (University of Kansas; \email{kylelang@ku.edu})
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\link{imposeMissing}} for directly imposing missingness into a dataset.
	}
}
\examples{
misstemplate <- miss(pmMCAR=0.2)
summary(misstemplate)
}
\keyword{classes}
