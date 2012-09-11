\name{miss}
\alias{miss}
\title{ 
Specifying the missing template to impose on a dataset
}
\description{
Specifying the missing template (\code{\linkS4class{SimMissing}}) to impose on a dataset. The template will be used in Monte Carlo simulation such that, in the \code{\link{sim}} function, datasets are created and imposed by missing values created by this template. See \code{\link{imposeMissing}} for further details of each argument.
}
\usage{
miss(cov = 0, pmMCAR = 0, pmMAR = 0, nforms = 0, itemGroups = list(), 
    timePoints = 1, twoMethod = 0, prAttr = 0, m = 0, chi = "all", 
	package = "default", convergentCutoff = 0.8, ignoreCols = 0, 
    threshold = 0, covAsAux = TRUE, logical = NULL, ...)
}
\arguments{
	\item{cov}{Column indices of any normally distributed covariates used in the data set.}
    \item{pmMCAR}{Decimal percent of missingness to introduce completely at random on all variables.}
    \item{pmMAR}{Decimal percent of missingness to introduce using the listed covariates as predictors.}
    \item{nforms}{The number of forms for planned missing data designs, not including the shared form.}
    \item{itemGroups}{List of lists of item groupings for planned missing data forms. Without this, items will be divided into groups sequentially (e.g. 1-3,4-6,7-9,10-12)}
    \item{timePoints}{Number of timepoints items were measured over. For longitudinal data, planned missing designs will be implemented within each timepoint.}
    \item{twoMethod}{With missing on one variable: vector of (column index, percent missing). Will put a given percent missing on that column in the matrix to simulate a two method planned missing data research design. With missing on two or more variables: list of (column indices, percent missing). }
	\item{prAttr}{Probability (or vector of probabilities) of an entire case being removed due to attrition at a given time point. See \code{\link{imposeMissing}} for further details.}
    \item{m}{The number of imputations. The default is 0 such that the full information maximum likelihood is used.}
    \item{chi}{The chi-square pooling method. See \code{\link[semTools]{runMI}} function for further details.}
    \item{package}{The package to be used in multiple imputation. The default value of this function is \code{"default"}. For the default option, if \code{m} is 0, the full information maximum likelihood is used. If \code{m} is greater than 0, the Amelia package is used. The possible inputs are \code{"default"}, \code{"Amelia"}, or \code{"mice"}.}
    \item{convergentCutoff}{If the proportion of convergent results across imputations are greater than the specified value (the default is 80\%), the analysis on the dataset is considered as convergent. Otherwise, the analysis is considered as nonconvergent. This attribute is applied for multiple imputation only. }
	\item{ignoreCols}{The columns not imposed any missing values for any missing data patterns} 
	\item{threshold}{The threshold of covariates that divide between the area to impose missing and the area not to impose missing. The default threshold is the mean of the covariate.}
	\item{covAsAux}{If \code{TRUE}, the covariate listed in the object will be used as auxiliary variables when putting in the model object. If \code{FALSE}, the covariate will be included in the analysis.}
	\item{logical}{A matrix of logical values (\code{TRUE/FALSE}). If a value in the dataset is corresponding to the \code{TRUE} in the logical matrix, the value will be missing.}
	\item{\dots}{Additional arguments used in multiple imputation function. This feature currently does not work now.}
}
\value{
	A missing object that contains missing-data template (\code{\linkS4class{SimMissing}})
}
\author{
	Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu}), 
    Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu}), 
	Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimMissing}} The resulting missing object
	}
}
\examples{
#Example of imposing 10% MCAR missing in all variables with no imputations (FIML method)
Missing <- miss(pmMCAR=0.1, ignoreCols="group")
summary(Missing)

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

#Create data
dat <- generate(CFA.Model, n = 20)

#Impose missing
dat <- impose(Missing, dat)

#Analyze data
out <- analyze(CFA.Model, dat)
summary(out)

#Example to create simMissing object for 3 forms design at 3 timepoints with 10 imputations
Missing <- miss(nforms=3, timePoints=3, numImps=10)

#Missing template for data analysis with multiple imputation
Missing <- miss(package="mice", m=10, chi="all", convergentCutoff=0.6)
}
