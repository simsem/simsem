\name{simMissing}
\alias{simMissing}
\title{ 
Construct a SimMissing object to create data with missingness and analyze missing data.
}
\description{
Function creates a \linkS4class{SimMissing} object that can be passed to \link{simResult} for creating and analyzing data with missingness.
}
\usage{
simMissing(cov=0, pmMCAR=0, pmMAR=0, nforms=0, itemGroups=list(0), timePoints=1, twoMethod=0, prAttr=0, numImps=0, ignoreCols=0, threshold=0, covAsAux=TRUE, logical=new("NullMatrix"), ...)
}
\arguments{
	\item{cov}{Column indices of any normally distributed covariates used in the data set.}
    \item{pmMCAR}{Decimal percent of missingness to introduce completely at random on all variables.}
    \item{pmMAR}{Decimal percent of missingness to introduce using the listed covariates as predictors.}
    \item{nforms}{The number of forms for planned missing data designs, not including the shared form.}
    \item{itemGroups}{List of lists of item groupings for planned missing data forms. Without this, items will be divided into groups sequentially (e.g. 1-3,4-6,7-9,10-12)}
    \item{twoMethod}{Vector of (percent missing, column index). Will put a given percent missing on that column in the matrix to simulate a two method
		planned missing data research design. }
	\item{prAttr}{Probability (or vector of probabilities) of an entire case being removed due to attrition at a given time point. See \code{\link{imposeMissing}} for further details.}
    \item{timePoints}{Number of timepoints items were measured over. For longitudinal data, planned missing designs will be implemented within each timepoint.}
    \item{numImps}{The number of imputations to be used when multiply imputing missing data. Setting numImps to 0 will use FIML to handle missing data.}
	\item{ignoreCols}{The columns not imposed any missing values for any missing data patterns} 
	\item{threshold}{The threshold of covariates that divide between the area to impose missing and the area not to impose missing. The default threshold is the mean of the covariate.}
	\item{covAsAux}{If \code{TRUE}, the covariate listed in the object will be used as auxiliary variables when putting in the model object. If \code{FALSE}, the covariate will be included in the analysis.}
	\item{logical}{A matrix of logical values (\code{TRUE/FALSE}). If a value in the dataset is corresponding to the \code{TRUE} in the logical matrix, the value will be missing.}
	\item{\dots}{Additional arguments to be passed to \code{\link[Amelia]{amelia}} for imputation. Only used if \code{numImps} is greater than 0.}
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
	#Example of imposing 10% MCAR missing in all variables with no imputations (FIML method)
	Missing <- simMissing(pmMCAR=0.1)
	summary(Missing)
	
	loading <- matrix(0, 6, 1)
	loading[1:6, 1] <- NA
	LX <- simMatrix(loading, 0.7)
	RPH <- symMatrix(diag(1))
	RTD <- symMatrix(diag(6))
	CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
	SimData <- simData(CFA.Model, 500)
	SimModel <- simModel(CFA.Model)
	
	#Create data
	dat <- run(SimData)
	
	#Impose missing
	dat <- run(Missing, dat)
	
	#Analyze data
	out <- run(SimModel, dat)
	summary(out)
	
	#Example to create simMissing object for 3 forms design at 3 timepoints with 10 imputations
	Missing <- simMissing(nforms=3, timePoints=3, numImps=10)

}
