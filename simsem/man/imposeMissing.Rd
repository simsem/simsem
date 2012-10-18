\name{imposeMissing}
\alias{imposeMissing}
\alias{impose}
\title{ 
Impose MAR, MCAR, planned missingness, or attrition on a data set
}
\description{
Function imposes missing values on a data based on the known missing data types,
including MCAR, MAR, planned, and attrition.
}
\usage{
impose(miss, data.mat, pmMCAR = NULL, pmMAR = NULL)
imposeMissing(data.mat, cov = 0, pmMCAR = 0, pmMAR = 0, nforms = 0, 
	itemGroups = list(), twoMethod = 0, prAttr = 0, timePoints = 1, 
	ignoreCols = 0, threshold = 0, logit = "", logical = NULL)
}
\arguments{
  \item{miss}{
    Missing data object (\code{\linkS4class{SimMissing}}) used as the template for impose missing values
}
  \item{data.mat}{
    Data to impose missing upon. Can be either a matrix or a data frame. 
}
  \item{cov}{
    Column indices of a covariate to be used to impose MAR missing, or
    MAR attrition. Will not be included in any removal procedure. See details.
}
  \item{pmMCAR}{
    Decimal percent of missingness to introduce completely at random on all variables.
}
  \item{pmMAR}{
    Decimal percent of missingness to introduce using the listed
    covariate as predictor. See details.
}
  \item{nforms}{
    The number of forms for planned missing data designs, not including
    the shared form.
}
  \item{itemGroups}{
    List of lists of item groupings for planned missing data
    forms. Unless specified, items will be divided into groups sequentially (e.g. 1-3,4-6,7-9,10-12)
}
  \item{twoMethod}{
    With missing on one variable: vector of (column index, percent missing). Will put a given percent
    missing on that column in the matrix to simulate a two method
    planned missing data research design. With missing on two or more variables: list of (column indices, percent missing).
  }
  \item{prAttr}{
    Probability (or vector of probabilities) of an entire case being removed due to attrition at a
    given time point. When a covariate is specified along with this argument, attrition
  will be predicted by the covariate (MAR attrition). See details.
  }
 \item{timePoints}{
    Number of timepoints items were measured over. For longitudinal
    data, planned missing designs will be implemented within each
    timepoint. All methods to impose missing values over time assume an equal number of
    variables at each time point.
}
 \item{ignoreCols}{
    The columns not imposed any missing values for any missing data patterns.
} 
 \item{threshold}{
    The threshold of the covariate used to impose missing values. Values
    on the covariate above this threshold are eligible to be
    deleted. The default threshold is the mean of the variable.
}
 \item{logit}{
    The script used for imposing missing values by logistic regression. The script is similar to the specification of regression in \code{lavaan} such that each line begins with a dependent variable, then '~' is used as regression sign, and the formula of a linear combination of independent variable plus constant, such as y1 ~ 0.5 + 0.2*y2. '#' and '!' can be used as a comment (like \code{lavaan}). For the intercept, users may use 'p()' to specify the average proportion of missing, such as y1 ~ p(0.2) + 0.3*y2, which the average missing proportion of y1 is 0.2 and the missing of y1 depends on y2. Users may visualize the missing proportion from the logistic specification by the \code{\link{plotLogitMiss}} function.
}
 \item{logical}{
    A matrix of logical values (\code{TRUE/FALSE}). If a value in the dataset is corresponding to the \code{TRUE} in the logical matrix, the value will be missing.
}
}
\details{
Without specifying any arguments, no missing values will be
introduced.

A single covariate is required to specify MAR missing - this
covariate can be distributed in any way. This covariate can be either continuous or categorical, as long as it is
numerical. If the covariate is categorical, the threshold should be
specified to one of the levels.

MAR missingness is specified using the threshold method - any value on the covariate that is above
the specified threshold indicates a row eligible for deletion. If the
specified total amount of MAR missingness is not possible given the
total rows eligible based on the threshold, the function iteratively
lowers the threshold until the total percent missing is possible.

Planned missingness is parameterized by the number of forms (n). This is used to divide the
cases into n groups. If the column groupings are not specified, a naive
method will be used that divides the columns into n+1 equal forms
sequentially (1-4,5-9,10-13..), where the first group is the shared
form.The first list of column indices given will be used as the shared
group. If this is not desired, this list can be left empty.

For attrition, the probability can be specified as a single value or as
a vector. For a single value, the probability of attrition will be
the same across time points, and affects only
cases not previously lost due to attrition. If this argument is a
vector, this specifies different probabilities of
attrition for each time point. Values will be recycled if this
vector is smaller than the specified number of time points.

An MNAR processes can be generated by specifying MAR missingness and then
dropping the covariate from the subsequent analysis.

Currently, if MAR missing is imposed along with attrition, both processes will use
the same covariate and threshold.

Currently, all types of missingness (MCAR, MAR, planned, and attrition)
are imposed independently. This means that specified global values of
percent missing will not be additive (10 percent MCAR + 10 percent MAR
does not equal 20 percent total missing). 
}
\value{
A data matrix with \code{NA}s introduced in the way specified by the arguments.
}
\author{
Patrick Miller(University of Kansas; \email{patr1ckm@ku.edu})
Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})  

}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimMissing}} for the alternative way to save missing data feature for using in the \code{\link{sim}} function.
		%\item \code{\link{runMI}} for imputing missing data by multiple imputation and analyze the imputed data.
	}
}
\examples{
  data <- matrix(rep(rnorm(10,1,1),19),ncol=19)
  datac <- cbind(data,rnorm(10,0,1),rnorm(10,5,5))
 
  # Imposing Missing with the following arguments produces no missing values
  imposeMissing(data)
  imposeMissing(data,cov=c(1,2))
  imposeMissing(data,pmMCAR=0)
  imposeMissing(data,pmMAR=0)
  imposeMissing(data,nforms=0)

  #Some more usage examples
  
  # No missing at variables 1 and 2
  imposeMissing(data,cov=c(1,2),pmMCAR=.1)
  
  # 3-Form design
  imposeMissing(data,nforms=3)
  
  # 3-Form design with specified groups of items (XABC)
  imposeMissing(data,nforms=3,itemGroups=list(c(1,2,3,4,5),c(6,7,8,9,10),c(11,12,13,14,15),c(16,17,18,19)))
  
  # 3-Form design when variables 20 and 21 are not missing
  imposeMissing(datac,cov=c(20,21),nforms=3)
  
  # 2 method design where the expensive measure is on Variable 19
  imposeMissing(data,twoMethod=c(19,.8))
  
  # Impose missing data with percent attrition of 0.1 in 5 time points
  imposeMissing(datac,cov=21,prAttr=.1,timePoints=5)

  # Logistic-regression MAR
  colnames(data) <- paste("y", 1:ncol(data), sep="")
  script <- 'y1 ~ 0.05 + 0.1*y2 + 0.3*y3
			y4 ~ -2 + 0.1*y4
			y5 ~ -0.5'
  imposeMissing(data, logit=script)
}
