\name{imposeMissing}
\alias{imposeMissing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Impose MAR, MCAR, and planned missingness on a data set

}
\description{
Function imposes missing values on a data based on the known missing data types,
including MCAR, MAR, and planned.
}
\usage{
imposeMissing(data.mat, covs = NULL, pmMCAR = NULL, pmMAR = NULL, nforms = NULL, itemGroups = NULL, twoMethod = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.mat}{
    A raw data matrix
}
  \item{covs}{
    Column indices of any normally distributed covariates used in the data set. Will not be
    included in any removal procedure, and are required to impose MAR missing. 
}
  \item{pmMCAR}{
    Decimal percent of missingness to introduce completely at random on all variables.
}
  \item{pmMAR}{
    Decimal percent of missingness to introduce using the listed
    covariates as predictors. 
}
  \item{nforms}{
    The number of forms for planned missing data designs, not including
    the shared form.
}
  \item{itemGroups}{
    List of lists of item groupings for planned missing data forms. Without this,
    items will be divided into groups sequentially (e.g. 1-3,4-6,7-9,10-12)
}
  \item{twoMethod}{
    Vector of (percent missing, column index). Will put a given percent
    missing on that column in the matrix to simulate a two method
    planned missing data research design. 
}
}
\details{
Without specifying any any arguments, no missingness will be
introduced. Covariates are required to impose MAR missing.
}
\value{
A data matrix with NAs introduced in the way specified by the arguments.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick Miller(University of Kansas; \email{patr1ckm@ku.edu})
Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})  
Kyle Lang (University of Kansas; \email{kylelang@ku.edu})
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  data <- matrix(rep(rnorm(10,1,1),19),ncol=19)
  datac <- cbind(data,rnorm(10,0,1),rnorm(10,5,5))

  # Imposing Missing with the following arguments produces no missing values
  imposeMissing(data)
  imposeMissing(data,covs=c(1,2))
  imposeMissing(data,pmMCAR=0)
  imposeMissing(data,pmMAR=0)
  imposeMissing(data,nforms=0)

  #Some more usage examples
  imposeMissing(data,covs=c(1,2),pmMCAR=.1)
  imposeMissing(datac,covs=c(20,21),pmMAR=.2)
  imposeMissing(data,nforms=3)
  imposeMissing(data,nforms=3,itemGroups=list(c(1,2,3,4,5),c(6,7,8,9,10),c(11,12,13,14,15),c(16,17,18,19)))
  imposeMissing(datac,covs=c(20,21),nforms=3)
  imposeMissing(data,twoMethod=c(19,.8))
  imposeMissing(datac,covs=c(20,21),pmMCAR=.1,pmMAR=.1,nforms=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
