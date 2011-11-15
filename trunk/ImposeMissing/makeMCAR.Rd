\name{makeMCAR}
\alias{makeMCAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to make Missing Completely at Random (MCAR) missing data

}
\description{
Function to make Missing Completely at Random (MCAR) missing data
}
\usage{
makeMCAR(dims, pm, covs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dims}{
Dimensions of raw data. This is a two items list with the number of rows followed by the number of columns
WE NEED TO MAKE THIS TAKE THE RAW DATA INSTEAD FOR COMPATABILITY
}
  \item{pm}{
Percent of missing data wanted
}
  \item{covs}{
Indices of covariates (column numbers corresponding to covariates). Covariates will not have any missing data
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Logical matrix of items to be deleted. Values marked TRUE will be deleted.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Kyle?
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(dims,pm,covs)
  {
    ## Fills a pattern matrix (R) of the same dimensions as your data with a bunch of binomially distributed (p=pm) TRUEs or FALSEs
       ## Provides an R matrix with a proportion of ones = proportion missing (all iid binomial)
    R <- matrix(as.logical(rbinom(n=dims[2]*dims[1],size=1,prob=pm)),dims[1],dims[2],byrow=TRUE)

    # Preserve covariates
    R[,covs] <- FALSE
    
    return(R)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
