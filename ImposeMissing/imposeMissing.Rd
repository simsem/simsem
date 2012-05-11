\name{imposeMissing}
\alias{imposeMissing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
imposeMissing(data.mat, covs = NULL, pmMCAR = 0, pmMAR = 0, nforms = NULL, itemGroups = NULL, twoMethod = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.mat}{
%%     ~~Describe \code{data.mat} here~~
}
  \item{covs}{
%%     ~~Describe \code{covs} here~~
}
  \item{pmMCAR}{
%%     ~~Describe \code{pmMCAR} here~~
}
  \item{pmMAR}{
%%     ~~Describe \code{pmMAR} here~~
}
  \item{nforms}{
%%     ~~Describe \code{nforms} here~~
}
  \item{itemGroups}{
%%     ~~Describe \code{itemGroups} here~~
}
  \item{twoMethod}{
%%     ~~Describe \code{twoMethod} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function(data.mat,covs=NULL,pmMCAR=0,pmMAR=0,nforms=NULL,itemGroups=NULL,twoMethod=NULL){

 # TRUE values are values to delete
 if(!is.null(nforms) || !is.null(twoMethod)){ 
 log.matpl <- planned.missing(dim(data.mat),covs,nforms=nforms,twoMethod=twoMethod,itemGroups=itemGroups)
  data.mat[log.matpl] <- NA
 }

 # Impose MAR and MCAR
 
 log.mat1 <- makeMCAR(dim(data.mat),pmMCAR,covs)
 
 log.mat2 <- makeMAR(data.mat,pmMAR,covs)


  data.mat[log.mat1] <- NA
  data.mat[log.mat2] <- NA
 
 return(data.mat) 
 
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
