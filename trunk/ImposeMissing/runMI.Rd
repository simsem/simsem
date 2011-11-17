\name{runMI}
\alias{runMI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiply impute and analyze data using lavaan
}
\description{
This function takes data with missing observations, multiple imputes the data, runs a SEM using lavaan and combines the results using Rubin's rules.
}
\usage{
runMI(data.mat, data.model, imps, miPackage="amelia")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.mat}{
Data frame with missing observations. 
}
  \item{data.model}{
Specification of the model to be analyzed. data.model can be either a simModel object or lavaan syntax
}
  \item{imps}{
Number of imputations wanted
}
 \item{miPackage}{
Package to be used for impuation. Currently runMI only uses amelia for imputation
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
runMI returns a list with pooled estimates, standard errors, fit indices and fraction missing information
\item{coef}{Pooled parameter estimates. The order of parameter estimates corresponds to the order reported by Lavaan}
\item{se}{Pooled standard errors. The order of standard errors corresponds to the order reported by Lavaan}
\item{fit}{Pooled fit indices. The order of fit indices corresponds to the order reported by Lavaan}
\item{FMI.1}{Fraction of missing information for each parameter. The order of fraction missing corresponds to the order of parameters reported by Lavaan}
\item{FMI.2}{Fraction of missing information for each parameter. The order of fraction missing corresponds to the order of parameters reported by Lavaan}
%% ...
}
\references{
Rubin, D.B. (1987) Multiple Imputation for Nonresponse in Surveys. J. Wiley & Sons, New York.
}
Patrick Miller & Alexander M. Schoemann
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
function(data.mat,data.model,imps) {
  #Impute missing data
  imputed.l<-imputeMissing(data.mat,imps)
  
  #Run models on each imputed data set
  #Does this give results from each dataset in the list?
  
  # imputed.results<-result.object(imputed.l[[1]],sim.data.model,10)

  imputed.results <- lapply(imputed.l,result.object,data.model,1)
  comb.results<-MIpool(imputed.results,imps)
  
  return(comb.results)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
