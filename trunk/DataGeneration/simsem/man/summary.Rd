\name{summary}
\alias{summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary all simsem objects
%%  ~~function to do ... ~~
}
\description{
The function will summarize all information in desired objects
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    The name of desired object
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{
    Any additional arguments based on characteristics of each type of objects
%%     ~~Describe \code{\dots} here~~
}
}
\details{
    The function summarize all information that used specified inside particular object.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Nothing returned from this function.
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
        Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
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
    u1 <- runif.object(-0.1, 0.1)
    summary(u1)
    
    n02 <- rnorm.object(0, 0.2)
    summary(n02)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (object, ...) 
standardGeneric("summary"), generic = structure("summary", package = "base"), package = "base", group = list(), valueClass = character(0), signature = "object", default = structure(function (object, 
    ...) 
UseMethod("summary"), target = structure(character(0), .Names = character(0), class = structure("signature", package = "methods")), defined = structure(character(0), .Names = character(0), class = structure("signature", package = "methods")), generic = structure("summary", package = "base"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = structure(function (object, 
    ...) 
UseMethod("summary"), target = structure(character(0), .Names = character(0), class = structure("signature", package = "methods")), defined = structure(character(0), .Names = character(0), class = structure("signature", package = "methods")), generic = structure("summary", package = "base"), class = structure("derivedDefaultMethod", package = "methods"))(object, 
    ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
