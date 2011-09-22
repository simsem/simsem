\name{runif.object}
\alias{runif.object}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Create random uniform distribution object
}
\description{
    Create random uniform distribution object. Random uniform distribution object will save mean and standard deviation parameter. This will use in specifying parameters that distributed as normal distribution.
}
\usage{
runif.object(Lower, Upper)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Lower}{ Lower bound of the distribution
%%     ~~Describe \code{Lower} here~~
}
  \item{Upper}{ Upper bound of the distribution
%%     ~~Describe \code{Upper} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    \item{Runif} {Random Uniform Distribution object that save the specified parameters}
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
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
u1 <- runif(-0.1, 0.1)
run(u1)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Lower, Upper) {
	temp <- new("Runif", Lower=Lower, Upper=Upper)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ uniform distribution }
