\name{rnorm.object}
\alias{rnorm.object}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Create random normal distribution object
%%  ~~function to do ... ~~
}
\description{
    Create random normal distribution object. Random normal distribution object will save mean and standard deviation parameter. This will use in specifying parameters that distributed as normal distribution.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rnorm.object(Mean, SD)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mean}{
    Desired population mean
%%     ~~Describe \code{Mean} here~~
}
  \item{SD}{
    Desired population standard deviation
%%     ~~Describe \code{SD} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    \item{Rnorm} {Random Normal Distribution object that save the specified parameters}
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
    n02 <- rnorm.object(0, 0.2)
    run(n02)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
