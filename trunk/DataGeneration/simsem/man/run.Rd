\name{run}
\alias{run}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a particular object in simsem package.
%%  ~~function to do ... ~~
}
\description{
Run a particular object such as running any distribution objects to create number.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    \sQuote{simsem} object
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{
    any additional arguments
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    \item{object}{depends on particular object}
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
n02 <- rnorm.object(0, 0.2)
run(n02)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ run }