\name{checkInputValue}
\alias{checkInputValue}
\alias{checkInputValueVector}
\title{
	Check the value argument in the matrix, symmetric matrix, or vector objects
}
\description{
	Check the value argument in the matrix, symmetric matrix, or vector objects constructors (\code{simMatrix}, \code{symMatrix}, and \code{simVector}) and provide back the appropriate text, which can be evaluated later directly. The \code{checkInputValue} function is used for a single value. The \code{checkInputValueVector} function is used for multiple values.
}
\usage{
checkInputValue(txt)
checkInputValueVector(txt)
}
\arguments{
  \item{txt}{
	The value that is fed in the constructors
}
}
\value{
	The appropriate text of code that can be evaluated later
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	The other functions that used this function: \code{simMatrix}, \code{symMatrix}, \code{simVector}, and \code{adjust}
}
\examples{
# No example
}
