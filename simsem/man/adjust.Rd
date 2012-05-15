\name{adjust}
\alias{adjust}
\alias{adjust-methods}
\alias{adjust,ANY-method}
\alias{adjust,SimMatrix-method}
\alias{adjust,SymMatrix-method}
\alias{adjust,SimVector-method}
\title{
	Change an element in \code{SimMatrix}, \code{SymMatrix}, or \code{SimVector}.
}
\description{
	This function will adjust an element in \code{\linkS4class{SimMatrix}}, \code{\linkS4class{SymMatrix}}, or \code{\linkS4class{SimVector}}. 
	The specified element may be set to be free parameter with number or distribution object as starting values. 
	Alternatively, the element can be fixed to be a value (such as 0).
}
\usage{
adjust(target, value, pos, numAsFixed)
}
\arguments{
  \item{target}{
	Target \code{\linkS4class{SimMatrix}}, \code{\linkS4class{SymMatrix}}, or \code{\linkS4class{SimVector}} that you would like to adjust.
}
  \item{value}{
	The name of \link[=VirtualDist-class]{distribution object} that you would like to specify (put as \strong{character} with single or double quotation) 
	or number that represents fixed values or starting values.
}
  \item{pos}{
	The position of element that you would like to adjust, such as \code{"c(1,2)"} for the element in Row 1 and Column 2 in the specified matrix.
}
  \item{numAsFixed}{ 
	This argument is used when the \code{VirtualDist} argument was specified as number. 
	If \code{TRUE} (as default), the number is treated as fixed parameters. 
	If \code{FALSE}, the number is treated as a starting value and the element is set to be free parameter.
}
}
\value{
	Return the input \code{\linkS4class{SimMatrix}}, \code{\linkS4class{SymMatrix}}, or \code{\linkS4class{SimVector}} with adjusted element.
}
\note{
	For \code{\linkS4class{SymMatrix}} class, above- and below-diagonal elements will be adjusted simultaneously. 
	Either above- or below-diagonal element is specified in the \code{pos} argument.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimMatrix}} for random parameter matrix
		\item \code{\linkS4class{SymMatrix}} for symmetric random parameter matrix
		\item \code{\linkS4class{SimVector}} for random parameter vector
	}
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)
summary(LX)
run(LX)

u34 <- simUnif(0.3, 0.4)
LX <- adjust(LX, "u34", c(2, 1))
summary(LX)
run(LX)

LX <- adjust(LX, 0, c(2,1))
LX <- adjust(LX, 0.5, c(2,2), FALSE)
summary(LX)
run(LX)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- simVector(factor.mean, factor.mean.starting)
run(AL)
summary(AL)

n01 <- simNorm(0, 1)
AL <- adjust(AL, "n01", 2)
run(AL)
summary(AL)
}

