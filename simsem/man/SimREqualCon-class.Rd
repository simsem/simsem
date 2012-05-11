\name{SimREqualCon-class}
\Rdversion{1.1}
\docType{class}
\alias{SimREqualCon-class}
\alias{summary,SimREqualCon-method}

\title{Class \code{"SimREqualCon"}}
\description{
Set of specified equality constraints using the matrix and vector of parameters notation
}
\section{Objects from the Class}{
Set of specified equality constraints using the matrix and vector of parameters notation (e.g., no \code{VTE} or \code{ME})
}
\section{Slots}{
  \describe{
    \item{\code{con}:}{List of equality constraint. Each element in the list is an individual equality constraint saved in a matrix.
			Each row represents each element. If the matrix has two columns, the first column indicates row of the element and 
			the second column indicates column of the element. If the matrix has three columns, the first column is the group
			of matrix. The rest is row and column. Row name represents the matrix that the element is in. The definition of row
			name can be seen in VirtualRSet definition.}
    \item{\code{modelType}:}{Analysis model (CFA, SEM, Path) }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{Summarize all attributes of this object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimEqualCon}}
	}
}
\examples{
# No example
}
\keyword{classes}
