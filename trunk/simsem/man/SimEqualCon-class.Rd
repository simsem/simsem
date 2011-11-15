\name{SimEqualCon-class}
\Rdversion{1.1}
\docType{class}
\alias{SimEqualCon-class}
\alias{summary,SimEqualCon-method}

\title{Class \code{"SimEqualCon"}}
\description{
Set of specified equality constraints
}
\section{Objects from the Class}{
Objects can be created by \code{\link{simEqualCon}}. Also, it can be called of the form \code{new("SimEqualCon", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{con}:}{List of equality constraint. See the \code{Details} section for the description of each equality constraint. }
    \item{\code{modelType}:}{Analysis model (CFA, SEM, Path) }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{Summarize all attributes of this object }
	 }
}
\details{
	The \code{Equality} slot contains list of equality constraint. Each element in the list is an individual equality constraint saved in a matrix. 
	Each row represents each element. If the matrix has two columns, the first column indicates row of the element and 
	the second column indicates column of the element. If the matrix has three columns, the first column is the group
	of matrix. The rest is row and column. Row name represents the matrix that the element is in. The definition of row
	name can be seen in simSetCFA, simSetPath, or simSetSEM, depending on analysis model you specify.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\link{simEqualCon}} for the constructor of this class
		\item \code{\link{simData}} for a potential use of this object to create data
		\item \code{\link{simModel}} for a potential use of this object to run an analysis
	}
}
\examples{
showClass("SimEqualCon")
constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM")
summary(equal.loading)
}
\keyword{classes}
