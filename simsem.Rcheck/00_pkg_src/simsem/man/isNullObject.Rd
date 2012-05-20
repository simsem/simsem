\name{isNullObject}
\alias{isNullObject}
\alias{isNullObject-methods}
\alias{isNullObject,ANY,ANY-method}
\alias{isNullObject,vector-method}
\alias{isNullObject,matrix-method}
\alias{isNullObject,SimMatrix-method}
\alias{isNullObject,SymMatrix-method}
\alias{isNullObject,SimVector-method}
\alias{isNullObject,SimSet-method}
\alias{isNullObject,SimEqualCon-method}
\alias{isNullObject,SimREqualCon-method}
\alias{isNullObject,SimMisspec-method}
\alias{isNullObject,VirtualRSet-method}
\alias{isNullObject,data.frame-method}
\alias{isNullObject,SimMissing-method}
\alias{isNullObject,SimDataDist-method}
\alias{isNullObject,SimFunction-method}
\title{
	Check whether the object is the \code{NULL} type of that class
}
\description{
	Check whether the object is the \code{NULL} type of that class
}
\usage{
isNullObject(target)
}
\arguments{
  \item{target}{
	The target object
}
}
\section{Methods}{
	\describe{
	\item{signature(target="vector")}{
		Check whether the \code{vector} is \code{\linkS4class{NullVector}}, \code{NaN}, or a vector with zero length
	}
	\item{signature(target="matrix")}{
		Check whether the \code{matrix} is \code{\linkS4class{NullMatrix}}, \code{NaN}, or a matrix with \eqn{0 \times 0} dimension
	}
	\item{signature(target="data.frame")}{
		Check whether the \code{data.frame} is \code{\linkS4class{NullDataFrame}} or a data frame with 0 row or 0 column
	}
	\item{signature(target="SimMatrix")}{
		Check whether the \code{\linkS4class{SimMatrix}} is \code{\linkS4class{NullSimMatrix}} 
	}
	\item{signature(target="SymMatrix")}{
		Check whether the \code{\linkS4class{SymMatrix}} is \code{\linkS4class{NullSymMatrix}} 
	}
	\item{signature(target="SimVector")}{
		Check whether the \code{\linkS4class{SimVector}} is \code{\linkS4class{NullSimVector}} 
	}
	\item{signature(target="SimSet")}{
		Check whether the \code{\linkS4class{SimSet}} is \code{\linkS4class{NullSimSet}} 
	}
	\item{signature(target="SimEqualCon")}{
		Check whether the \code{\linkS4class{SimEqualCon}} is \code{\linkS4class{NullSimEqualCon}} 
	}
	\item{signature(target="SimREqualCon")}{
		Check whether the \code{\linkS4class{SimREqualCon}} is \code{\linkS4class{NullSimREqualCon}} 
	}
	\item{signature(target="SimMisspec")}{
		Check whether the \code{\linkS4class{SimMisspec}} is \code{\linkS4class{NullSimMisspec}} 
	}
	\item{signature(target="SimMissing")}{
		Check whether the \code{\linkS4class{SimMissing}} is \code{\linkS4class{NullSimMissing}} 
	}
	\item{signature(target="SimDataDist")}{
		Check whether the \code{\linkS4class{SimDataDist}} is \code{\linkS4class{NullSimDataDist}} 
	}
	\item{signature(target="SimFunction")}{
		Check whether the \code{\linkS4class{SimFunction}} is \code{\linkS4class{NullSimFunction}} 
	}
}}
\value{
	\code{TRUE} if the model is the \code{NULL} type.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
