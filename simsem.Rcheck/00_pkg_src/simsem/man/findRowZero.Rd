\name{findRowZero}
\alias{findRowZero}
\title{
	Find rows in a matrix that all elements are zero in non-fixed subset rows and columns. 
}
\description{
	Find rows in a matrix that all elements are zero in non-fixed subset rows and columns. This function will be used in the \code{\link{findRecursiveSet}} function
}
\usage{
findRowZero(square.matrix, is.row.fixed = FALSE)
}
\arguments{
  \item{square.matrix}{
	Any square matrix
}
  \item{is.row.fixed}{
	A logical vector with the length equal to the dimension of the \code{square.matrix}. If \code{TRUE}, the function will skip examining this row.
}
}
\value{
	A vector of positions that contain rows of all zeros
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
