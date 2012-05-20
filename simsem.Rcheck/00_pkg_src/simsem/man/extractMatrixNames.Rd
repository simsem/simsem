\name{extractMatrixNames}
\alias{extractMatrixNames}
\title{
	Extract a vector of parameter names based on specified rows and columns
}
\description{
	Extract a vector of parameter names based on specified rows and columns
}
\usage{
extractMatrixNames(columnName, keepRow=NULL, keepCol=NULL)
}
\arguments{
  \item{columnName}{
	A column name representing matrix elements (e.g., LY2_1) that will be used to be extracted
}
  \item{keepRow}{
	The rows of the matrix that we need to keep
}
  \item{keepCol}{
	The columns of the matrix that we need to keep
}
}
\value{
	A list that contains: 1) \code{columnName}: Original column name and 2) \code{newName}: Reordered column name.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# The function is not public

# vec <- c("LY1_1", "LY2_1", "LY3_1", "LY4_2", "LY5_2", "LY6_2", "LY7_3")
# extractMatrixNames(vec, 5:6, 2)
}
