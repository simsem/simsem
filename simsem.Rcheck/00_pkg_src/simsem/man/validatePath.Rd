\name{validatePath}
\alias{validatePath}
\title{
	Validate whether the regression coefficient (or loading) matrix is good
}
\description{
	Validate whether the regression coefficient (or loading) matrix is good
}
\usage{
validatePath(path, var.iv, var.dv)
}
\arguments{
  \item{path}{
	A regression coefficient or loading matrix
}
  \item{var.iv}{
	The variances of variables corresponding to the columns
}
  \item{var.dv}{
	The variances of variables corresponding to the rows
}
}
\value{
	Return \code{TRUE} if the target regression coefficient matrix is good.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
