\name{reassignNames}
\alias{reassignNames}
\title{
	Reassign the name of equality constraint
}
\description{
	Match the rownames of the equality constraint, check whether it match any model matrices, and substitute with an appropriate matrix name.
}
\usage{
reassignNames(modelType, Name)
}
\arguments{
  \item{modelType}{
	The type of the analysis model
}
  \item{Name}{
	The row names of a matrix in the equality constraint
}
}
\value{
	The appropriate row names given the analysis model
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
