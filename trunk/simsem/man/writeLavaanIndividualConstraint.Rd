\name{writeLavaanIndividualConstraint}
\alias{writeLavaanIndividualConstraint}
\title{
	Write a lavaan code for a given equality constraint for each parameter
}
\description{
	Write a lavaan code for a given equality constraint for each parameter
}
\usage{
writeLavaanIndividualConstraint(Matrix, Attribute, Names)
}
\arguments{
  \item{Matrix}{
	The name of a matrix or a vector
}
  \item{Attribute}{
	A row in each equality constraint matrix ([group], [row], [column]) or ([group], [element])
}
  \item{Names}{
	A matrix or a vector that contains row and column names for indicator or factor labels
}
}
\value{
	A string for the specification of equality constraint in \code{lavaan}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
