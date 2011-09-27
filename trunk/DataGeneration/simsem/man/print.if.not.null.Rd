\name{print.if.not.null}
\alias{print.if.not.null}
\title{
	Provide basic summary of each object if that object is not NULL (Internal)
}
\description{
	Provide basic summary of each object if that object is not NULL (Internal)
}
\usage{
print.if.not.null(object, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Checked object whether it is NULL
}
  \item{name}{
	Name of the object
}
}
\value{
	NONE. Will print on R screen only.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
Internal Function
}
\examples{
AL <- vector.object(rep(NA, 5), "0")
print.if.not.null(AL, "Factor Mean")
}
