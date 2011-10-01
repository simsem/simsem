\name{is.default}
\alias{is.default}
\title{
	Check whether a specified \code{simVector} was a default \code{simVector} (Internal)
}
\description{
	Check whether a specified \code{simVector} was a default \code{simVector} such that users did not specify anything. For example, check whether means of indicators are specified as 1.
}
\usage{
is.default(object)
}
\arguments{
  \item{object}{ checked \code{simVector} 
}
}
\value{
	TRUE if the object is a default \code{simVector}. FALSE, otherwise.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	Internal Function
}