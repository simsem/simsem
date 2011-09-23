\name{loading.from.alpha}
\alias{loading.from.alpha}
\title{Find standardized factor loading from coefficient alpha}
\description{
    Find standardized factor loading from coefficient alpha assuming that all items have equal loadings.
}
\usage{
loading.from.alpha(alpha, ni)
}
\arguments{
  \item{alpha}{A desired coefficient alpha value.}
  \item{ni}{A desired number of items.}
}
\value{
    \item{result}{The standardized factor loadings that make desired coefficient alpha with specified number of items.}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
    loading.from.alpha(0.8, 4)
}