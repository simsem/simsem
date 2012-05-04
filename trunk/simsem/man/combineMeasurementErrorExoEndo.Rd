\name{combineMeasurementErrorExoEndo}
\alias{combineMeasurementErrorExoEndo}
\title{
	Combine measurement error correlation from the exogenous and endogenous sides into a single matrix
}
\description{
	Combine measurement error correlation from the exogenous and endogenous sides into a single matrix
}
\usage{
combineMeasurementErrorExoEndo(TD, TE, TH)
}
\arguments{
  \item{TD}{
	The error of measurement correlation in the exogenous side
}
  \item{TE}{
	The error of measurement correlation in the endogenous side
}
  \item{TH}{
	The correlation between the error of measurement in the exogenous and endogenous sides
}
}
\value{
	The combined error correlation matrix
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
