\name{planned.missing}
\alias{planned.missing}
\title{
  Impose planned missing data
}
\description{
  Function to generate a logical matrix for values to be deleted for
  planned missing data designs. 
}
\usage{
runif.object(Lower, Upper)
}
\arguments{
  \item{dims}{ Dimensions of the data matrix
}
  \item{nforms}{ Number of forms (defaults to 3)
  }
  \item{itemGroups} {list of lists of column groupings for each
    form. The first group is always the shared form by convention.}
  \item{twoMethod} {Pass this a list in the form of (column(s) to delete,
    percent to delete). Deletes that percent of values from that
    column(s)}
  \item{covs} {List of column indices that should not have missing
    values.Likely to be covariates.}
}
\value{
    Logical matrix of values in the matrix to be deleted based on the
    original dimensions.
}
\author{
    Patrick Miller (University of Kansas; \email{patr1ckm@ku.edu})
}

\examples{

  data <- matrix(rep(rnorm(10,1,1),19),ncol=19)

  pl1 <- planned.missing(dim(data))
  
  itemGroups <- list(c(1,2,3,4,5),c(7,8,9,10,11),c(12,14,16,18,20),c(13,15,17,19))

  pl2 <- planned.missing(dim(data), itemGroups=itemGroups)

  pl3 <- planned.missing(dim(data),nforms=4)

  pl4 <- planned.missing(dim(data),twoMethod=c(19,.8))

  pl5 <- planned.missing(dim(data),covs=c(19,20))
   
}