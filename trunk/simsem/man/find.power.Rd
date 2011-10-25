\name{find.power}
\alias{find.power}
\alias{find.power-methods}
\alias{find.power,data.frame-method}
\alias{find.power,matrix-method}
\alias{find.power,simResult-method}
\title{
	Find power in rejecting alternative models based on fit indices criteria
}
\description{
	Find the proportion of fit indices that indicate worse fit than a specified cutoffs. The cutoffs may be calculated from \code{\link{find.cutoff}} of the null model.
}
\usage{
find.power(object.alt, cutoff, reverse = FALSE, used.fit=NULL)
}
\arguments{
  \item{object.alt}{
	\code{\linkS4class{simResult}} that indicates alternative model that users wish to reject
}
  \item{cutoff}{
	Fit indices cutoffs from null model or users. This should be a vector with a specified fit indices names as the name of vector elements. 
	The best way to specify cutoff is to calculate from \code{\link{find.cutoff}} function.
}
  \item{reverse}{
	The default is to count the proportion of fit indices that indicates lower fit to the model, such as 
	how many RMSEA in the alternative model that is worse than cutoffs. The direction can be reversed by setting as \code{TRUE}.
}
  \item{used.fit}{
	Vector of names of fit indices that researchers wish to find cutoffs from. The default is to find cutoffs of all fit indices.  
  }
}
\value{
	List of power given different fit indices.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\link{find.cutoff}} to find the cutoffs from null model.
		\item \code{\linkS4class{simResult}} to see how to create result object
	}
}
\examples{
loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- matrix.object(loading.null, 0.7)
PH.NULL <- sym.matrix.object(diag(1))
TD <- sym.matrix.object(diag(6))
CFA.Model.NULL <- matrix.CFA.object(LY = LX.NULL, PS = PH.NULL, TE = TD)
SimData.NULL <- data.object(500, CFA.Model.NULL)
SimModel <- model.object(CFA.Model.NULL)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output.NULL <- result.object(SimData.NULL, SimModel, 50)
Cut.NULL <- find.cutoff(Output.NULL, 0.95)

u79 <- runif.object(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- matrix.object(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
PH.ALT <- sym.matrix.object(latent.cor.alt, "u79")
CFA.Model.ALT <- matrix.CFA.object(LY = LX.ALT, PS = PH.ALT, TE = TD)
SimData.ALT <- data.object(500, CFA.Model.ALT)
Output.ALT <- result.object(SimData.ALT, SimModel, 50)
find.power(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
find.power(Output.ALT, Rule.of.thumb, used.fit=c("RMSEA", "CFI", "TLI", "SRMR"))
}
