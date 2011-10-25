\name{plot.power}
\alias{plot.power}
\alias{plot.power-methods}
\alias{plot.power,data.frame,data.frame-method}
\alias{plot.power,data.frame,vector-method}
\alias{plot.power,simResult,simResult-method}
\alias{plot.power,simResult,vector-method}
\title{
Plot sampling distributions of fit indices that visualize power
}
\description{
This function will plot sampling distributions of fit indices that visualize power in either a histogram or overlapping histograms.
}
\usage{
plot.power(object.alt, object.null, ...)
}
\arguments{
  \item{object.alt}{
	The object (\code{\linkS4class{simResult}} or \code{data.frame}) that saves fit indices for alternative hypothesis
}
  \item{object.null}{
	The object that represents null hypothesis. It can be \code{vector} of cutoffs (that might be calculated from \code{\link{find.cutoff}} 
	or an object that save raw data of fit indices for null hypothesis (\code{\linkS4class{simResult}} or \code{data.frame}).
}
  \item{\dots}{
	Other arguments specific to different types of object you pass in the function.
}
}
\value{
NONE. Only plot the fit indices distributions.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\section{Details in \dots}{
	\enumerate{
		\item \code{alpha}:	A priori alpha level to find cutoffs of fit indices (do not specify when you have \code{cutoff})
		\item \code{used.fit}:	The name of fit indices that researchers wish to plot
	}
}
\seealso{
	\enumerate{
		\item \code{\linkS4class{simResult}} for result object that used in this function.
		\item \code{\link{find.cutoff}} to find values of cutoffs based on null hypothesis sampling distributions only
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
plot.power(Output.ALT, Output.NULL, alpha=0.05, used.fit=c("RMSEA", "CFI", "TLI", "SRMR"))
}
