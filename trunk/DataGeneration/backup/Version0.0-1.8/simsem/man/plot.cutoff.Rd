\name{plot.cutoff}
\alias{plot.cutoff}
\alias{plot.cutoff-methods}
\alias{plot.cutoff,data.frame-method}
\alias{plot.cutoff,simResult-method}
\title{
Plot sampling distributions of fit indices
}
\description{
This function will plot sampling distributions of null hypothesis fit indices. The users may add cutoffs by specifying the \code{alpha} level.
}
\usage{
plot.cutoff(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	The object (\code{\linkS4class{simResult}} or \code{data.frame}) that contains values of fit indices in each distribution.
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
		\item \code{cutoff}: A priori cutoffs for fit indices, saved in a vector
		\item \code{alpha}:	A priori alpha level to find cutoffs of fit indices (do not specify when you have \code{cutoff})
		\item \code{reverse}:	The default is to find critical point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as TRUE, the directions are reversed.
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
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(200, CFA.Model)
SimModel <- model.object(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- result.object(SimData, SimModel, 50)
plot.cutoff(Output, 0.05, used.fit=c("RMSEA", "SRMR", "CFI", "TLI"))
}
