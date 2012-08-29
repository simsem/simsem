\name{plotCutoffNested}
\alias{plotCutoffNested}
\title{
	Plot sampling distributions of the differences in fit indices between nested models with fit indices cutoffs 
}
\description{
	This function will plot sampling distributions of the differences in fit indices between nested models if the nested model is true. The users may add cutoffs by specifying the \code{alpha} level.
}
\usage{
plotCutoffNested(nested, parent, alpha = 0.05, cutoff = NULL, 
usedFit = NULL, useContour = T)
}
\arguments{
  \item{nested}{
	\code{\linkS4class{SimResult}} that saves the analysis results of nested model from multiple replications
}
  \item{parent}{
	\code{\linkS4class{SimResult}} that saves the analysis results of parent model from multiple replications
}
  \item{alpha}{
	A priori alpha level
}
  \item{cutoff}{
	A priori cutoffs for fit indices, saved in a vector
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to plot the sampling distribution. 
}
  \item{useContour}{
	If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
}
}
\value{
	NONE. Only plot the fit indices distributions.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for simResult that used in this function.
		\item \code{\link{getCutoffNested}} to find the difference in fit indices cutoffs
	}
}
\examples{
\dontrun{
loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- bind(loading.null, 0.7)
RPH.NULL <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model.NULL <- model(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD, modelType="CFA")

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
CFA.Model.ALT <- model(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD, modelType="CFA")

# The actual number of replications should be greater than 10.
Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL) 
Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL)

plotCutoffNested(Output.NULL.NULL, Output.NULL.ALT, alpha=0.05)
}
}
