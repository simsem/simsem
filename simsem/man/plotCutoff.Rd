\name{plotCutoff}
\alias{plotCutoff}
\title{
Plot sampling distributions of fit indices with fit indices cutoffs
}
\description{
This function will plot sampling distributions of fit indices. The users may add cutoffs by specifying the \code{alpha} level.
}
\usage{
plotCutoff(object, alpha = NULL, revDirec = FALSE, usedFit = NULL, 
	useContour = TRUE)
}
\arguments{
  \item{object}{
	The target (\code{\linkS4class{SimResult}} 
}
  \item{alpha}{
	A priori alpha level to get the cutoffs of fit indices
}
  \item{revDirec}{
	The default is to find critical point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as \code{TRUE}, the directions are reversed.
}
  \item{usedFit}{
	The name of fit indices that researchers wish to plot
}
  \item{useContour}{
	If there are two things from varying sample size, varying percent completely at random, or varying percent missing at random, the \code{plotCutoff} function will provide 3D graph. A contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
}
}
\value{
NONE. The plot the fit indices distributions is provided.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for simResult that used in this function.
		\item \code{\link{getCutoff}} to find values of cutoffs based on null hypothesis sampling distributions only
	}
}
\examples{
\dontrun{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LY <- bind(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTE <- binds(error.cor)
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=200, model=CFA.Model) 

# Plot the cutoffs with desired fit indices
plotCutoff(Output, 0.05, usedFit=c("RMSEA", "SRMR", "CFI", "TLI"))

# The example of continous varying sample size. Note that more fine-grained 
# values of n is needed, e.g., n=seq(50, 500, 1)
Output2 <- sim(NULL, n=seq(450, 500, 10), model=CFA.Model)

# Plot the cutoffs along sample size value
plotCutoff(Output2, 0.05)

# Specify both continuous sample size and percent missing completely at random. 
# Note that more fine-grained values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) 
# and pmMCAR=seq(0, 0.2, 0.01)
Output3 <- sim(NULL, n=seq(450, 500, 10), pmMCAR=c(0, 0.05, 0.1, 0.15), model=CFA.Model)

# Plot the contours that each contour represents the value of cutoff at each level
# of sample size and percent missing completely at random
plotCutoff(Output3, 0.05)
}
}
