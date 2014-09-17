\name{plotCIwidth}
\alias{plotCIwidth}
\title{
Plot a confidence interval width of a target parameter
}
\description{
Plot a confidence interval width of a target parameter
}
\usage{
plotCIwidth(object, targetParam, assurance = 0.50, useContour = TRUE)
}
\arguments{
  \item{object}{
	The target (\code{\linkS4class{SimResult}} 
}
  \item{targetParam}{
	One or more target parameters to be plotted
}
  \item{assurance}{
	The percentile of the resulting width. When assurance is 0.50, the median of the widths is provided. See Lai & Kelley (2011) for more details.
}
  \item{useContour}{
	If there are two things from varying sample size, varying percent completely at random, or varying percent missing at random, the \code{plotCutoff} function will provide 3D graph. A contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
}
}
\value{
NONE. The plot the confidence interval width is provided.
}
\references{
Lai, K., & Kelley, K. (2011). Accuracy in parameter estimation for targeted effects in structural equation modeling: Sample size planning for narrow confidence intervals. \emph{Psychological Methods, 16}, 127-148.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for simResult that used in this function.
		\item \code{\link{getCIwidth}} to get confidence interval widths 
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

# Plot the widths of factor correlation
plotCIwidth(Output, "f1~~f2", assurance = 0.80)

# The example of continous varying sample size. Note that more fine-grained 
# values of n is needed, e.g., n=seq(50, 500, 1)
Output2 <- sim(NULL, n=seq(450, 500, 10), model=CFA.Model)

# Plot the widths along sample size value
plotCIwidth(Output2, "f1~~f2", assurance = 0.80)

# Specify both continuous sample size and percent missing completely at random. 
# Note that more fine-grained values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) 
# and pmMCAR=seq(0, 0.2, 0.01)
Output3 <- sim(NULL, n=seq(450, 500, 10), pmMCAR=c(0, 0.05, 0.1, 0.15), model=CFA.Model)

# Plot the contours that each contour represents the value of widths at each level
# of sample size and percent missing completely at random
plotCIwidth(Output3, "f1~~f2", assurance = 0.80)
}
}
