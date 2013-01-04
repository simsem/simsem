\name{summaryConverge}
\alias{summaryConverge}
\title{
Provide a comparison between the characteristics of convergent replications and nonconvergent replications
}
\description{
This function provides a comparison between the characteristics of convergent replications and nonconvergent replications. The comparison includes sample size (if varying), percent missing completely at random (if varying), percent missing at random (if varying), parameter values, misspecified-parameter values (if applicable), and population misfit (if applicable).
}
\usage{
summaryConverge(object, improper = FALSE)
}
\arguments{
  \item{object}{
		\code{\link{SimResult}} object being described
}
  \item{improper}{
	If TRUE, include the replications that provided improper solutions
}
}
\value{
A list with the following elements:
\itemize{
	\item \code{Converged} The number of convergent and nonconvergent replications
	\item \code{n} Sample size
	\item \code{pmMCAR} Percent missing completely at random
	\item \code{pmMAR} Percent missing at random
	\item \code{paramValue} Parameter values
	\item \code{misspecValue} Misspecified-parameter values
	\item \code{popFit} Population misfit. See details of each element at \code{\link{summaryMisspec}}.
}
Each element will provide the head-to-head comparison between convergent and nonconvergent replications properties.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
\dontrun{
path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1, 0.5, 0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1, -0.1, 0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")

loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA
mis.loading <- matrix("runif(1, -0.3, 0.3)", 12, 4)
mis.loading[is.na(loading)] <- 0
LY <- bind(loading, "runif(1, 0.7, 0.9)", misspec=mis.loading)

mis.error.cor <- matrix("rnorm(1, 0, 0.1)", 12, 12)
diag(mis.error.cor) <- 0
RTE <- binds(diag(12), misspec=mis.error.cor)

SEM.Model <- model(RPS = RPS, BE = BE, LY=LY, RTE=RTE, modelType="SEM")

n1 <- list(mean = 0, sd = 0.1)
chi5 <- list(df = 5)

facDist <- bindDist(c("chisq", "chisq", "norm", "norm"), chi5, chi5, n1, n1)

# In reality, more than 50 replications are needed.
simOut <- sim(50, n=500, SEM.Model, sequential=TRUE, facDist=facDist, estimator="mlr")

# Summary the convergent and nonconvergent replications
summaryConverge(simOut)
}
}
