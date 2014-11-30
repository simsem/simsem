\name{inspect}
\alias{inspect,SimResult-method}
\title{
	Extract information from a simulation result
}
\description{
	Extract information from a simulation result
}
\arguments{
  \item{object}{
	The target \code{\linkS4class{SimResult}} object
}
  \item{what}{
    The target component to be extracted. Please see details below.
}
  \item{improper}{
    Specify whether to include the information from the replications with improper solutions
}
  \item{nonconverged}{
    Specify whether to include the information from the nonconvergent replications
}
}
\details{
	Here are the list of information that can be specified in the \code{what} argument. The items starting with * are the information that the \code{improper} and \code{nonconverged} arguments are not applicable.
	\itemize{
		\item *\code{"modeltype"}: The type of the simulation result
		\item *\code{"nrep"}: The number of overall replications, including converged and nonconverged replications
		\item \code{"param"}: Parameter values (equivalent to the \code{\link{getPopulation}} function)
		\item \code{"stdparam"}: Standardized parameter values (equivalent to the \code{\link{getPopulation}} function with \code{std = TRUE})
		\item \code{"coef"}: Parameter estimates (equivalent to the \code{\link[=coef,SimResult-method]{coef}} method)
		\item \code{"se"}: Standard errors
		\item \code{"fit"}: Fit indices
		\item \code{"misspec"}: Misspecified parameter values
		\item \code{"popfit"}: Population misfit
		\item \code{"fmi1"}: Fraction missings type 1
		\item \code{"fmi2"}: Fraction missings type 2
		\item \code{"std"}: Standardized Parameter Estimates
		\item \code{"stdse"}: Standard Errors of Standardized Values
		\item \code{"cilower"}: Lower bounds of confidence intervals
		\item \code{"ciupper"}: Upper bounds of confidence intervals
		\item \code{"ciwidth"}: Widths of confidence intervals
		\item *\code{"seed"}: Seed number (equivalent to the \code{\link{summarySeed}} function)
		\item \code{"ngroup"}: Sample size of each group
		\item \code{"ntotal"}: Total sample size
		\item \code{"mcar"}: Percent missing completely at random
		\item \code{"mar"}: Percent missing at random
		\item \code{"extra"}: Extra output from the \code{outfun} argument from the \code{\link{sim}} function)
		\item *\code{"time"}: Time elapsed in running the simulation (equivalent to the \code{\link{summaryTime}} function) 
		\item *\code{"converged"}: Convergence of each replication
	}
}
\value{
	The target information depending on the \code{what} argument
}
\author{
	Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for the object input
}
\examples{
\dontrun{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

# In reality, more than 5 replications are needed.
Output <- sim(5, CFA.Model, n=200)
inspect(Output, "coef")
inspect(Output, "param")
inspect(Output, "se", improper = TRUE, nonconverged = TRUE)
}
}
