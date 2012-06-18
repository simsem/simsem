\name{anova}
%\alias{anova}
%\alias{anova-methods}
%\alias{anova,ANY-method}
\alias{anova,SimResult-method}
\alias{anova,SimModelOut-method}
\alias{anova,SimModelMIOut-method}
\title{
	Provide a comparison of nested models and nonnested models across replications
}
\description{
	This function will provide averages of model fit statistics and indices for nested models. It will also provide average differences of fit indices and power for likelihood ratio tests of nested models.
}
%\usage{
%anova(object, ...)
%}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} object being described. Currently at least two objects must be included as arguments
}
  \item{\dots}{
    any additional arguments, such as additional objects or for the function with result object
}
}
\value{
	A data frame that provides the statistics described above from all parameters.
	For using with \code{\linkS4class{SimModelOut}}, each column means
	\itemize{
		\item \code{summary:} The fit indices of each output
		\item \code{diff:} The differences in fit indices among outputs 
	}	
	For using with \code{linkS4class{SimResult}}, the result is a list with two or three elements:
	\itemize{
		\item \code{summary:} Average of fit indices across all replications
		\item \code{diff:} Average of the differences in fit indices across all replications
		\item \code{varyParam:} The statistical power of chi-square difference test given values of varying parameters (such as sample size or percent missing) 
	}
}
\author{
    Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu}), 
	Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}

\seealso{
	\code{\linkS4class{SimResult}} for the object input
}
\examples{
loading1 <- matrix(0, 6, 1)
loading1[1:6, 1] <- NA
loading2 <- loading1
loading2[6,1] <- 0
LX1 <- simMatrix(loading1, 0.7)
LX2 <- simMatrix(loading2, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model1 <- simSetCFA(LY = LX1, RPS = RPH, RTE = RTD)
CFA.Model2 <- simSetCFA(LY = LX2, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model1, 500)
SimModel1 <- simModel(CFA.Model1)
SimModel2 <- simModel(CFA.Model2)
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
# Need to make sure that both simResult calls have the same seed!
Output1 <- simResult(5, SimData, SimModel1, seed=123567)
Output2 <- simResult(5, SimData, SimModel2, seed=123567)
anova(Output1, Output2)

Output1b <- simResult(NULL, SimData, SimModel1, seed=123567, n=seq(50, 500, 50))
Output2b <- simResult(NULL, SimData, SimModel2, seed=123567, n=seq(50, 500, 50))
anova(Output1b, Output2b)
}
