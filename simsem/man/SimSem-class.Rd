\name{SimSem-class}
\Rdversion{1.1}
\docType{class}
\alias{SimSem-class}
\alias{summary,SimSem-method}
\title{Class \code{"SimSem"}}
\description{
	The template containing data-generation and data-analysis specification
}
\section{Objects from the Class}{
	Objects can be created by \code{\link{model}}. 
}
\section{Slots}{
  \describe{
    \item{\code{pt}:}{ Parameter table used in data analysis }
    \item{\code{dgen}:}{ Data generation template }
    \item{\code{modelType}:}{ Type of models (CFA, Path, or SEM) contained in this object }
    \item{\code{groupLab}:}{ The label of grouping variable }
	\item{\code{con}:}{ The list of defined parameters, equality constraints, or inequality constraints specified in the model }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{Get the summary of model specification }
 }
}
\author{
    Patrick Miller (University of Notre Dame; \email{pmille13@nd.edu}), 
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item Create an object this class by CFA, Path Analysis, or SEM model by \code{\link{model}}.
	}
}
\examples{
showClass("SimSem")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LY <- bind(loading, loadingValues)
summary(LY)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTE <- binds(error.cor)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
summary(CFA.Model)
}
