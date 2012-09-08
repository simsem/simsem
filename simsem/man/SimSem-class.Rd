\name{SimSem-class}
\Rdversion{1.1}
\docType{class}
\alias{SimSem-class}
%\alias{run,SimSem-method}
\alias{summary,SimSem-method}
%\alias{extract,SimSem-method}

\title{Class \code{"SimSem"}}
\description{
	TBA
}
\section{Objects from the Class}{
	TBA
}
\section{Slots}{
  \describe{
    \item{\code{pt}:}{ TBA }
    \item{\code{dgen}:}{TBA}
    \item{\code{modelType}:}{TBA}
    \item{\code{groupLab}:}{TBA}
  }
}
\section{Methods}{
  \describe{
    %\item{run}{Create a sample of parameters in this object. In other words, draw a sample from all random parameters which is represented in \code{\linkS4class{VirtualDist}}. }
    \item{summary}{Get the summary of model specification }
    %\item{extract}{Extract elements from a SimSem. There are several additional arguments. First, if \code{yOnly} is \code{TRUE}, then the result will provide only Y side. Second, \code{y} is the index of indicators in Y side to be extracted. Third, \code{e} is the index of factors in Y side to be extracted. Fourth, \code{x} is the index of the indicators in X side to be extracted. Finally, \code{k} is the index of the factors in X side to be extracted. }
	 }
}
\author{
    Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu}), 
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
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
LX <- bind(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- binds(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- binds(error.cor)

CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")
summary(CFA.Model)
#run(CFA.Model)

#CFA.Model2 <- extract(CFA.Model, y=1:3, e=1)
#summary(CFA.Model2)
}
