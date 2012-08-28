\name{model}
\alias{model}
\title{
	TBA
}
\description{
	TBA... the \code{VE} argument is needed
}
\usage{
model(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, 
VTE = NULL, VY = NULL, VPS = NULL, TY = NULL, AL = NULL, MY = NULL, 
ME = NULL, modelType, indLab=NULL, facLab=NULL, ngroups=1, smartStart=TRUE)
}
\arguments{
  \item{LY}{
	Factor loading matrix from endogenous factors to Y indicators (need to be \code{\linkS4class{SimMatrix}} object).
}
  \item{PS}{
	Residual covariance matrix among endogenous factors (need to be \code{\linkS4class{SymMatrix}} object).
}
  \item{RPS}{
	Residual correlation matrix among endogenous factors (need to be \code{\linkS4class{SymMatrix}} object).
}
  \item{TE}{
	Measurement error covariance matrix among Y indicators (need to be \code{\linkS4class{SymMatrix}} object).
}
  \item{RTE}{
	Measurement error correlation matrix among Y indicators (need to be \code{\linkS4class{SymMatrix}} object).
}
  \item{BE}{
	Regression coefficient matrix among endogenous factors (need to be \code{\linkS4class{SimMatrix}} object).
}
  \item{VTE}{
	Measurement error variance of Y indicators (need to be \code{\linkS4class{SimVector}} object).
}
  \item{VY}{
	Total variance of Y indicators (need to be \code{\linkS4class{SimVector}} object). NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
}
  \item{VPS}{
	Residual variance of endogenous factors (need to be \code{\linkS4class{SimVector}} object).
}
  \item{TY}{
	Measurement intercepts of Y indicators. (need to be \code{\linkS4class{SimVector}} object).
}
  \item{AL}{
	Endogenous factor intercept (need to be \code{\linkS4class{SimVector}} object).
}
  \item{MY}{
	Overall Y indicator means. (need to be \code{\linkS4class{SimVector}} object). NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
}
  \item{ME}{
	Total mean of endogenous factors (need to be \code{\linkS4class{SimVector}} object). NOTE: Either endogenous factor intercept or total mean of endogenous factor is specified. Both cannot be simultaneously specified.
}
  \item{modelType}{
	TBA
}
  \item{indLab}{
	TBA
}
  \item{facLab}{
	TBA
}
  \item{ngroups}{
	TBA
}
  \item{smartStart}{
	TBA
}
}
\details{
	The matrices and vectors in the endogenous side are
	\itemize{
		\item \code{LY} for  
		\item \code{TE} for 
		\item \code{RTE} for 
		\item \code{BE} for  
		\item \code{PS} for 
		\item \code{RPS} for 
		\item \code{VTE} for 
		\item \code{VY} for 
		\item \code{TY} for 
		\item \code{MY} for 
		\item \code{VPS} for 
		\item \code{VE} for total endogenous factor variance (need to be \code{\linkS4class{SimVector}} object). NOTE: Either total endogenous factor variance or residual endogenous factor variance is specified. Both cannot be simultaneously specified.
		\item \code{AL} for 
		\item \code{ME} for 
	}
}
\value{
	\code{\linkS4class{SimSet}} object that represents the SEM object. This will be used for specifying data or analysis models later.
}
\author{
	Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu})
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{

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
}
