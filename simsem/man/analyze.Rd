\name{analyze}
\alias{analyze}
\title{
	Data analysis using the model specification
}
\description{
	Data analysis using the model specification (\code{linkS4class{SimSem}}) or the mx model object (\code{MxModel}). Data will be multiply imputed if the \code{miss} argument is specified. 
}
\usage{
analyze(model, data, package="lavaan", miss=NULL, aux=NULL, group = NULL, 
	mxMixture = FALSE, ...)
}
\arguments{
  \item{model}{
	The simsem model template (\code{linkS4class{SimSem}}) or the mx model object (\code{MxModel})
}
  \item{data}{
	The target dataset 
}
  \item{package}{
	The package used in data analysis. Currently, only \code{lavaan} package can be used.
}
  \item{miss}{ 
	The missing object with the specification of auxiliary variable or the specification for the multiple imputation.
}
  \item{aux}{ 
	List of auxiliary variables
}
  \item{group}{ 
	A group variable. This argument is applicable only when the \code{model} argument is a \code{MxModel} object.
}
  \item{mxMixture}{
    A logical whether to the analysis model is a mixture model. This argument is applicable when \code{MxModel} is used in the \code{model} argument only.
}
  \item{\dots}{ 
	Additional arguments in the \code{\link[lavaan]{lavaan}} function. See also \code{\link[lavaan]{lavOptions}}
}
}
\value{
	The \code{\linkS4class{lavaan}} object containing the output
}
\author{
	Patrick Miller (University of Notre Dame; \email{pmille13@nd.edu}), 
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	Note that users can use functions provided by \code{lavaan} package (\code{\link[lavaan]{lavaan}}, \code{\link[lavaan]{cfa}}, \code{\link[lavaan]{sem}}, or \code{\link[lavaan]{growth}}) if they wish to analyze data by lavaan directly. 
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

dat <- generate(CFA.Model,200)
out <- analyze(CFA.Model,dat)
}

