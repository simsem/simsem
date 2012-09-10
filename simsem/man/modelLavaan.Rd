\name{model.lavaan}
\alias{model.lavaan}
\title{
	Build the data generation template and analysis template from the lavaan result
}
\description{
	Creates a data generation and analysis template (lavaan parameter table) for simulations with the \code{\linkS4class{lavaan}} result. Model misspecification may be added into the template by a vector, a matrix, or a list of vectors or matrices (for multiple groups). 
}
\usage{
model.lavaan(object, std = FALSE, LY = NULL, PS = NULL, RPS = NULL, 
	TE = NULL, RTE = NULL, BE = NULL, VTE = NULL, VY = NULL, VPS = NULL, 
	VE=NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, smartStart=TRUE)
}
\arguments{
  \item{object}{
	A \code{\linkS4class{lavaan}} object to be used to build the data generation and analysis template.
}
  \item{std}{
	If TRUE, use the resulting standardized parameters for data generation. If FALSE, use the unstandardized parameters for data generation.
}
  \item{LY}{
	Model misspecification in factor loading matrix from endogenous factors to Y indicators (need to be a matrix or a list of matrices).
}
  \item{PS}{
	Model misspecification in residual covariance matrix among endogenous factors (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{RPS}{
	Model misspecification in residual correlation matrix among endogenous factors (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{TE}{
	Model misspecification in measurement error covariance matrix among Y indicators (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{RTE}{
	Model misspecification in measurement error correlation matrix among Y indicators (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{BE}{
	Model misspecification in regression coefficient matrix among endogenous factors (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{VTE}{
	Model misspecification in measurement error variance of indicators (need to be a vector or a list of vectors).
}
  \item{VY}{
	Model misspecification in total variance of indicators (need to be a vector or a list of vectors). NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
}
  \item{VPS}{
	Model misspecification in residual variance of factors (need to be a vector or a list of vectors).
}
  \item{VE}{
  Model misspecification in total variance of of factors (need to be a vector or a list of vectors). NOTE: Either residual variance of factors or total variance of factors is specified. Both cannot be simulatneously specified.
}  
  \item{TY}{
	Model misspecification in measurement intercepts of Y indicators. (need to be a vector or a list of vectors).
}
  \item{AL}{
	Model misspecification in endogenous factor intercept (need to be a vector or a list of vectors).
}
  \item{MY}{
	Model misspecification in overall Y indicator means. (need to be a vector or a list of vectors). NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
}
  \item{ME}{
	Model misspecification in total mean of endogenous factors (need to be a vector or a list of vectors). NOTE: Either endogenous factor intercept or total mean of endogenous factor is specified. Both cannot be simultaneously specified.
}
  \item{smartStart}{
	Defaults to FALSE. If TRUE, population parameter values that are real numbers will be used as starting values.
}
}
\value{
	\code{SimSem} object that contains the data generation template (\code{@dgen}) and analysis template (\code{@pt}).
}

\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{model}} To build data generation and data analysis template for simulation.
		\item \code{\link{sim}} for simulations using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{generate}} To generate data using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{analyze}} To analyze real or generated data using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{draw}} To draw parameters using the \code{\linkS4class{SimSem}} template.
	}
}
\examples{
HS.model <- ' visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)

# Create data generation and data analysis model from lavaan
# Data generation is based on standardized parameters
datamodel1 <- model.lavaan(fit, std=TRUE)

# Data generation is based on unstandardized parameters
datamodel2 <- model.lavaan(fit, std=FALSE)

# Data generation model with misspecification on cross-loadings
crossload <- matrix("runif(1, -0.1, 0.1)", 9, 3)
crossload[1:3, 1] <- 0
crossload[4:6, 2] <- 0
crossload[7:9, 3] <- 0
datamodel3 <- model.lavaan(fit, std=TRUE, LY=crossload)
}
