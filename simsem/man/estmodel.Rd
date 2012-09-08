\name{estmodel}
\alias{estmodel}
\alias{estmodel.cfa}
\alias{estmodel.path}
\alias{estmodel.sem}
\title{
	Shortcut for data analysis template for simulation.
}
\description{
	Creates a data analysis template (lavaan parameter table) for simulations with structural equation models based on Y-side LISREL design matrices. Each corresponds to a LISREL matrix, but must be a matrix or a vector. In addition to the usual Y-side matrices in LISREL, both PS and TE can be specified using correlations (RPS, RTE) and scaled by a vector of residual variances (VTE, VPS) or total variances (VY, VE). Multiple groups are supported by passing lists of matrices or vectors to arguments, or by specifying the number of groups. 
}
\usage{
estmodel(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL, 
	VTE = NULL, VY = NULL, VPS = NULL, VE=NULL, TY = NULL, AL = NULL, 
	MY = NULL, ME = NULL, modelType, indLab=NULL, facLab=NULL, groupLab="group", 
	ngroups=1, smartStart=TRUE)
estmodel.cfa(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, VTE = NULL, 
	VY = NULL, VPS = NULL, VE=NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, 
	indLab=NULL, facLab=NULL, groupLab="group", ngroups=1, smartStart=TRUE)
estmodel.path(PS = NULL, RPS = NULL, BE = NULL, VPS = NULL, VE=NULL, AL = NULL, 
	ME = NULL, indLab=NULL, facLab=NULL, groupLab="group", ngroups=1, smartStart=TRUE)
estmodel.sem(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, 
	VTE = NULL, VY = NULL, VPS = NULL, VE=NULL, TY = NULL, AL = NULL, MY = NULL, 
	ME = NULL, indLab=NULL, facLab=NULL, groupLab="group", ngroups=1, smartStart=TRUE)
}
\arguments{
  \item{LY}{
	Factor loading matrix from endogenous factors to Y indicators (need to be a matrix or a list of matrices).
}
  \item{PS}{
	Residual covariance matrix among endogenous factors (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{RPS}{
	Residual correlation matrix among endogenous factors (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{TE}{
	Measurement error covariance matrix among Y indicators (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{RTE}{
	Measurement error correlation matrix among Y indicators (need to be a symmetric matrix or a list of symmetric matrices).
}
  \item{BE}{
	Regression coefficient matrix among endogenous factors (need to be a matrix or a list of matrices).
}
  \item{VTE}{
	Measurement error variance of indicators (need to be a vector or a list of vectors).
}
  \item{VY}{
	Total variance of indicators (need to be a vector or a list of vectors). NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
}
  \item{VPS}{
	Residual variance of factors (need to be a vector or a list of vectors).
}
  \item{VE}{
  Total variance of of factors (need to be a vector or a list of vectors). NOTE: Either residual variance of factors or total variance of factors is specified. Both cannot be simulatneously specified.
}  
  \item{TY}{
	Measurement intercepts of Y indicators. (need to be a vector or a list of vectors).
}
  \item{AL}{
	Endogenous factor intercept (need to be a vector or a list of vectors).
}
  \item{MY}{
	Overall Y indicator means. (need to be a vector or a list of vectors). NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
}
  \item{ME}{
	Total mean of endogenous factors (need to be a vector or a list of vectors). NOTE: Either endogenous factor intercept or total mean of endogenous factor is specified. Both cannot be simultaneously specified.
}
  \item{modelType}{
	"CFA", "Sem", or "Path". This is specified to ensure that the analysis and data generation template created based on specified matrices in model correspond to what the user intends.
}
  \item{indLab}{
	Character vector of indicator labels. If left blank, automatic labels will be generated as  \code{y1}, \code{y2}, ... \code{yy}.
}
  \item{facLab}{ 
	Character vector of factor labels. If left blank, automatic labels will be generated as \code{f1}, \code{f2}, ... \code{ff}}
  \item{groupLab}{
	Character of group-variable label (not the names of each group). If left blank, automatic labels will be generated as \code{group}
}
  \item{ngroups}{
	Integer. Number of groups for data generation, defaults to 1. If larger than one, all specified matrices will be repeated for each additional group. If any matrix argument is a list, the length of this list will be the number of groups and ngroups is ignored.
}
  \item{smartStart}{
	Defaults to FALSE. If TRUE, population parameter values that are real numbers will be used as starting values.
}
}

\details{
This function contains default settings:

For \code{modelType="CFA"}, \code{LY} is required. As the default, the on-diagonal elements of \code{PS} are fixed as 1 and the off-diagonal elements of \code{PS} are freely estimated. The off-diagonal elements of \code{TE} are freely estimated and the off-diagonal elements of \code{TE} are fixed to 0. The \code{AL} elements are fixed to 0. The \code{TY} elements are freely estimated.

For \code{modelType="Path"}, \code{BE} is required. As the default, the on-diagonal elements of \code{PS} are freely estimated, the off-diagonal elements between exogenous variables (covariance between exogenous variables) are freely estimated, and the other off-diagonal elements are fixed to 0. The \code{AL} elements are freely estimated. 

For \code{modelType="SEM"}, \code{LY} and \code{BE} are required. As the default, the on-diagonal elements of \code{PS} are fixed to 1, the off-diagonal elements between exogenous factors (covariance between exogenous factors) are freely estimated, and the other off-diagonal elements are fixed to 0. The off-diagonal elements of \code{TE} are freely estimated and the off-diagonal elements of \code{TE} are fixed to 0. The \code{AL} elements are fixed to 0. The \code{TY} elements are freely estimated.

The \code{estmodel.cfa}, \code{estmodel.path}, and \code{estmodel.sem} are the shortcuts for the \code{estmodel} function when \code{modelType} are \code{"CFA"}, \code{"Path"}, and \code{"SEM"}, respectively.
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
		\item \code{\link{sim}} For simulations using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{generate}} To generate data using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{analyze}} To analyze real or generated data using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{draw}} To draw parameters using the \code{\linkS4class{SimSem}} template.
	}
}
\examples{

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA

CFA.Model <- estmodel(LY = loading, modelType = "CFA")
}
