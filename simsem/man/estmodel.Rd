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
	VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, 
	MY = NULL, ME = NULL, KA = NULL, GA = NULL, modelType, indLab = NULL, facLab = NULL, covLab = NULL, 
	groupLab = "group", ngroups = 1, con = NULL)
estmodel.cfa(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, VTE = NULL, 
	VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, KA = NULL, GA = NULL, 
	indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1, con = NULL)
estmodel.path(PS = NULL, RPS = NULL, BE = NULL, VPS = NULL, VE = NULL, AL = NULL, 
	ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1,
	con = NULL)
estmodel.sem(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, 
	VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, 
	ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, groupLab = "group", ngroups = 1,
	con = NULL)
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
  \item{KA}{
	Regression coefficient matrix from covariates to indicators (need to be a matrix or a list of matrices). KA is needed when (fixed) exogenous covariates are needed only.
}
  \item{GA}{
	Regression coefficient matrix from covariates to factors (need to be a matrix or a list of matrices). GA is needed when (fixed) exogenous covariates are needed only.
}
  \item{modelType}{
	"CFA", "Sem", or "Path". This is specified to ensure that the analysis and data generation template created based on specified matrices in model correspond to what the user intends.
}
  \item{indLab}{
	Character vector of indicator labels. If left blank, automatic labels will be generated as  \code{y1}, \code{y2}, ... \code{yy}.
}
  \item{facLab}{ 
	Character vector of factor labels. If left blank, automatic labels will be generated as \code{f1}, \code{f2}, ... \code{ff}
}
  \item{covLab}{ 
	Character vector of covariate labels. If left blank, automatic labels will be generated as \code{z1}, \code{z2}, ... \code{zz}
}
  \item{groupLab}{
	Character of group-variable label (not the names of each group). If left blank, automatic labels will be generated as \code{group}
}
  \item{ngroups}{
	Integer. Number of groups for data generation, defaults to 1. If larger than one, all specified matrices will be repeated for each additional group. If any matrix argument is a list, the length of this list will be the number of groups and ngroups is ignored.
}
  \item{con}{
	Additional parameters (phantom variables), equality constraints, and inequality constraints that users wish to specify in the model. The additional parameters are specified in lavaan syntax. The allowed operator are ":=" (is defined as), "==" (is equal to), "<" (is less than), and ">" (is greater than). Names used in the syntax are the labels defined on free parameters in the model except that the left-handed-side name of ":=" is a new parameter name. On the right hand side of all operators, any mathematical expressions are allowed, e.g., \code{"newparam := (load1 + load2 + load3)/3"}. For the "<" and ">" operators in data generation, if the parameters relation is not hold (e.g., the left hand side is less than the right hand side in the ">" operator), the left hand side parameters will be changed such that the relation holds with a very small difference (i.e., 0.000001). For example, in "load1 > load2", if load1 is 0.5 and load2 is 0.6, load1 will be changed to 0.6 + 0.000001 = 0.600001. 
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
loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA

CFA.Model <- estmodel(LY = loading, modelType = "CFA")

path <- matrix(0, 4, 4)
path[3, 1:2] <- NA
path[4, 3] <- NA
Path.Model <- estmodel(BE = path, modelType = "Path")

SEM.Model <- estmodel(BE = path, LY = loading, modelType="SEM")

# Shortcut
CFA.Model <- estmodel.cfa(LY = loading)
Path.Model <- estmodel.path(BE = path)
SEM.Model <- estmodel.sem(BE = path, LY = loading)
}
