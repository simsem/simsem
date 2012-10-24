\name{model}
\alias{model}
\alias{model.cfa}
\alias{model.path}
\alias{model.sem}
\title{
	Data generation template and analysis template for simulation.
}
\description{
	Creates a data generation and analysis template (lavaan parameter table) for simulations with structural equation models based on Y-side LISREL design matrices. Each corresponds to a LISREL matrix, but must be a \code{\linkS4class{SimMatrix}} or \code{\linkS4class{SimVector}} built using \code{bind}. In addition to the usual Y-side matrices in LISREL, both PS and TE can be specified using correlations (RPS, RTE) and scaled by a vector of residual variances (VTE, VPS) or total variances (VY, VE). Multiple groups are supported by passing lists of \code{\linkS4class{SimMatrix}} or \code{\linkS4class{SimVector}} to arguments, or by specifying the number of groups. 
}
\usage{
model(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL, 
	VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, 
	ME = NULL, modelType, indLab = NULL, facLab = NULL, groupLab = "group", 
	ngroups = 1, con = NULL)
model.cfa(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, VTE = NULL, 
	VY = NULL, VPS = NULL, VE=NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, 
	indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, con = NULL)
model.path(PS = NULL, RPS = NULL, BE = NULL, VPS = NULL, VE=NULL, AL = NULL, 
	ME = NULL, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1, 
	con = NULL)
model.sem(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, 
	VTE = NULL, VY = NULL, VPS = NULL, VE=NULL, TY = NULL, AL = NULL, MY = NULL, 
	ME = NULL, indLab = NULL, facLab = NULL, groupLab = "group", ngroups = 1,
	con = NULL)
}
\arguments{
  \item{LY}{
	Factor loading matrix from endogenous factors to Y indicators (need to be \code{\linkS4class{SimMatrix}} object).
}
  \item{PS}{
	Residual covariance matrix among endogenous factors (need to be \code{\linkS4class{SimMatrix}} object).
}
  \item{RPS}{
	Residual correlation matrix among endogenous factors (need to be \code{\linkS4class{SimMatrix}} object).
}
  \item{TE}{
	Measurement error covariance matrix among Y indicators (need to be \code{\linkS4class{SimMatrix}} object).
}
  \item{RTE}{
	Measurement error correlation matrix among Y indicators (need to be \code{\linkS4class{SimMatrix}} object).
}
  \item{BE}{
	Regression coefficient matrix among endogenous factors (need to be \code{\linkS4class{SimMatrix}} object).
}
  \item{VTE}{
	Measurement error variance of indicators (need to be \code{\linkS4class{SimVector}} object).
}
  \item{VY}{
	Total variance of indicators (need to be \code{\linkS4class{SimVector}} object). NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
}
  \item{VPS}{
	Residual variance of factors (need to be \code{\linkS4class{SimVector}} object).
}
  \item{VE}{
  Total variance of of factors (need to be \code{\linkS4class{SimVector}} object). NOTE: Either residual variance of factors or total variance of factors is specified. Both cannot be simulatneously specified.
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
	"CFA", "Sem", or "Path". This is specified to ensure that the analysis and data generation template created based on specified matrices in model correspond to what the user intends.
}
  \item{indLab}{
	Character vector of indicator labels. If left blank, automatic labels will be generated as  \code{y1}, \code{y2}, ... \code{yy}.
}
  \item{facLab}{ 
	Character vector of factor labels. If left blank, automatic labels will be generated as \code{f1}, \code{f2}, ... \code{ff}
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
The \emph{simsem} package is intricately tied to the \emph{lavaan} package for analysis of structural equation models. The analysis template that is generated by \code{model} is a lavaan parameter table, a low-level access point to lavaan that allows repeated analyses to happen more rapidly. If desired, the parameter table generated can be used directly with lavaan for many analyses.

The data generation template is simply a list of \code{SimMatrix} or \code{SimVector}. The \code{SimSem} object can be passed to the function \code{generate} to generate data.

If multiple group data is desired, the user can optionally either specify the number of groups argument, or pass a list of  \code{SimMatrix} or \code{SimVector} to any of the matrix arguments. The length of this list will be the number of groups. If only one argument is a list, all other arguments will be automatically replicated to that length, parameters will be identified in the same way, have the same population parameter value/distribution, and have the same misspecification. If only ngroups is specified, all arguments will be replicated in this fashion. If equality constraints are present during the automatic replication, these parameters will be constrained to be equal across groups.	

The \code{model.cfa}, \code{model.path}, and \code{model.sem} are the shortcuts for the \code{model} function when \code{modelType} are \code{"CFA"}, \code{"Path"}, and \code{"SEM"}, respectively.
}

\value{
	\code{SimSem} object that contains the data generation template (\code{@dgen}) and analysis template (\code{@pt}).
}

\author{
	Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu}), 
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{\item {\code{\link{sim}} for simulations using the \code{\linkS4class{SimSem}} template.}
		\item \code{\link{generate}} To generate data using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{analyze}} To analyze real or generated data using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{draw}} To draw parameters using the \code{\linkS4class{SimSem}} template.
	}
}
\examples{
# Example 1: Confirmatory factor analysis
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

# Example 2: Multiple-group CFA with weak invariance
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- paste0("con", 1:3)
loading[4:6, 2] <- paste0("con", 4:6)
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

CFA.Model <- model(LY = LY, RPS = list(RPS, RPS), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

# Example 3: Linear growth curve model with model misspecification
factor.loading <- matrix(NA, 4, 2)
factor.loading[,1] <- 1
factor.loading[,2] <- 0:3
LY <- bind(factor.loading)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- bind(factor.mean, factor.mean.starting)

factor.var <- rep(NA, 2)
factor.var.starting <- c(1, 0.25)
VPS <- bind(factor.var, factor.var.starting)

factor.cor <- matrix(NA, 2, 2)
diag(factor.cor) <- 1
RPS <- binds(factor.cor, 0.5)

VTE <- bind(rep(NA, 4), 1.2)

RTE <- binds(diag(4))

TY <- bind(rep(0, 4))

LCA.Model <- model(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY, modelType="CFA")

# Example 4: Path analysis model with misspecified direct effect
path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1,0.5,0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1,-0.1,0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1,0.3,0.1)")

ME <- bind(rep(NA, 4), 0)

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType="Path")

# Example 5: Full SEM model 
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- "con1"
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
LY <- bind(loading, loading.start)

RTE <- binds(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- binds(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"
BE <- bind(path, path.start)

SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

# Shortcut example
SEM.model <- model.sem(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

# Example 6: Multiple Group Model
loading1 <- matrix(NA, 6, 1)
LY1 <- bind(loading1, 0.7)
loading2 <- matrix(0, 6, 2)
loading2[1:3, 1] <- NA
loading2[4:6, 2] <- NA
LY2 <- bind(loading2, 0.7)

latent.cor2 <- matrix(NA, 2, 2)
diag(latent.cor2) <- 1
RPS1 <- binds(as.matrix(1))
RPS2 <- binds(latent.cor2, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

noninvariance <- model(LY = list(LY1, LY2), RPS = list(RPS1, RPS2), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

# Example 7: Inequality Constraints

loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- c("load1", "load2", "load3")
loading.in[4:6, 2] <- c("load4", "load5", "load6")
mis <- matrix(0,6,2)
mis[loading.in == "0"] <- "runif(1, -0.1, 0.1)"
LY.in <- bind(loading.in, "runif(1, 0.7, 0.8)", mis)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

VPS1 <- bind(rep(1, 2))

VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))

# Inequality constraint
script <- "
sth := load1 + load2 + load3
load4 == (load5 + load6) / 2
load4 > 0
load5 > 0
sth2 := load1 - load2
"

# Model Template
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA", con=script)
}
