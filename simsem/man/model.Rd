\name{model}
\alias{model}
\alias{model.cfa}
\alias{model.path}
\alias{model.sem}
\title{
	Data generation template and analysis template for simulation.
}
\description{
	This function creates a model template (lavaan parameter table), which can be used for data generation and/or analysis for simulated structural equation modeling using simsem. Models are specified using Y-side parameter matrices with LISREL syntax notation. Each parameter matrix must be a \code{\linkS4class{SimMatrix}} or \code{\linkS4class{SimVector}} built using \code{bind}. In addition to the usual Y-side matrices in LISREL, both PS and TE can be specified using correlation matrices (RPS, RTE) and scaled by a vector of residual variances (VTE, VPS) or total variances (VY, VE). Multiple group models can be created by passing lists of \code{\linkS4class{SimMatrix}} or \code{\linkS4class{SimVector}} to arguments, or by simply specifying the number of groups when all group models are identical. 
}
\usage{
model(LY = NULL, PS = NULL, RPS = NULL, TE = NULL, RTE = NULL, BE = NULL, 
	VTE = NULL, VY = NULL, VPS = NULL, VE = NULL, TY = NULL, AL = NULL, MY = NULL, 
	ME = NULL, KA = NULL, GA = NULL, modelType, indLab = NULL, facLab = NULL, 
	covLab = NULL, groupLab = "group", ngroups = 1, con = NULL)
model.cfa(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, VTE = NULL, 
	VY = NULL, VPS = NULL, VE=NULL, TY = NULL, AL = NULL, MY = NULL, ME = NULL, 
	KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, 
	groupLab = "group", ngroups = 1, con = NULL)
model.path(PS = NULL, RPS = NULL, BE = NULL, VPS = NULL, VE=NULL, AL = NULL, 
	ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, 
	groupLab = "group", ngroups = 1, con = NULL)
model.sem(LY = NULL,PS = NULL,RPS = NULL, TE = NULL,RTE = NULL, BE = NULL, 
	VTE = NULL, VY = NULL, VPS = NULL, VE=NULL, TY = NULL, AL = NULL, MY = NULL, 
	ME = NULL, KA = NULL, GA = NULL, indLab = NULL, facLab = NULL, covLab = NULL, 
	groupLab = "group", ngroups = 1, con = NULL)
}
\arguments{
  \item{LY}{
	Factor loading matrix from endogenous factors to Y indicators (must be \code{\linkS4class{SimMatrix}} object).
}
  \item{PS}{
	Residual variance-covariance matrix among endogenous factors (must be \code{\linkS4class{SimMatrix}} object). Either RPS or PS (but not both) must be specified in SEM and CFA models.
}
  \item{RPS}{
	Residual correlation matrix among endogenous factors (must be \code{\linkS4class{SimMatrix}} object). Either RPS or PS (but not both) must be specified in SEM and CFA models.
}
  \item{TE}{
	Measurement error variance-covariance matrix among Y indicators (must be \code{\linkS4class{SimMatrix}} object). Either RTE or TE (but not both) must be specified in SEM and CFA models.
}
  \item{RTE}{
	Measurement error correlation matrix among Y indicators (must be \code{\linkS4class{SimMatrix}} object). Either RTE or TE (but not both) must be specified in SEM and CFA models.
}
  \item{BE}{
	Regression coefficient matrix among endogenous factors (must be \code{\linkS4class{SimMatrix}} object). BE must be specified in path analysis and SEM models. 
}
  \item{VTE}{
	Measurement error variance of indicators (must be \code{\linkS4class{SimVector}} object). Either VTE or VY (but not both) can be specified when RTE (instead of TE) is specified.
}
  \item{VY}{
	Total variance of indicators (must be \code{\linkS4class{SimVector}} object). Either VTE or VY (but not both) can be specified when RTE (instead of TE) is specified.
}
  \item{VPS}{
	Residual variance of factors (must be \code{\linkS4class{SimVector}} object). Either VPS or VE (but not both) can be specified when RPS (instead of PS) is specified.
}
  \item{VE}{
  Total variance of of factors (must be \code{\linkS4class{SimVector}} object). Either VPS or VE (but not both) can be specified when RPS (instead of PS) is specified.
}  
  \item{TY}{
	Measurement intercepts of Y indicators (must be \code{\linkS4class{SimVector}} object). Either TY or MY (but not both) can be specified. 
}
  \item{AL}{
	Endogenous factor intercepts (must be \code{\linkS4class{SimVector}} object). Either AL or ME (but not both) can be specified. 
}
  \item{MY}{
	Y indicator means (must be \code{\linkS4class{SimVector}} object). Either TY or MY (but not both) can be specified. 
}
  \item{ME}{
	Total mean of endogenous factors (must be \code{\linkS4class{SimVector}} object). NOTE: Either endogenous factor intercept or total mean of endogenous factor is specified. Both cannot be simultaneously specified.
}
  \item{KA}{
	Regression coefficient matrix from covariates to indicators (must be \code{\linkS4class{SimMatrix}} object). KA is needed when (fixed) exogenous covariates are needed only.
}
  \item{GA}{
	Regression coefficient matrix from covariates to factors (must be \code{\linkS4class{SimMatrix}} object). GA is needed when (fixed) exogenous covariates are needed only.
}
  \item{modelType}{
	"CFA", "Sem", or "Path". Model type must be specified to ensure that the matrices specified in model templates for data generation and analysis correspond to what the user intends.
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
	Number of groups for data generation (defaults to 1). Should only be specified for multiple group models in which all parameter matrices are identical across groups (when ngroups > 1, specified matrices are replicated for all groups). For multiple group models in which parameter matrices differ among groups, parameter matrices should instead be specified as a list (if any matrix argument is a list, the number of groups will be equal to the list's length, and the ngroups argument will be ignored).
}
  \item{con}{
	Additional parameters (phantom variables), equality constraints, and inequality constraints. Additional parameters must be specified using lavaan syntax. Allowed operators are ":=" (is defined as), "==" (is equal to), "<" (is less than), and ">" (is greater than). Names used in syntax must correspond to labels defined on free parameters in the model (with the exception that the name to the left of ":=" is a new parameter name). On the right hand side of all operators, any mathematical expressions are allowed, e.g., \code{"newparam := (load1 + load2 + load3)/3"}. For the "<" and ">" operators in data generation, if the specified relation is at odds with parameter specifications (e.g., the parameter to the left of the ">" operator is less that the parameter to the right), the left hand side parameter will be changed so that the relation holds with a very small difference (i.e., 0.000001). For example, in "load1 > load2", if load1 is 0.5 and load2 is 0.6, load1 will be changed to 0.6 + 0.000001 = 0.600001. 
}
}

\details{
The \emph{simsem} package is intricately tied to the \emph{lavaan} package for analysis of structural equation models. The analysis template that is generated by \code{model} is a lavaan parameter table, a low-level access point to lavaan that allows repeated analyses to happen more rapidly. If desired, the parameter table generated can be used directly with lavaan for many analyses.

The data generation template is simply a list of \code{SimMatrix} or \code{SimVector} objects. The \code{SimSem} object can be passed to the function \code{generate} to generate data, or can be passed to the function \code{sim} to generate and/or analyze data.

To simulate multiple group data, users can either specify a integer in the ngroups argument (which creates a list of identical model arguments for each group), or pass a list of  \code{SimMatrix} or \code{SimVector} to any of the matrix arguments with length(s) equal to the number of groups desired (this approach will cause the ngroups argument to be ignored). If only one argument is a list, all other arguments will be replicated across groups (with the same parameter identification, population parameter values/distributions, and misspecification). If equality constraints are specified, these parameters will be constrained to be equal across groups.	

The \code{model.cfa}, \code{model.path}, and \code{model.sem} are the shortcuts for the \code{model} function when \code{modelType} are \code{"CFA"}, \code{"Path"}, and \code{"SEM"}, respectively.
}

\value{
	\code{SimSem} object that contains the data generation template (\code{@dgen}) and analysis template (\code{@pt}).
}

\author{
	Patrick Miller (University of Notre Dame; \email{pmille13@nd.edu}), 
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
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

CFA.Model <- model(LY = LY, RPS = list(RPS, RPS), RTE = list(RTE, RTE), VTE=list(VTE, VTE), 
	ngroups=2, modelType = "CFA")

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

noninvariance <- model(LY = list(LY1, LY2), RPS = list(RPS1, RPS2), RTE = list(RTE, RTE), 
	VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

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
weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, 
	modelType = "CFA", con=script)
}
