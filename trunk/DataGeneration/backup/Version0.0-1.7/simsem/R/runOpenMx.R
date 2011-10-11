runOpenMx <- function(object, Data) {
	if(!require(OpenMx))
	{
		try(source("http://openmx.psyc.virginia.edu/getOpenMx.R"), silent = TRUE)
		tryCatch(library(OpenMx), error=function(e) {stop("The OpenMx package cannot be loaded. Please install OpenMx packages manually.")})
	}
	data <- as.data.frame(Data)
	ni <- ncol(data)
	Parameters <- object@Parameters
	Labels <- make.labels(Parameters)
	varnames <- NULL
	for(i in 1:ni) {
		temp <- paste("x", i, sep="")
		varnames <- c(varnames, temp)
	}
	if(is.null(colnames(data))) colnames(data) <- varnames
	Mean <- mean(data)
	CM <- cov(data)
	N <- nrow(data)	
	Starting.Values <- object@Starting.Values
	if(!is.null.object(object@Constraint)) {
		Labels <- constrain.matrices(Labels, object@Constraint)
		Starting.Values <- constrain.matrices(Starting.Values, object@Constraint)
	}
	Parameters <- collapse.exo(Parameters)
	nk <- ncol(Parameters@PS)
	Labels <- collapse.exo(Labels, value = NA)
	Starting.Values <- collapse.exo(Starting.Values)
	Starting.Values <- find.OpenMx.values(Parameters, Starting.Values)
	matrixLY <- mxMatrix(type="Full", nrow=ni, ncol=nk, free=as.vector(is.na(Parameters@LY)), values=as.vector(Starting.Values@LY), labels=as.vector(Labels@LY), name="LY")#
	matrixPS <- mxMatrix(type="Symm", nrow=nk, ncol=nk, free=as.vector(is.na(Parameters@PS)), values=as.vector(Starting.Values@PS), labels=as.vector(Labels@PS), name="PS")#
	matrixTE <- mxMatrix(type="Symm", nrow=ni, ncol=ni, free=as.vector(is.na(Parameters@TE)), values=as.vector(Starting.Values@TE), labels=as.vector(Labels@TE), name="TE")#
	matrixTY <- mxMatrix(type="Full", nrow=ni, ncol=1, free=as.vector(is.na(Parameters@TY)), values=as.vector(Starting.Values@TY), labels=as.vector(Labels@TY), name="TY")#
	matrixAL <- mxMatrix(type="Full", nrow=nk, ncol=1, free=as.vector(is.na(Parameters@AL)), values=as.vector(Starting.Values@AL), labels=as.vector(Labels@AL), name="AL")#
	matrixBE <- mxMatrix(type="Full", nrow=nk, ncol=nk, free=as.vector(is.na(Parameters@BE)), values=as.vector(Starting.Values@BE), labels=as.vector(Labels@BE), name="BE")#
	matrixID <- mxMatrix(type="Iden", nrow=nk, ncol=nk, name="ID")
	Data <- mxData(observed=CM, type="cov", means=Mean, numObs=N)
	Model <- NULL
	if(object@Tag == "CFA") {
		algebraR <- mxAlgebra(expression = LY %*% PS %*% t(LY) + TE, name="R")
		algebraM <- mxAlgebra(expression = t(TY + (LY %*% AL)), name="M")
		Obj <- mxMLObjective(covariance="R", means="M", dimnames=varnames)
		Model <- mxModel("Model", matrixLY, matrixPS, matrixTE, matrixTY, matrixAL, algebraR, algebraM, Obj, Data)
	} else if(object@Tag == "Path" | object@Tag == "Path.exo") {
		algebraR <- mxAlgebra(expression = solve(ID - BE) %*% PS %*% t(solve(ID - BE)), name="R")
		algebraM <- mxAlgebra(expression = t(solve(ID - BE) %*% AL), name="M")
		Obj <- mxMLObjective(covariance="R", dimnames=varnames)
		Model <- mxModel("Model", matrixBE, matrixPS, matrixAL, matrixID, algebraR, algebraM, Obj, Data)
	} else if(object@Tag == "SEM" | object@Tag == "SEM.exo") {
		algebraR <- mxAlgebra(expression = LY %*% (solve(ID - BE) %*% PS %*% t(solve(ID - BE))) %*% t(LY), name="R")
		algebraM <- mxAlgebra(expression = t(TY + (LY %*% (solve(ID - BE) %*% AL))), name="M")
		Obj <- mxMLObjective(covariance="R", dimnames=varnames)
		Model <- mxModel("Model", matrixBE, matrixLY, matrixPS, matrixTE, matrixTY, matrixAL, matrixID, algebraR, algebraM, Obj, Data)	
	} 
	Fit <- mxRun(Model, silent=TRUE)
	#output <- summary(Fit)
	return(Fit)
}
