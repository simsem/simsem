runOpenMx <- function(object, Data) {
	if(length(grep("OpenMx",installed.packages()[,1])) == 0)
	{
		# try(source("http://openmx.psyc.virginia.edu/getOpenMx.R"), silent = TRUE)
		# tryCatch(library(OpenMx), error=function(e) {stop("The OpenMx package cannot be loaded. Please install OpenMx packages manually.")})
          print("Please install OpenMx.")
          # Brief workaround while we try to figure out how to install OpenMx on the cluster. You may want to keep it
          # like this so that users who don't use OpenMx don't have to install it to use the package.
	}
	data <- as.data.frame(Data)
	ni <- ncol(data)
	param <- object@param
	Labels <- make.labels(param)
	varnames <- NULL
	for(i in 1:ni) {
		temp <- paste("x", i, sep="")
		varnames <- c(varnames, temp)
	}
	if(is.null(colnames(data))) colnames(data) <- varnames
	mean <- mean(data)
	CM <- cov(data)
	n <- nrow(data)	
	start <- object@start
	if(!is.null.object(object@equalCon)) {
		Labels <- constrain.matrices(Labels, object@equalCon)
		start <- constrain.matrices(start, object@equalCon)
	}
	param <- collapse.exo(param)
	nk <- ncol(param@PS)
	Labels <- collapse.exo(Labels, value = NA)
	start <- collapse.exo(start)
	start <- find.OpenMx.values(param, start)
	matrixLY <- mxMatrix(type="Full", nrow=ni, ncol=nk, free=as.vector(is.na(param@LY)), values=as.vector(start@LY), labels=as.vector(Labels@LY), name="LY")#
	matrixPS <- mxMatrix(type="Symm", nrow=nk, ncol=nk, free=as.vector(is.na(param@PS)), values=as.vector(start@PS), labels=as.vector(Labels@PS), name="PS")#
	matrixTE <- mxMatrix(type="Symm", nrow=ni, ncol=ni, free=as.vector(is.na(param@TE)), values=as.vector(start@TE), labels=as.vector(Labels@TE), name="TE")#
	matrixTY <- mxMatrix(type="Full", nrow=ni, ncol=1, free=as.vector(is.na(param@TY)), values=as.vector(start@TY), labels=as.vector(Labels@TY), name="TY")#
	matrixAL <- mxMatrix(type="Full", nrow=nk, ncol=1, free=as.vector(is.na(param@AL)), values=as.vector(start@AL), labels=as.vector(Labels@AL), name="AL")#
	matrixBE <- mxMatrix(type="Full", nrow=nk, ncol=nk, free=as.vector(is.na(param@BE)), values=as.vector(start@BE), labels=as.vector(Labels@BE), name="BE")#
	matrixID <- mxMatrix(type="Iden", nrow=nk, ncol=nk, name="ID")
	Data <- mxData(observed=CM, type="cov", means=mean, numObs=n)
	Model <- NULL
	if(object@modelType == "CFA") {
		algebraR <- mxAlgebra(expression = LY %*% PS %*% t(LY) + TE, name="R")
		algebraM <- mxAlgebra(expression = t(TY + (LY %*% AL)), name="M")
		Obj <- mxMLObjective(covariance="R", means="M", dimnames=varnames)
		Model <- mxModel("Model", matrixLY, matrixPS, matrixTE, matrixTY, matrixAL, algebraR, algebraM, Obj, Data)
	} else if(object@modelType == "Path" | object@modelType == "Path.exo") {
		algebraR <- mxAlgebra(expression = solve(ID - BE) %*% PS %*% t(solve(ID - BE)), name="R")
		algebraM <- mxAlgebra(expression = t(solve(ID - BE) %*% AL), name="M")
		Obj <- mxMLObjective(covariance="R", dimnames=varnames)
		Model <- mxModel("Model", matrixBE, matrixPS, matrixAL, matrixID, algebraR, algebraM, Obj, Data)
	} else if(object@modelType == "SEM" | object@modelType == "SEM.exo") {
		algebraR <- mxAlgebra(expression = LY %*% (solve(ID - BE) %*% PS %*% t(solve(ID - BE))) %*% t(LY), name="R")
		algebraM <- mxAlgebra(expression = t(TY + (LY %*% (solve(ID - BE) %*% AL))), name="M")
		Obj <- mxMLObjective(covariance="R", dimnames=varnames)
		Model <- mxModel("Model", matrixBE, matrixLY, matrixPS, matrixTE, matrixTY, matrixAL, matrixID, algebraR, algebraM, Obj, Data)	
	} 
	fit <- mxRun(Model, silent=TRUE)
	#output <- summary(fit)
	return(fit)
}
