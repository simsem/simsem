# runLavaan
# function -- simsem package
# Transform model object to lavaan script, run the obtained data, and make model output object
# Argument:
#	object: 	model object
# 	Data: 		real data
#	miss: 		method of missing data handling
#	estimator:	Method of estimation. The default is maximum likelihood (ML)
# Return:	Model output object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

runLavaan <- function(object, Data, miss="fiml", estimator="ML") {
	if(!require(lavaan)) {
		install.packages("lavaan")
		tryCatch(library(lavaan), error=function(e) {stop("The lavaan package cannot be loaded. Please install lavaan packages manually.")})
	}
	Data <- as.data.frame(Data)
	ni <- ncol(Data)
	param <- object@param
	modelType <- object@modelType
	varnames <- NULL
	nz <- 0
	if(!isNullObject(object@auxiliary)) nz <- length(object@auxiliary)
	if(modelType == "Path.exo") {
		nx <- ncol(object@param@PH)
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}
		for(i in 1:(ncol(Data) - nx - nz)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else if(modelType == "SEM.exo") {
		nx <- nrow(object@param@LX)
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}	
		for(i in 1:(ncol(Data) - nx - nz)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else {
		for(i in 1:(ncol(Data) - nz)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	}
	nameAux <- NULL
	if(!isNullObject(object@auxiliary)) {
		nameAux <- paste("z", 1:length(object@auxiliary), sep="")
		varnames <- c(varnames, nameAux)
	}
	colnames(Data) <- varnames
	param <- tagHeaders(param)
	con.text <- NULL
	if(!isNullObject(object@equalCon)) {
		equalCon <- object@equalCon
		equalCon <- reduceConstraint(equalCon)
		con.text <- writeLavaanConstraint(param, equalCon)
	} else {
		con.text <- blankParameters(param)
	}	
	code <- writeLavaanCode(param, con.text, aux = nameAux)
	fit <- NULL
	if(modelType == "Path.exo" | modelType == "Path") {
		try(fit <- sem(code, data=Data, meanstructure=TRUE, missing=miss, fixed.x=FALSE, estimator=estimator))
	} else {
		try(fit <- sem(code, data=Data, meanstructure=TRUE, missing=miss, estimator=estimator))
	}
	coef <- new("SimRSet")
	se <- new("SimRSet")
	name <- slotNames(param)
	for(i in 1:length(name)) {
		slot(coef, name[i]) <- slot(param, name[i])
		slot(se, name[i]) <- slot(param, name[i])
	}
	FitIndices <- NA
	FitIndicesNull <- NA
	Converged <- FALSE
	if(!is.null(fit)) {
		try(FitIndices <- extractLavaanFit(fit))
		if(nz > 0) {
			############################ Run Null Model
			codeNull <- writeLavaanNullCode(setdiff(varnames, nameAux), nameAux)
			try(fitNull <- sem(codeNull, data=Data, meanstructure=TRUE, missing=miss, estimator=estimator, fixed.x=FALSE))
			try(FitIndicesNull <- extractLavaanFit(fitNull))
			if(!is.na(FitIndices) && !is.na(FitIndicesNull)) {
				ratioNULL <- FitIndicesNull["Chi"]/FitIndicesNull["df"]
				ratioReal <- FitIndices["Chi"]/FitIndices["df"]
				TLI <- (ratioNULL - ratioReal)/(ratioNULL - 1)
				if(TLI < 0) TLI <- 0
				# Compute CFI
				num1 <- FitIndicesNull["Chi"] - FitIndicesNull["df"]
				if(num1 < 0) num1 <- 0
				num2 <- FitIndices["Chi"] - FitIndices["df"]
				if(num2 < 0) num2 <- 0
				CFI <- (num1 - num2)/num1
				FitIndices["baseline.Chi"] <- FitIndicesNull["Chi"]
				FitIndices["baseline.df"] <- FitIndicesNull["df"]
				FitIndices["baseline.pvalue"] <- FitIndicesNull["pvalue"]
				FitIndices["CFI"] <- CFI
				FitIndices["TLI"] <- TLI
			}
		}
		try(coef <- combineObject(param, inspect(fit, "coef")))
		try(se <- combineObject(param, inspect(fit, "se")))
		try(Converged <- inspect(fit, "converged"))
		try(check <- sum(unlist(lapply(inspect(fit, "se"), sum))))
		try(if(is.na(check) || check == 0) Converged = FALSE, silent=TRUE)
	} 
    return(new("SimModelOut", param=object@param, start=object@start,
        equalCon=object@equalCon, package=object@package, coef=coef,
        fit=FitIndices, se=se, converged=Converged))
}
