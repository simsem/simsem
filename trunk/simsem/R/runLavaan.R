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
	if(modelType == "Path.exo") {
		nx <- ncol(object@param@PH)
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else if(modelType == "SEM.exo") {
		nx <- nrow(object@param@LX)
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}	
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else {
		for(i in 1:ncol(Data)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	}
	colnames(Data) <- varnames
	param <- tag.headers(param)
	con.text <- NULL
	if(!is.null.object(object@equalCon)) {
		equalCon <- object@equalCon
		equalCon <- reduce.constraint(equalCon)
		con.text <- transform.constraint(param, equalCon)
	} else {
		con.text <- blank.parameters(param)
	}	
	code <- write.lavaan.code(param, con.text)
	fit <- NULL
	if(modelType == "Path.exo") {
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
	Converged <- FALSE
	if(!is.null(fit)) {
		try(FitIndices <- extract.lavaan.summary(fit))
		try(coef <- combine.object(param, inspect(fit, "coef")))
		try(se <- combine.object(param, inspect(fit, "se")))
		try(Converged <- inspect(fit, "converged"))
		try(check <- sum(unlist(lapply(inspect(fit, "se"), sum))))
		try(if(is.na(check) || check == 0) Converged = FALSE, silent=TRUE)
	} 
    return(new("SimModelOut", param=object@param, start=object@start,
        equalCon=object@equalCon, package=object@package, coef=coef,
        fit=FitIndices, se=se, converged=Converged))
}
