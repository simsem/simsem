runLavaan <- function(object, Data, miss="fiml") {
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
	fit <- sem(code, data=Data, meanstructure=TRUE, missing=miss)
	FitIndices <- extract.lavaan.summary(fit)
	coef <- combine.object(param, inspect(fit, "coef"))
    se <- combine.object(param, inspect(fit, "se"))
	#Converged <- fit@fit@converged
	Converged = TRUE
    if(sum(unlist(lapply(inspect(fit, "se"), sum))) == 0) Converged = FALSE
    return(new("SimModelOut", param=object@param, start=object@start,
        equalCon=object@equalCon, package=object@package, coef=coef,
        fit=FitIndices, se=se, converged=Converged))
}
