runLavaan <- function(object, Data) {
	if(!require(lavaan)) {
		install.packages("lavaan")
		tryCatch(library(lavaan), error=function(e) {stop("The lavaan package cannot be loaded. Please install lavaan packages manually.")})
	}
	Data <- as.data.frame(Data)
	ni <- ncol(Data)
	Parameters <- object@Parameters
	modelType <- object@modelType
	varnames <- NULL
	if(modelType == "Path.exo") {
		nx <- ncol(object@Parameters@PH)
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else if(modelType == "SEM.exo") {
		nx <- nrow(object@Parameters@LX)
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
	Parameters <- tag.headers(Parameters)
	con.text <- NULL
	if(!is.null.object(object@Constraint)) {
		Constraint <- object@Constraint
		Constraint <- reduce.constraint(Constraint)
		con.text <- transform.constraint(Parameters, Constraint)
	} else {
		con.text <- blank.parameters(Parameters)
	}	
	code <- write.lavaan.code(Parameters, con.text)
	Fit <- sem(code, data=Data, meanstructure=TRUE)
	Fit.indices <- extract.lavaan.summary(Fit)
	Estimates <- combine.object(Parameters, inspect(Fit, "coef"))
    SE <- combine.object(Parameters, inspect(Fit, "se"))
	#Converged <- Fit@Fit@converged
	Converged = TRUE
    if(sum(unlist(lapply(inspect(Fit, "se"), sum))) == 0) Converged = FALSE
    return(new("SimModelOut", Parameters=object@Parameters, Starting.Values=object@Starting.Values,
        Constraint=object@Constraint, Program=object@Program, Estimates=Estimates,
        Fit=Fit.indices, SE=SE, Convergence=Converged))
}
