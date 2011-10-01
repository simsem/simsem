runLavaan <- function(object, Data) {
	if(!require(lavaan)) {
		install.packages("lavaan")
		tryCatch(library(lavaan), error=function(e) {stop("The lavaan package cannot be loaded. Please install lavaan packages manually.")})
	}
	Data <- as.data.frame(Data)
	ni <- ncol(Data)
	Parameters <- object@Parameters
	Tag <- object@Tag
	varnames <- NULL
	if(Tag == "Path.exo") {
		nx <- ncol(object@Parameters@PH)
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else if(Tag == "SEM.exo") {
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
	#browser()
	code <- write.lavaan.code(Parameters, con.text)
	Fit <- sem(code, data=Data, meanstructure=TRUE)
	#Output <- capture.output(summary(Fit, fit.measures=TRUE, standardized=TRUE))
	Model.summary <- extract.lavaan.summary(Fit)
	Converged <- Fit@Fit@converged
	return(list(Summary=Model.summary, Detail=Fit, Converged = Converged))
}
