setClass("simModel",  # The class provides model specification.
	representation(
		Tag="character",
		Parameters="freeParamSet",
		Starting.Values="reducedMatrixSet",
		Constraint="simConstraint",
		Program="character"), #OpenMx, lavaan
	prototype(
		Constraint=.NULL.simConstraint,
		Program="OpenMx")
)

setClass("simAnalysis", # The class provides model result.
         representation(
                        Parameters="freeParamSet",
                        Starting.Values="reducedMatrixSet",
                        Constraint="simConstraint",
                        Program="character",
                        Estimates="list",
                        Fit="numeric",
                        SE="list",
                        Convergence="logical"),
         prototype(
                Constraint=.NULL.simConstraint,
                   Convergence=FALSE),
         )

setMethod("model.object", signature(object="freeParamSet"), definition=function(object, Starting.Values = NULL, Constraint=.NULL.simConstraint, Program="lavaan") {
	Tag <- object@Tag
	if(!is.null(Starting.Values)) {
		if(Tag != Starting.Values@Tag) stop("Starting Values and Parameters do not have the same tag")
	} else {
		Starting.Values <- default.starting.values(object)
	}
	if(!is.null.object(simConstraint)) {
		if(Tag != Constraint@Tag) stop("simConstraint and freeParamSet do not have the same tag")
	}
	return(new("simModel", Tag=Tag, Parameters=object, Starting.Values=Starting.Values, Constraint=Constraint, Program=Program))
})

setMethod("model.object", signature(object="simMatrixSet"), definition=function(object, Constraint=.NULL.simConstraint, Program="lavaan", trial=10) {
	#browser()
	Starting.Values <- starting.values(object, trial)
	Starting.Values <- reduce.matrices(Starting.Values)
	#browser()
	freeParameters <- create.free.parameters(object)
	Tag <- object@Tag
	if(!is.null.object(Constraint)) {
		if(Tag != Constraint@Tag) stop("simConstraint and simMatrixSet do not have the same tag")
	}
	return(new("simModel", Tag=Tag, Parameters=freeParameters, Starting.Values=Starting.Values, Constraint=Constraint, Program=Program))
})

###################summary####################################

setMethod("run", signature="simModel", definition=function(object, Data) {
	Output <- NULL
	if(object@Program == "OpenMx") {
		Output <- runOpenMx(object, Data)
	} else if (object@Program == "lavaan") {
		Output <- runLavaan(object, Data)
	}
	return(Output)
})

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
	Output <- capture.output(summary(Fit, fit.measures=TRUE, standardized=TRUE))
	Fit.indices <- as.vector(fitmeasures(Fit))     #extract.lavaan.summary(Output)
        names(Fit.indices) <- names(fitmeasures(Fit))
        Estimates <- inspect(Fit, "coef")
        SE <- inspect(Fit, "se")
	Converged <- Fit@Fit@converged
        ### lavaan always converges, even it does not provide SE. We need something to check convergence status.
        return(new("simAnalysis", Parameters=object@Parameters, Starting.Values=object@Starting.Values,
                   Constraint=object@Constraint, Program=object@Program, Estimates=Estimates,
                   Fit=Fit.indices, SE=SE, Convergence=Converged))
}



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

setMethod("find.OpenMx.values", signature(Parameters="vector", Starting.Values="vector"), definition=function(Parameters, Starting.Values) {
	if(is.null.object(Parameters)) {
		return(Starting.Values)
	} else {
		Starting.Values[!is.na(Parameters)] <- Parameters[!is.na(Parameters)]
		return(round(Starting.Values, 3))
	}
})

setMethod("find.OpenMx.values", signature(Parameters="matrix", Starting.Values="matrix"), definition=function(Parameters, Starting.Values) {
	if(is.null.object(Parameters)) {
		return(Starting.Values)
	} else {
		Starting.Values[!is.na(Parameters)] <- Parameters[!is.na(Parameters)]
		return(round(Starting.Values, 3))
	}
})

setMethod("find.OpenMx.values", signature(Parameters="freeParamSet", Starting.Values="reducedMatrixSet"), definition=function(Parameters, Starting.Values) {
	Starting.Values@LY <- find.OpenMx.values(Parameters@LY, Starting.Values@LY)
	Starting.Values@TE <- find.OpenMx.values(Parameters@TE, Starting.Values@TE)
	Starting.Values@PS <- find.OpenMx.values(Parameters@PS, Starting.Values@PS)
	Starting.Values@BE <- find.OpenMx.values(Parameters@BE, Starting.Values@BE)
	Starting.Values@TY <- find.OpenMx.values(Parameters@TY, Starting.Values@TY)
	Starting.Values@AL <- find.OpenMx.values(Parameters@AL, Starting.Values@AL)
	Starting.Values@LX <- find.OpenMx.values(Parameters@LX, Starting.Values@LX)
	Starting.Values@TD <- find.OpenMx.values(Parameters@TD, Starting.Values@TD)
	Starting.Values@PH <- find.OpenMx.values(Parameters@PH, Starting.Values@PH)
	Starting.Values@GA <- find.OpenMx.values(Parameters@GA, Starting.Values@GA)
	Starting.Values@TX <- find.OpenMx.values(Parameters@TX, Starting.Values@TX)
	Starting.Values@KA <- find.OpenMx.values(Parameters@KA, Starting.Values@KA)
	Starting.Values@TH <- find.OpenMx.values(Parameters@TH, Starting.Values@TH)
	return(Starting.Values)
})

write.lavaan.code <- function(object, constraint) {
	#browser()
	result <- NULL
	object <- collapse.exo(object, label=TRUE)
	constraint <- collapse.exo(constraint, label=TRUE, value=NA) ###################Have some zeros
	if(!is.null.object(object@LY)) {
		for(i in 1:ncol(object@LY)) {
			temp <- paste(colnames(object@LY)[i], "=~")
			something <- FALSE
			for(j in 1:nrow(object@LY)) {
				if(is.na(object@LY[j, i]) | object@LY[j, i] != 0 | !is.na(constraint@LY[j, i])) {
					content <- paste(object@LY[j, i], "*", sep="")
					if(!is.na(constraint@LY[j, i])) content <- constraint@LY[j, i]
					temp <- paste(temp, " ", content, rownames(object@LY)[j], sep = "")
					something <- TRUE
				}
				if(something && j != nrow(object@LY) && (is.na(object@LY[j + 1, i]) | object@LY[j + 1, i] != 0)) temp <- paste(temp, "+")
			}
			result <- paste(result, temp, "\n")
		}
	}
	if(!is.null.object(object@BE)) {
		for(i in 1:nrow(object@BE)) {
			temp <- NULL 
			for(j in 1:ncol(object@BE)) {
				if(is.na(object@BE[i, j]) | object@BE[i, j] != 0 | !is.na(constraint@BE[i, j])) {
					content <- paste(object@BE[i, j], "*", sep="")
					if(!is.na(constraint@BE[i, j])) content <- constraint@BE[i, j]
					temp <- paste(temp, " ", content, colnames(object@BE)[j], sep = "")
				}
				if(!is.null(temp) && j != ncol(object@BE) && (is.na(object@BE[i, j + 1]) | object@BE[i, j + 1] != 0)) temp <- paste(temp, "+")
			}
			if(!is.null(temp)) {
				temp2 <- paste(rownames(object@BE)[i], "~")
				result <- paste(result, temp2, temp, "\n")	
			}
		}
	}	
	if(!is.null.object(object@PS)) {
		var.code <- NULL
		for(i in 1:length(diag(object@PS))) {
			if(!is.na(object@PS[i, i]) | !is.na(constraint@PS[i, i])) {
				content <- paste(object@PS[i, i], "*", sep = "")
				if(!is.na(constraint@PS[i, i])) content <- constraint@PS[i, i]
				var.code <- paste(var.code, colnames(object@PS)[i], " ~~ ", content, colnames(object@PS)[i], " \n", sep = "")
			}
		}
		cov.code <- NULL
		if(nrow(object@PS) > 1) {
		for(i in 2:nrow(object@PS)) {
			for(j in 1:(i-1)) {
				if(is.na(object@PS[i, j]) | object@PS[i, j] != 0 | !is.na(constraint@PS[i, j])) {
					content <- paste(object@PS[i, j], "*", sep="")
					if(!is.na(constraint@PS[i, j])) content <- constraint@PS[i, j]
					if(is.null.object(object@BE)) {
						cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ ", content, colnames(object@PS)[j], " \n", sep = "")
					} else {
						exo.set <- find.recursive.set(object@BE)[[1]]
						if(!(is.element(i, exo.set) & is.element(j, exo.set))) cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ ", content, colnames(object@PS)[j], " \n", sep = "")
					}
				}
			}
		}
		}
		result <- paste(result, var.code, cov.code)
	}
	if(!is.null.object(object@TE)) {
		var.code <- NULL
		for(i in 1:length(diag(object@TE))) {
			if(!is.na(object@TE[i, i]) | !is.na(constraint@TE[i, i])) {
				content <- paste(object@TE[i, i], "*", sep="")
				if(!is.na(constraint@TE[i, i])) content <- constraint@TE[i, i]
				var.code <- paste(var.code, colnames(object@TE)[i], " ~~ ", content, colnames(object@TE)[i], " \n", sep = "")
			}
		}
		cov.code <- NULL
		for(i in 2:nrow(object@TE)) {
			for(j in 1:(i-1)) {
				if(is.na(object@TE[i, j]) | object@TE[i, j] != 0 | !is.na(constraint@TE[i, j])) {
					content <- paste(object@TE[i, j], "*", sep="")
					if(!is.na(constraint@TE[i, j])) content <- constraint@TE[i, j]
					cov.code <- paste(cov.code, rownames(object@TE)[i], " ~~ ", content, colnames(object@TE)[j], " \n", sep = "")
				}
			}
		}
		result <- paste(result, var.code, cov.code)
	}	
	if(!is.null.object(object@AL)) {
		mean.code <- NULL
		for(i in 1:length(object@AL)) {
			if(!(object@Tag == "Path" | object@Tag == "Path.exo") | !is.na(object@AL[i]) | !is.na(constraint@AL[i])) {
				content <- paste(object@AL[i], "*", sep="")
				if(!is.na(constraint@AL[i])) content <- constraint@AL[i]
				mean.code <- paste(mean.code, names(object@AL)[i], " ~ ", content, "1 \n", sep = "")
			}
		}
		result <- paste(result, mean.code)
	}
	if(!is.null.object(object@TY)) {
		mean.code <- NULL
		for(i in 1:length(object@TY)) {
			content <- paste(object@TY[i], "*", sep="")
			if(!is.na(constraint@TY[i])) content <- constraint@TY[i]
			mean.code <- paste(mean.code, names(object@TY)[i], " ~ ", content, "1 \n", sep = "")
		}
		result <- paste(result, mean.code)
	}	
	return(result)
}

extract.lavaan.summary <- function(Output) {
    templine <- grep("Minimum Function Chi-square", Output)
    Chi <- as.numeric(extract.line(Output, templine[1])[4])
    templine <- grep("Degrees of freedom", Output)
    degree.freedom <- as.numeric(extract.line(Output, templine[1])[4])
    templine <- grep("P-value", Output)
    p.Chi <- as.numeric(extract.line(Output, templine[1])[2])
       
	templine <- grep("Minimum Function Chi-square", Output)
    Chi.base <- as.numeric(extract.line(Output, templine[2])[4])
    templine <- grep("Degrees of freedom", Output)
    degree.freedom.base <- as.numeric(extract.line(Output, templine[2])[4])
    templine <- grep("P-value", Output)
    p.Chi.base <- as.numeric(extract.line(Output, templine[2])[2])
	
	templine <- grep("Comparative Fit Index", Output)
    CFI <- as.numeric(extract.line(Output, templine)[5])
    templine <- grep("Tucker-Lewis Index", Output)
    TLI <- as.numeric(extract.line(Output, templine)[4])

	templine <- grep("Akaike", Output)
    AIC <- as.numeric(extract.line(Output, templine)[3])
    templine <- grep("Bayesian", Output)
    BIC <- as.numeric(extract.line(Output, templine)[3])
	
	templine <- grep("RMSEA", Output)
    RMSEA <- as.numeric(extract.line(Output, templine)[2])
    lower.RMSEA <- as.numeric(extract.line(Output, templine + 1)[5])	
    upper.RMSEA <- as.numeric(extract.line(Output, templine + 1)[6])	
    p.RMSEA <- as.numeric(extract.line(Output, templine + 2)[5])

	templine <- grep("SRMR", Output)
	SRMR <- as.numeric(extract.line(Output, templine)[2])
	result <- list(Chi=Chi, df=degree.freedom, p.Chi=p.Chi,
		Chi.base=Chi.base, df.base=degree.freedom.base, p.Chi.base=p.Chi.base,
		CFI=CFI, TLI=TLI, AIC=AIC, BIC=BIC, RMSEA=RMSEA, CI.90.RMSEA=c(lower.RMSEA, upper.RMSEA),
		p.RMSEA=p.RMSEA, SRMR=SRMR)
	return(result)
}

extract.line <- function(text, line=1) {
    Completed.text <- strsplit(text[line], " ")
    Completed.screen <- Completed.text[[1]] == ""
    Completed.split <- Completed.text[[1]][!Completed.screen]
    return(Completed.split)
}

transform.constraint <- function(object, constraint) {
	object <- blank.parameters(object)
	if(!is.null(constraint)) {
		Equality <- constraint@Equality
		for(i in 1:length(Equality)) {
			current <- Equality[[i]]
			con.text <- type.constraint(rownames(current)[1], current[1,], slot(object, rownames(current)[1]))
			for(j in 2:nrow(current)) {
				Matrix <- rownames(current)[j]
				if(Matrix == "PS" | Matrix == "PH" | Matrix == "TE" | Matrix == "TD") {
					elements <- c(as.numeric(current[j, 2]), as.numeric(current[j, 3]))
					slot(object, Matrix)[max(elements), min(elements)] <- con.text
				} else {
					slot(object, Matrix)[as.numeric(current[j, 2]), as.numeric(current[j, 3])] <- con.text
				}
			#con.text <- Equality[[i]][1,]     get(rownames(current)[j])
		################################################ Right Here###############
			#equal("PA.domain2 ~ 1")*1 
			}
		}
	}
	return(object)
}

blank.parameters <- function(object) {
	if(!is.null.object(object@LY)) object@LY[,] <- NA
	if(!is.null.object(object@TE)) object@TE[,] <- NA
	if(!is.null.object(object@PS)) object@PS[,] <- NA
	if(!is.null.object(object@BE)) object@BE[,] <- NA
	if(!is.null.object(object@TY)) object@TY[] <- NA
	if(!is.null.object(object@AL)) object@AL[] <- NA
	if(!is.null.object(object@LX)) object@LX[,] <- NA
	if(!is.null.object(object@TD)) object@TD[,] <- NA
	if(!is.null.object(object@PH)) object@PH[,] <- NA
	if(!is.null.object(object@GA)) object@GA[,] <- NA
	if(!is.null.object(object@TX)) object@TX[] <- NA
	if(!is.null.object(object@KA)) object@KA[] <- NA
	if(!is.null.object(object@TH)) object@TH[,] <- NA
	return(object)
}

type.constraint <- function(Matrix, Attribute, Names) {
	result <- "equal('"
	if(!is.na(Attribute[1])) result <- paste(result, Attribute[1], ".", sep="")
	if(length(Attribute) == 2) {
		result <- paste(result, names(Names)[as.numeric(Attribute[2])], " ~ 1')*", sep="")
		#############################Right Here#############################
	} else if(length(Attribute) == 3) {
		Row <- as.numeric(Attribute[2])
		Column <- as.numeric(Attribute[3])
		if(Matrix == "LY" | Matrix == "LX") {
			result <- paste(result, colnames(Names)[Column], " =~ ", rownames(Names)[Row], "')*", sep="")
		} else if(Matrix == "PS" | Matrix == "PH" | Matrix == "TE" | Matrix == "TD" | Matrix == "TH") {
			result <- paste(result, rownames(Names)[Row], " ~~ ", colnames(Names)[Column], "')*", sep="")
		} else if(Matrix == "GA" | Matrix == "BE") {
			result <- paste(result, rownames(Names)[Row], " ~ ", colnames(Names)[Column], "')*", sep="")		
		}
	}
	return(result)
}




