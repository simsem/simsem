# run
# Methods -- simsem package
# Run a particular object in simsem package.
# Generic Function: run(object, ...)
# Argument:
#	object: object in simsem that users wish to run
# 	... : Other arguments, such as data
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("run",
    signature(object = "SimNorm"),
    function (object, n = 1) 
    {
        rnorm(n,object@mean, object@sd)
    }
)
#Arguments: 
#	object:	SimNorm.c object
#	n:		Sample size. The default is 1.
#Description: This function will random samples from normal distribution object.
#Return: 	a number or numbers from normal distribution object
#Example:
#n02 <- simNorm(0, 0.2)
#run(n02)

setMethod("run",
    signature(object = "SimUnif"),
    function (object, n = 1) 
    {
        runif(n,object@min, object@max)
    }
)
#Arguments: 
#	object:	SimUnif.c object
#	n:		Sample size. The default is 1.
#Description: This function will random samples from uniform distribution object.
#Return: 	a number or numbers from uniform distribution object
#Example:
#u01 <- simUnif(0, 1)
#run(u01)

setMethod("run",
    signature(object = "SimMatrix"),
    function (object) 
    {
		Matrix <- object@free
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:Ncol) {
				if(is.na(Matrix[i, j]) & !is.nan(Matrix[i, j])) {
					temp <- suppressWarnings(as.numeric(object@param[i,j]))
					if(is.na(temp)) {
						Matrix[i, j] <- run(get(object@param[i,j])) #first, second)
					} else {
						Matrix[i, j] <- temp
					}
				}
			}
		}
		return(Matrix)
    }
)
#Arguments: 
#	object:	SimMatrix.c object
#Description: This function will draw numbers from any VirtualDist.c in the element. If there is no VirtualDist.c, the fixed value and starting values will be used.
#Return: 	matrix.c of the example of drawing a sample from SimMatrix.c

setMethod("run", signature="SymMatrix", definition= function(object) {
		if(is.null.object(object)) return(new("NullMatrix"))
		Matrix <- object@free
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:i) {
				if(is.na(Matrix[i, j]) & !is.nan(Matrix[i, j])) {
					temp <- suppressWarnings(as.numeric(object@param[i,j]))
					if(is.na(temp)) {
						Matrix[i, j] <- run(get(object@param[i,j])) #first, second)
					} else {
						Matrix[i, j] <- temp
					}
					Matrix[j, i] <- Matrix[i, j]
				}
			}
		}
		return(Matrix)
	}
)
#Arguments: 
#	object:	SymMatrix.c object
#Description: This function will draw numbers from any VirtualDist.c in the element. If there is no VirtualDist.c, the fixed value and starting values will be used.
#		This run function will keep the resulting object as symmetric matrix.
#Return: 	symmetric matrix.c of the example of drawing a sample from SymMatrix.c

setMethod("run", signature="SimVector", definition= function(object) {
		if(is.null.object(object)) return(new("NullVector"))
		Vector <- object@free
		Length <- length(Vector)
		for(i in 1:Length) {
			if(is.na(Vector[i]) & !is.nan(Vector[i])) { 
				temp <- suppressWarnings(as.numeric(object@param[i]))
				if(is.na(temp)) {
					Vector[i] <- run(get(object@param[i]))  #first, second)
				} else {
					Vector[i] <- temp
				}
			}
		}
		return(Vector)
	}
)
#Arguments: 
#	object:	SimVector.c object
#Description: This function will draw numbers from any VirtualDist.c in the element. If there is no VirtualDist.c, the fixed value and starting values will be used.
#Return: 	vector.c of the example of drawing a sample from SimVector.c

setMethod("run",
    signature(object = "NullSimMatrix"),
    function (object) 
    {
		return(new("NullMatrix"))
    }
)
#Arguments: 
#	object:	NullSimMatrix.c object
#Description:	return NullMatrix, which is the null object of the matrix.c
#Return: 	NullMatrix.c

setMethod("run", signature="NullSymMatrix", definition= function(object) {
		return(new("NullMatrix"))
	}
)
#Arguments: 
#	object:	NullSymMatrix.c object
#Description:	return NullMatrix, which is the null object of the matrix.c
#Return: 	NullMatrix.c

setMethod("run", signature="NullSimVector", definition= function(object) {
		return(new("NullVector"))
	}
)
#Arguments: 
#	object:	NullSimVector.c object
#Description:	return NullVector, which is the null object of the vector.c
#Return: 	NullVector.c

setMethod("run", signature(object="SimSet"), definition=function(object, equalCon=new("NullSimEqualCon")) {
		param <- list(LY = run(object@LY),
			VTE = run(object@VTE),
			TE = run(object@TE),
			VY = run(object@VY),
			TY = run(object@TY),
			MY = run(object@MY),
			BE = run(object@BE),
			VPS = run(object@VPS),
			PS = run(object@PS),
			VE = run(object@VE),
			AL = run(object@AL),
			ME = run(object@ME),
			LX = run(object@LX),
			VTD = run(object@VTD),
			TD = run(object@TD),
			VX = run(object@VX),
			TX = run(object@TX),
			MX = run(object@MX),
			GA = run(object@GA),
			VPH = run(object@VPH),
			PH = run(object@PH),
			KA = run(object@KA),
			TH = run(object@TH))
		if(!is.null.object(equalCon)) {
			if(object@modelType != equalCon@modelType) stop("Please provide same tags of SimSet and constraint")
			param <- constrain.matrices(param, equalCon, object@modelType)
		}
		LY <- param$LY
		VTE <- param$VTE
		TE <- param$TE
		VY <- param$VY
		TY <- param$TY
		MY <- param$MY
		BE <- param$BE
		VPS <- param$VPS
		PS <- param$PS
		VE <- param$VE
		AL <- param$AL
		ME <- param$ME
		LX <- param$LX
		VTD <- param$VTD
		TD <- param$TD
		VX <- param$VX
		TX <- param$TX
		MX <- param$MX
		GA <- param$GA
		VPH <- param$VPH
		PH <- param$PH
		KA <- param$KA
		TH <- param$TH
		if(object@modelType == "CFA") {
			if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
			if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
			if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
			if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		} else if(object@modelType == "Path") {
			if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
			if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
			if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
			if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		} else if(object@modelType =="Path.exo") {
			nx <- ncol(GA)
			ny <- nrow(GA)
			temp.BE <- combine.path.exo.endo(GA, BE)
			temp.PS <- combine.latent.cor.exo.endo(PH, PS)
			if(is.null.object(VPS)) {
				temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
				VPS <- temp.VPS[(nx + 1):(nx + ny)]
			}
			if(is.null.object(VE)) {
				temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
				VE <- temp.VE[(nx + 1):(nx + ny)]
			}
			if(is.null.object(ME)) {
				temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
				ME <- temp.ME[(nx + 1):(nx + ny)]
			}
			if(is.null.object(AL)) {
				temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
				AL <- temp.AL[(nx + 1):(nx + ny)]
			}
		} else if(object@modelType == "SEM") {
			if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
			if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
			if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
			if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
			if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
			if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
			if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
			if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		} else if(object@modelType == "SEM.exo") {
			nk <- ncol(GA)
			ne <- nrow(GA)
			temp.BE <- combine.path.exo.endo(GA, BE)
			temp.PS <- combine.latent.cor.exo.endo(PH, PS)
			if(is.null.object(VPS)) {
				temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
				VPS <- temp.VPS[(nk + 1):(nk + ne)]
			}
			if(is.null.object(VE)) {
				temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
				VE <- temp.VE[(nk + 1):(nk + ne)]
			}
			if(is.null.object(ME)) {
				temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
				ME <- temp.ME[(nk + 1):(nk + ne)]
			}
			if(is.null.object(AL)) {
				temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
				AL <- temp.AL[(nk + 1):(nk + ne)]
			}
			if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
			if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
			if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
			if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
			if(is.null.object(VTD)) VTD <- find.measurement.error.var(LX, PH, VX, VPH)
			if(is.null.object(VX)) VX <- find.indicator.var(LX, PH, VTD, VPH)
			if(is.null.object(MX)) MX <- find.indicator.mean(LX, KA, TX)
			if(is.null.object(TX)) TX <- find.measurement.intercept(LX, KA, MX)
		}
		return(new("MatrixSet", modelType=object@modelType, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH))
	}
)
#Arguments: 
#	object:	SimSet.c object
#	equalCon:	SimEqualCon.c that save all user-specified constraints.
#Description: This function will draw all SimMatrix.c, SymMatrix.c, and SimVector.c and return matrix, symmetric matrix, and vector. 
#		Also, the function will equate those elements that have equality constraint, if specified.
#Return: 	MatrixSet.c that is a random sample of all objects in SimSet.c

setMethod("run", signature(object="SimMisspec"), definition=function(object) {
		LY <- run(object@LY)
		VTE <- run(object@VTE)
		TE <- run(object@TE)
		VY <- run(object@VY)
		TY <- run(object@TY)
		MY <- run(object@MY)
		BE <- run(object@BE)
		VPS <- run(object@VPS)
		PS <- run(object@PS)
		VE <- run(object@VE)
		AL <- run(object@AL)
		ME <- run(object@ME)
		LX <- run(object@LX)
		VTD <- run(object@VTD)
		TD <- run(object@TD)
		VX <- run(object@VX)
		TX <- run(object@TX)
		MX <- run(object@MX)
		GA <- run(object@GA)
		VPH <- run(object@VPH)
		PH <- run(object@PH)
		KA <- run(object@KA)
		TH <- run(object@TH)
		return(new("MisspecSet", modelType=object@modelType, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH))
	}
)
#Arguments: 
#	object:	SimMisspec.c object
#Description: This function will draw all available SimMatrix.c, SymMatrix.c, and SimVector.c and return matrix, symmetric matrix, and vector. 
#Return: 	misspecifiedSet.c that is a random sample of all objects in SimMisspec.c

setMethod("run", signature="SimData", definition=function(object, n=NULL, dataOnly=TRUE) {
	if(!require(MASS)) stop("Please install MASS package")
	modelType <- object@modelType
	if(is.null(n)) n <- object@n
	param <- NULL
	misspec <- NULL
	implied.CM.param <- NULL
	implied.CM.misspec <- NULL
	misfit <- NULL
	count <- 0
	repeat {
		#browser()
		if(!is.null.object(object@misspec)) {
			Output <- run.misspecified(object@param, object@misspec, object@equalCon, object@conBeforeMis)
			param <- Output$param
			misspec <- Output$misspec
			if(validate.object(param) | validate.object(misspec)) {
				param <- reduce.matrices(param)
				misspec <- reduce.matrices(misspec)
				implied.CM.param <- create.implied.MACS(param)
				implied.CM.misspec <- create.implied.MACS(misspec)
				misfit <- average.misfit(implied.CM.misspec$M, implied.CM.misspec$CM, 
					implied.CM.param$M, implied.CM.param$CM, count.random.object(object@misspec))
				#param <- misspec # Pretend Misspecified as real parameters for data generation
				if(is.null.object(object@misfitBound)) break
				if(misfit > object@misfitBound[1] & misfit < object@misfitBound[2]) break
			}
		} else {
			param <- run(object@param, equalCon=object@equalCon)
			if(validate.object(param)) {
				param <- reduce.matrices(param)
				implied.CM.param <- create.implied.MACS(param)
				implied.CM.misspec <- implied.CM.param
				break
			}
		}
		count <- count + 1
		if(count > object@maxDraw) stop("The model cannot make a good set of parameters within limit of maximum random sampling of parameters")
	}
	# if(modelType == "CFA") {
		# factor.score <- mvrnorm(n, param@AL, param@PS)
		# error.score <- mvrnorm(n, rep(0, length(param@TY), param@TE)
		# intercept <- as.data.frame(matrix(param@TY, ncol=length(param@TY), byrow=TRUE))
		# Data <- (factor.score %*% t(param@LY)) + error.score + intercept
	# } else if (modelType == "Path") {
		# error.score <- mvrnorm(n, param@AL, param@PS)
		# ID <- matrix(0, nrow(param@BE), ncol(param@BE))
		# diag(ID) <- 1
		# data <- error.score %*% t(solve(ID - param@BE))
	# } else if (modelType == "Path.exo") {
	
	# } else if (modelType == "SEM") {
	
	# } else if (modelType == "SEM.exo") {
	
	# }
	Data <- mvrnorm(n, implied.CM.param$M, implied.CM.param$CM)
	varnames <- NULL
	if(modelType == "Path.exo") {
		nx <- ncol(run(object@param@PH))
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else if(modelType == "SEM.exo") {
		nx <- nrow(run(object@param@LX))
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
	Data <- as.data.frame(Data)
	if(dataOnly) {
		return(Data)
	} else {
		if(is.null(misspec)) misspec <- new("NullRSet")
		out <- new("SimDataOut", modelType=object@modelType, data=Data, param=create.free.parameters(object@param), 
					paramOut=param, misspecOut=misspec, equalCon=object@equalCon)
	}
})
#Arguments: 
#	object:	SimData.c object
#Description: 	The SimData object will draw samples from specified model.
#Return: 	Data frame drawn from the specified model.

setMethod("run", signature="SimModel", definition=function(object, data, simMissing=new("NullSimMissing")) {
	Output <- NULL
	DataOut <- NULL
	if(class(data) == "SimDataOut") {
		DataOut <- data
		data <- DataOut@data
	}
	if(!is.null.object(simMissing) && simMissing@numImps > 0) {
		Output <- runMI(data, object, simMissing@numImps,simMissing@impMethod)
	} else {
		if(object@package == "OpenMx") {
			Output <- runOpenMx(object, data)
		} else if (object@package == "lavaan") {
			Output <- runLavaan(object, data)
		}
	}
	#is.equal(DataOut@param, Output@param) yes --> compute bias
	if(!is.null(DataOut)) {
		check <- all.equal(DataOut@param, Output@param)
		if(length(check) == 1 && check == TRUE) {
			#paramOut <- DataOut@paramOut
			Output@paramValue <- DataOut@paramOut
		}
	}
	return(Output)
})
#Arguments: 
#	object:	SimModel.c object
#	Data:	Data that used to be analyzed by the specified model
#Description: 	The SimData will analyze the data and return the SimModelOut.c that saves the result.
#Return: 	SimModelOut.c that saves the result.

setMethod("run", signature="SimMissing", definition=function(object, data) {
	result <- NULL
	if(is(data, "SimDataOut")) {
		data@data <- imposeMissing(data@data, covs=object@covs, pmMCAR=object@pmMCAR,
            pmMAR=object@pmMAR, nforms=object@nforms, timePoints=object@timePoints,
            itemGroups=object@itemGroups, twoMethod=object@twoMethod)
	} else if (is.data.frame(data)) {
		data <- imposeMissing(data, covs=object@covs, pmMCAR=object@pmMCAR,
            pmMAR=object@pmMAR, nforms=object@nforms, timePoints=object@timePoints,
            itemGroups=object@itemGroups, twoMethod=object@twoMethod)	
	} else if (is.matrix(data)) {
		data <- as.data.frame(data)
		data <- imposeMissing(data, covs=object@covs, pmMCAR=object@pmMCAR,
            pmMAR=object@pmMAR, nforms=object@nforms, timePoints=object@timePoints,
            itemGroups=object@itemGroups, twoMethod=object@twoMethod)	
	}
	return(data)
})
#Arguments: 
#	object:	SimMissing object
#	Data:	Data that used to be imputed missing value
#Description: 	The SimData will analyze the data and return the SimModelOut.c that saves the result.
#Return: 	SimModelOut.c that saves the result.
