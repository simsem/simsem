# run
# Methods -- simsem package
# Run a particular object in simsem package.
# Generic Function: run(object, ...)
# Argument:
#	object: object in simsem that users wish to run
# 	... : Other arguments, such as data
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 26, 2012

################################################################################
# Distribution object: draw a random sample from a distribution

setMethod("run",
    signature(object = "SimNorm"),
    function (object, n = 1) 
    {
        rnorm(n,object@mean, object@sd)
    }
)
# Normal Distribution
# Argument: mean = population mean, sd = standard deviation

setMethod("run",
    signature(object = "SimUnif"),
    function (object, n = 1) 
    {
        runif(n,object@min, object@max)
    }
)
# Uniform Distribution
# Argument: min = lower bound, max = upper bound

setMethod("run",
    signature(object = "SimBeta"),
    function (object, n = 1) 
    {
        rbeta(n, object@shape1, object@shape2, object@ncp)
    }
)
# Beta Distribution
# Attributes: shape1, shape2 = positive numbers of beta distributions, ncp = non-centrality parameter (shape1, shape2 > 0)

setMethod("run",
    signature(object = "SimBinom"),
    function (object, n = 1) 
    {
        rbinom(n, object@size, object@prob)
    }
)
# Binomial Distribution
# Attributes: size = Number of trials (zero or more), prob = probability of success on each trial (0 to 1)

setMethod("run",
    signature(object = "SimCauchy"),
    function (object, n = 1) 
    {
        rcauchy(n, object@location, object@scale)
    }
)
# Cauchy Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

setMethod("run",
    signature(object = "SimChisq"),
    function (object, n = 1) 
    {
        rchisq(n, object@df, object@ncp)
    }
)
# Chi-squared Distribution
# Attributes: df = degrees of freedom (non-negative), ncp = non-centrality parameter (non-negative)

setMethod("run",
    signature(object = "SimExp"),
    function (object, n = 1) 
    {
        rexp(n, object@rate)
    }
)
# Exponential Distribution
# Attributes: rate = rate parameter

setMethod("run",
    signature(object = "SimF"),
    function (object, n = 1) 
    {
        rf(n, object@df1, object@df2, object@ncp)
    }
)
# F-distribution
# Attributes: df1, df2 = degrees of freedom (>0), ncp = non-centrality parameter (>=0)

setMethod("run",
    signature(object = "SimGamma"),
    function (object, n = 1) 
    {
        rgamma(n, object@shape, object@rate)
    }
)
# Gamma Distribution
# Attributes: shape = Shape parameter, scale = Scale parameter

setMethod("run",
    signature(object = "SimGeom"),
    function (object, n = 1) 
    {
        rgeom(n, object@prob)
    }
)
# Geometric Distribution
# Attributes: prob = probability of successes

setMethod("run",
    signature(object = "SimHyper"),
    function (object, n = 1) 
    {
        rhyper(n, object@m, object@n, object@k)
    }
)
# Hypergeometric Distribution
# Attributes: m = The number of successes, n = The number of failures, k =  The number of drawns (All are integers)

setMethod("run",
    signature(object = "SimLnorm"),
    function (object, n = 1) 
    {
        rlnorm(n, object@meanlog, object@sdlog)
    }
)
# Log Normal Distribution
# Attributes: meanlog = mean of the distribution in log scale, sdlog = standard deviation of the distribution in log scale (sdlog > 0)

setMethod("run",
    signature(object = "SimLogis"),
    function (object, n = 1) 
    {
        rlogis(n, object@location, object@scale)
    }
)

# Logistic Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

setMethod("run",
    signature(object = "SimNbinom"),
    function (object, n = 1) 
    {
        rnbinom(n, object@size, object@prob)
    }
)
# Negative Binomial Distribution
# Attributes: size = Target for number of sucessful trials (> 0), prob = probability of each trials (0 < p < 1)

setMethod("run",
    signature(object = "SimPois"),
    function (object, n = 1) 
    {
        rpois(n, object@lambda)
    }
)
# Poisson Distribution
# Attributes: lambda = mean and variance (> 0)

setMethod("run",
    signature(object = "SimT"),
    function (object, n = 1) 
    {
        rt(n, object@df, object@ncp)
    }
)
# Student t Distribution
# Attributes: df = degree of freedom (> 0), ncp = non-centrality parameter

setMethod("run",
    signature(object = "SimWeibull"),
    function (object, n = 1) 
    {
        rweibull(n, object@shape, object@scale)
    }
)
# Weibull Distribution
# Attributes: shape = shape parameter, scale = scale parameter (> 0)
###############################################################################

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

setMethod("run", signature(object="SimSet"), definition=function(object, equalCon=new("NullSimEqualCon"), makeList=FALSE) {
		param <- new("MatrixSet", modelType=object@modelType,
			LY = run(object@LY),
			VTE = run(object@VTE),
			TE = run(object@TE),
			RTE = run(object@RTE),
			VY = run(object@VY),
			TY = run(object@TY),
			MY = run(object@MY),
			BE = run(object@BE),
			VPS = run(object@VPS),
			PS = run(object@PS),
			RPS = run(object@RPS),
			VE = run(object@VE),
			AL = run(object@AL),
			ME = run(object@ME),
			LX = run(object@LX),
			VTD = run(object@VTD),
			TD = run(object@TD),
			RTD = run(object@RTD),
			VX = run(object@VX),
			TX = run(object@TX),
			MX = run(object@MX),
			GA = run(object@GA),
			VPH = run(object@VPH),
			PH = run(object@PH),
			RPH = run(object@RPH),
			KA = run(object@KA),
			TH = run(object@TH),
			RTH = run(object@RTH))
		if(!is.null.object(equalCon)) {
			if(object@modelType != equalCon@modelType) stop("Please provide same tags of SimSet and constraint")
			param <- constrain.matrices(param, equalCon)
		}
		out <- fillParam(param, object@modelType)
		if(makeList) {
			return(list(out, param))
		} else {
			return(out)
		}
	}
)
#Arguments: 
#	object:	SimSet.c object
#	equalCon:	SimEqualCon.c that save all user-specified constraints.
#Description: This function will draw all SimMatrix.c, SymMatrix.c, and SimVector.c and return matrix, symmetric matrix, and vector. 
#		Also, the function will equate those elements that have equality constraint, if specified.
#Return: 	MatrixSet.c that is a random sample of all objects in SimSet.c

setMethod("run", signature(object="SimMisspec"), definition=function(object) {
		misspec <- new("MisspecSet", modelType=object@modelType,
			LY = run(object@LY),
			VTE = run(object@VTE),
			TE = run(object@TE),
			RTE = run(object@RTE),
			VY = run(object@VY),
			TY = run(object@TY),
			MY = run(object@MY),
			BE = run(object@BE),
			VPS = run(object@VPS),
			PS = run(object@PS),
			RPS = run(object@RPS),
			VE = run(object@VE),
			AL = run(object@AL),
			ME = run(object@ME),
			LX = run(object@LX),
			VTD = run(object@VTD),
			TD = run(object@TD),
			RTD = run(object@RTD),
			VX = run(object@VX),
			TX = run(object@TX),
			MX = run(object@MX),
			GA = run(object@GA),
			VPH = run(object@VPH),
			PH = run(object@PH),
			RPH = run(object@RPH),
			KA = run(object@KA),
			TH = run(object@TH),
			RTH = run(object@RTH))
		return(misspec)
	}
)
#Arguments: 
#	object:	SimMisspec.c object
#Description: This function will draw all available SimMatrix.c, SymMatrix.c, and SimVector.c and return matrix, symmetric matrix, and vector. 
#Return: 	misspecifiedSet.c that is a random sample of all objects in SimMisspec.c

setMethod("run", signature="SimData", definition=function(object, n=NULL, dataOnly=TRUE) {
	if(!require(MASS)) stop("Please install MASS package")
	if(is.null(n)) n <- object@n
	paramSet <- drawParameters(object)
	DataOut <- createData(paramSet, n, object, dataOnly)
	return(DataOut)
	
})
#Arguments: 
#	object:	SimData.c object
#Description: 	The SimData object will draw samples from specified model.
#Return: 	Data frame drawn from the specified model.

setMethod("run", signature="SimModel", definition=function(object, data, simMissing=new("NullSimMissing"), estimator=NULL) {
	#find number of indicators
	#if indicator lab is specified, find indicator labels
	#check whether it matches between data and model; if not stop
	
	Output <- NULL
	DataOut <- NULL
	if(class(data) == "SimDataOut") {
		DataOut <- data
		data <- DataOut@data
	}
	if(is.null(colnames(data))) colnames(data) <- paste("y", 1:ncol(data))
	if(is.null.object(object@auxiliary)) {
		if(!(length(simMissing@cov) == 1 && simMissing@cov == 0)) object@auxiliary <- simMissing@cov
	}
	if(is.null.object(object@indicatorLab)) {
		if(is.null.object(object@auxiliary)) {
			object@indicatorLab <- colnames(data)
		} else if (is.numeric(object@auxiliary)) {
			if(max(object@auxiliary) > ncol(data)) stop("The maximum index in the auxiliary variable set is greater than the number of variables in the data.")
			object@indicatorLab <- colnames(data)[-object@auxiliary]
		} else {
			if(length(intersect(colnames(data), object@auxiliary)) != length(object@auxiliary)) stop("Some auxiliary variables does not exist in the dataset.")
			object@indicatorLab <- setdiff(colnames(data), object@auxiliary)
		}
	}
	if(is.numeric(object@indicatorLab)) object@indicatorLab <- colnames(data)[object@indicatorLab]
	if(is.numeric(object@auxiliary)) object@auxiliary <- colnames(data)[object@auxiliary]
	if(length(intersect(object@auxiliary, object@indicatorLab)) != 0) stop("There is common variable between the variables in the model and the auxiliary variables.")
	data <- data[,c(object@indicatorLab, object@auxiliary)]
	miss <- sum(is.na(data)) > 0	
	if(is.null(estimator)) estimator <- object@estimator
	estimator <- tolower(estimator)
	if(!is.null.object(simMissing) && simMissing@numImps > 0) {
		Output <- runMI(data, object, simMissing@numImps,simMissing@impMethod)
	} else {
		if(object@package == "OpenMx") {
			Output <- runOpenMx(object, data)
		} else if (object@package == "lavaan") {
			if(miss) {
				Output <- runLavaan(object, data, miss="fiml", estimator=estimator)
			} else {
				Output <- runLavaan(object, data, miss="listwise", estimator=estimator)
			}
		}
	}
	#is.equal(DataOut@param, Output@param) yes --> compute bias
	if(!is.null(DataOut)) {
		param <- DataOut@param
		check <- all.equal(param, Output@param)
		usedX <- NULL
		usedY <- NULL
		if(!(length(check) == 1 && check == TRUE) & !is.null.object(object@auxiliary)) {
			usedY <- which(!(colnames(data) %in% object@auxiliary))
			nx <- 0
			if(object@modelType == "SEM.exo") nx <- nrow(object@param@LX)
			if(object@modelType == "Path.exo") nx <- nrow(object@param@PH)
			if(nx > 0) usedX <- intersect(1:nx, usedY)
			usedY <- setdiff(usedY, usedX)
			param <- extract(param, y=usedY, x=usedX)
		}
		check <- all.equal(param, Output@param)
		if(length(check) == 1 && check == TRUE) {
			paramOut <- DataOut@paramOut
			if(!is.null.object(object@auxiliary)) paramOut <- extract(paramOut, y=usedY, x=usedX)
			Output@paramValue <- paramOut
		}
	}
	Output@n <- nrow(data)
	#Add labels in the SimModelOut --> go to SimModelOut and relabels it
	#Provide a nicer summary --> Groups elements from the same matrix together
	return(Output)
})
#Arguments: 
#	object:	SimModel.c object
#	Data:	Data that used to be analyzed by the specified model
#Description: 	The SimData will analyze the data and return the SimModelOut.c that saves the result.
#Return: 	SimModelOut.c that saves the result.

setMethod("run", signature="SimMissing", definition=function(object, data) {
	if(is(data, "SimDataOut")) {
		data@data <- as.data.frame(imposeMissing(data@data, cov=object@cov, pmMCAR=object@pmMCAR,
            pmMAR=object@pmMAR, nforms=object@nforms, timePoints=object@timePoints,
            itemGroups=object@itemGroups, twoMethod=object@twoMethod))
	} else if (is.data.frame(data)) {
		data <- as.data.frame(imposeMissing(data, cov=object@cov, pmMCAR=object@pmMCAR,
            pmMAR=object@pmMAR, nforms=object@nforms, timePoints=object@timePoints,
            itemGroups=object@itemGroups, twoMethod=object@twoMethod))	
	} else if (is.matrix(data)) {
		data <- as.data.frame(data)
		data <- as.data.frame(imposeMissing(data, cov=object@cov, pmMCAR=object@pmMCAR,
            pmMAR=object@pmMAR, nforms=object@nforms, timePoints=object@timePoints,
            itemGroups=object@itemGroups, twoMethod=object@twoMethod))	
	}
	return(data)
})
#Arguments: 
#	object:	SimMissing object
#	Data:	Data that used to be imputed missing value
#Description: 	The SimData will analyze the data and return the SimModelOut.c that saves the result.
#Return: 	SimModelOut.c that saves the result.

setMethod("run", signature="SimDataDist", definition=function(object, n, m, cm) {
	library(MASS)
	Data <- NULL
	# Check dim(M) dim(CM) dim(copula) are equal
	if (is.null.object(object)) {
		Data <- mvrnorm(n, m, cm)
	} else {
		library(copula)
		if(object@p > 1) {
			varNotZeros <- diag(cm) != 0
			object2 <- object
			cm2 <- cm
			if(sum(varNotZeros) < object@p) {
				object2 <- extract(object, which(varNotZeros))
				cm2 <- extract(cm, which(varNotZeros), which(varNotZeros))
			} 
			r <- cov2cor(as.matrix(cm2))
			for(i in 1:object2@p) {
				if(object2@reverse[i] == TRUE) {
					r[i,] <- -1 * r[i,]
					r[,i] <- -1 * r[,i]
				}
			}
			listR <- r[lower.tri(diag(object2@p))]
			CopNorm <- ellipCopula(family = "normal", dim = object2@p, dispstr = "un", param = listR)
			distName <- sapply(object2@dist, class)
			distName <- tolower(gsub("Sim", "", distName))
			attribute <- list()
			for(i in 1:length(object2@dist)) {
				temp <- list()
				indivAttr <- slotNames(object2@dist[[i]])
				for(j in 1:length(indivAttr)) {
					temp[[j]] <- call("=", indivAttr[[j]], slot(object2@dist[[i]], indivAttr[[j]]))
				}
				attribute[[i]] <- temp
			}
			Mvdc <- mvdc(CopNorm, distName, attribute)
			Data <- rmvdc(Mvdc, n)
			if(sum(varNotZeros) < object@p) {
				varZeros <- diag(cm) == 0
				constant <- matrix(0, n, sum(varZeros))
				Data <- data.frame(Data, constant)
				Data[,c(which(varNotZeros), which(varZeros))] <- Data
			} 
		} else if (object@p == 1) {
			if(as.matrix(cm)[1, 1] == 0) {
				Data <- rep(m[1], n)
			} else {
				Data <- as.matrix(run(object@dist[[1]],n=n))
			}
		} else {
			stop("Error in the run-SimDataDist.")
		}
		for(i in 1:object@p) {
			if(object@reverse[i] == TRUE) {
				meanOld <- mean(Data[,i])
				anchor <- max(Data[,i])
				datNew <- anchor - Data[,i]
				Data[,i] <- datNew - mean(datNew) + meanOld
			}
		}
		if(!is.matrix(Data)) Data <- as.matrix(Data)
		if(object@keepScale) {
			Data <- scale(Data)
			Data[is.na(Data)] <- 0
			fakeDat <- mvrnorm(n, m, cm)
			fakeMean <- apply(fakeDat, 2, mean)
			fakeSD <- apply(fakeDat, 2, sd)
			Data <- t(apply(Data, 1, function(y, m, s) { y * s + m }, m = fakeMean, s = fakeSD))
		}
		if(nrow(Data) == 1) Data <- t(Data)
	}
	return(Data)
})
#Arguments: 
#	object:	SimMissing object
#	Data:	Data that used to be imputed missing value
#Description: 	The SimData will analyze the data and return the SimModelOut.c that saves the result.
#Return: 	SimModelOut.c that saves the result.
