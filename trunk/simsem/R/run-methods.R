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
	if(is.null(n)) n <- object@n
	paramSet <- drawParameters(object)
	DataOut <- createData(paramSet, n, object, dataOnly)
	return(DataOut)
	
})
#Arguments: 
#	object:	SimData.c object
#Description: 	The SimData object will draw samples from specified model.
#Return: 	Data frame drawn from the specified model.

setMethod("run", signature="SimModel", definition=function(object, data, simMissing=new("NullSimMissing"), estimator=NULL, indicatorLab=NULL, factorLab=NULL, auxilliary=NULL, covariate=NULL) {
	#find number of indicators
	#if indicator lab is specified, find indicator labels
	#check whether it matches between data and model; if not stop
	
	Output <- NULL
	DataOut <- NULL
	if(class(data) == "SimDataOut") {
		DataOut <- data
		data <- DataOut@data
	}
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
		check <- all.equal(DataOut@param, Output@param)
		if(length(check) == 1 && check == TRUE) {
			#paramOut <- DataOut@paramOut
			Output@paramValue <- DataOut@paramOut
		}
	}
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
			if(cm[1, 1] == 0) {
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
