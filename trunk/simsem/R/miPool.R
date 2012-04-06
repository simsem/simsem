# miPoolVector
# Function -- simsem package
# Pool MI results that providing in matrix or vector formats
# Argument:
#	MI.param: 	Coefficients matrix (row = imputation, col = parameters)
#	MI.se:		Standard errors matrix (row = imputation, col = parameters)
#	imps:		Number of imputations
# Return:
#	coef:	Parameter estimates
#	se:		Standard error combining the between and within variances
#	FMI.1:	Fraction missing?
#	FMI.2:	Fraction missing?
# Author: 	Mijke Rhumtella
#			Alex Schoemann
#			Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 8, 2012

miPoolVector <- function(MI.param, MI.se, imps) {
   #compute parameter estimates
  Estimates <- colMeans(MI.param)

#compute between-imputation variance: variance of parameter estimates
  Bm <- apply(MI.param,2,var)

#compute within-imputation variance: average of squared estimated SEs 
#Um <- colSums(MI.se^2/m)
  Um <- apply(MI.se^2,2,mean)

#Total variance
#Tm <- Um + (Bm)*((1+m)/m+1)
#compute total variance: sum of between- and within- variance with correction
  TV <- Um + ((imps+1)/imps)*Bm

#compute correction factor for fraction of missing info
  nu <- (imps-1)*((((1+1/imps)*Bm)/TV)^-2)

#compute 2 estimates of fraction of missing information
  FMI.1 <- 1-(Um/TV)
  FMI.2 <- 1- ((nu+1)*Um)/((nu+3)*TV)
  FMI.2[is.nan(FMI.2)] <- 0
  FMI<-rbind(FMI.1,FMI.2)

#Get rid of estimates from fixed variables
#fixedParam <- Bm==0

#Estimates <- subset(Estimates, !fixedParam)
#TV <- subset(TV, !fixedParam)
#FMI.1 <- subset(FMI.1, !fixedParam)
#FMI.2 <- subset(FMI.2, !fixedParam)
SE <- sqrt(TV)
MI.res<-list(Estimates,SE,FMI.1,FMI.2)
names(MI.res)<-c('coef','se','FMI.1','FMI.2')
#compute chi-square proportion (is this useful?)
#(MI.fit.mat$chisq.p is a placeholder for however we'll index the p-value of chi square)
#chisq <- sum(MI.fit.mat$chisq.pval<.05)/m
  return(MI.res)
}
#Examples:
#param <- matrix(c(0.7, 0.1, 0.5,
#					0.75, 0.12, 0.54,
#					0.66, 0.11, 0.56,
#					0.74, 0.09, 0.55), nrow=4, byrow=T)
#SE <- matrix(c(0.1, 0.01, 0.05,
#				0.11, 0.023, 0.055,
#				0.10, 0.005, 0.04,
#				0.14, 0.012, 0.039), nrow=4, byrow=T)
#nimps <- 4
#miPoolVector(param, SE, nimps)

# miPoolChi
# Function -- simsem package
# Pool Chi-square statistic based on Li, Meng, Raghunathan, & Rubin (1991) adapted from http://psychology.clas.asu.edu/files/CombiningLikelihoodRatioChi-SquareStatisticsFromaMIAnalysis.sas
# Argument:
#	chis: 	vector of chi-square values
#	df:		degree of freedom
# Author: 	Craig Enders
#			Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 31, 2012

miPoolChi <- function(chis, df) {
	# From Li, Meng, Raghunathan, & Rubin (1991)
	if(is.matrix(chis)) {
		ifelse(ncol(chis) == 1 | nrow(chis) == 1, chis <- as.vector(chis), stop("Please put a vector of chi-square values"))
	}
	m <- length(chis)
	dbar <- mean(chis)
	sqrtd <- sqrt(chis)
	xbarsqrtd <- mean(sqrtd)
	# Equation 2.2
	r <- (1 + 1/m) * (sum((sqrtd - xbarsqrtd)^2)/(m - 1))
	# Equation 2.1
	D <- (dbar/df - ((m + 1) * r /(m - 1)))/(1 + r)
	if(D < 0) D <- 0
	# Equation 2.16 and 2.17
	aw <- df^(-(3/m)) * (m - 1) * (1 + (1/r))^2
	p <- 1 - pf(D, df, aw)
	result <- c(D, df, aw, p)
	names(result) <- c("F", "df1", "df2", "p.F")
	return(result)
}
#Examples:
#miPoolChi(c(89.864, 81.116,71.500,49.022,61.986,64.422,55.256,57.890,79.416,63.944), 2)


# miPool
# Function -- simsem package
# Pool MI results in SimModelOut class format
# Argument:
#	Result.l: 	List of MI results
# Return:
#	output: 	SimModelMIOut that provides output and fraction missing information
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 11, 2012

miPool <- function(Result.l) {
	Converged <- sapply(Result.l, function(object) {object@converged})

	allNames <- slotNames(Result.l[[which(Converged==TRUE)[1]]]@param)
	paramNames <- allNames != "modelType"
	paramNames <- allNames[paramNames]

	OutputCoef <- Result.l[[which(Converged==TRUE)[1]]]@coef
	OutputSE <- Result.l[[which(Converged==TRUE)[1]]]@se
	OutputFMI1 <- Result.l[[which(Converged==TRUE)[1]]]@se
	OutputFMI2 <- Result.l[[which(Converged==TRUE)[1]]]@se

	for(i in 1:length(paramNames)) {
		if(!isNullObject(slot(OutputCoef, paramNames[i]))) {
			mparam <- as.matrix(sapply(Result.l, function(result, slotname1, slotname2) { slot(slot(result, slotname1), slotname2)}, slotname1 = "param", slotname2 = paramNames[i]))
			if(ncol(mparam) == 1) mparam <- t(mparam) # Prevent single element matrix problem
			mparam <- mparam[,1] 
			mcoef <- as.matrix(sapply(Result.l, function(result, slotname1, slotname2) { slot(slot(result, slotname1), slotname2)}, slotname1 = "coef", slotname2 = paramNames[i]))
			if(ncol(mcoef) == 1) mcoef <- t(mcoef) # Prevent single element matrix problem
			mcoef <- mcoef[is.na(mparam),]
			mse <- as.matrix(sapply(Result.l, function(result, slotname1, slotname2) { slot(slot(result, slotname1), slotname2)}, slotname1 = "se", slotname2 = paramNames[i]))
			if(ncol(mse) == 1) mse <- t(mse) # Prevent single element matrix problem
			mse <- mse[is.na(mparam),]
			temp <- miPoolVector(t(mcoef), t(mse), length(Result.l))
			temp1 <- as.vector(slot(OutputCoef, paramNames[i]))
			temp2 <- as.vector(slot(OutputSE, paramNames[i]))
			temp3 <- as.vector(slot(OutputFMI1, paramNames[i]))
			temp4 <- as.vector(slot(OutputFMI2, paramNames[i]))
			temp1[which(is.na(mparam))] <- temp$coef
			temp2[which(is.na(mparam))] <- temp$se
			temp3[which(is.na(mparam))] <- temp$FMI.1
			temp4[which(is.na(mparam))] <- temp$FMI.2
			if(is.matrix(slot(OutputCoef, paramNames[i]))) {
				numcol <- ncol((slot(OutputCoef, paramNames[i])))
				slot(OutputCoef, paramNames[i]) <- matrix(temp1, ncol=numcol)
				slot(OutputSE, paramNames[i]) <- matrix(temp2, ncol=numcol)
				slot(OutputFMI1, paramNames[i]) <- matrix(temp3, ncol=numcol)
				slot(OutputFMI2, paramNames[i]) <- matrix(temp4, ncol=numcol)
			} else {
				slot(OutputCoef, paramNames[i]) <- temp1
				slot(OutputSE, paramNames[i]) <- temp2
				slot(OutputFMI1, paramNames[i]) <- temp3
				slot(OutputFMI2, paramNames[i]) <- temp4
			}
		}
	}
	conv <- mean(Converged, na.rm=TRUE) > 0.8
	start <- Result.l[[which(Converged==TRUE)[1]]]@start
	equalCon <- Result.l[[which(Converged==TRUE)[1]]]@equalCon
	package <- Result.l[[which(Converged==TRUE)[1]]]@package
	param <- Result.l[[which(Converged==TRUE)[1]]]@param

	Fit <- sapply(Result.l, function(object) {object@fit})
	dfPool <- Fit["df", 1]
	nullDfPool <- Fit["baseline.df", 1]
	chiPool <- miPoolChi(Fit["Chi", Converged], dfPool)
	nullChiPool <- miPoolChi(Fit["baseline.Chi", Converged], nullDfPool)
	OutputFit <- fitMeasures(X2=chiPool["df1"]*chiPool["F"], df=chiPool["df1"], p=chiPool["p.F"], 
					X2.null=nullChiPool["df1"]*nullChiPool["F"], df.null=nullChiPool["df1"], p.null=nullChiPool["p.F"], 
					N=Result.l[[1]]@n, fit.measures="all")
	toGetAverage <- setdiff(rownames(Fit), names(OutputFit))
	OutputFit2 <- rowMeans(Fit[toGetAverage, Converged], na.rm=TRUE)
	OutputFit <- c(OutputFit, OutputFit2)[rownames(Fit)]
	names(nullChiPool) <- paste("baseline.", names(nullChiPool), sep="")
	OutputFit <- c(OutputFit, chiPool, nullChiPool)
	return(new("SimModelMIOut", param=param, start=start,
			equalCon=equalCon, package=package, coef=OutputCoef,
			fit=OutputFit, se=OutputSE, converged=conv,
			FMI1=OutputFMI1, FMI2=OutputFMI2
			))
}


  
