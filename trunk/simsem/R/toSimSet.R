# toSimSet
# Function -- simsem package
# Create a SimSet object from SimModelOut
# Argument:
#	out:	The SimModelOut object that users need to transform it to SimSet
#	usedStd:	TRUE for using standardized parameter. FALSE for using unstandardized parameter
# Return: 	SimSet class 
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

toSimSet <- function(out, usedStd=TRUE) { 
	start <- out@coef
	if(usedStd) start <- standardize(out)
	param <- out@param
	LY <- new("NullSimMatrix")
	TY <- new("NullSimVector")
	BE <- new("NullSimMatrix")
	AL <- new("NullSimVector")
	TE <- new("NullSymMatrix")
	PS <- new("NullSymMatrix")
	LX <- new("NullSimMatrix")
	TX <- new("NullSimVector")
	GA <- new("NullSimMatrix")
	KA <- new("NullSimVector")
	TD <- new("NullSymMatrix")
	PH <- new("NullSymMatrix")
	TH <- new("NullSimMatrix")
	if(!isNullObject(param@LY)) LY <- simMatrix(free = param@LY, param = start@LY)
	if(!isNullObject(param@TY)) TY <- simVector(free = param@TY, param = start@TY)
	if(!isNullObject(param@BE)) BE <- simMatrix(free = param@BE, param = start@BE)
	if(!isNullObject(param@AL)) AL <- simVector(free = param@AL, param = start@AL)
	if(!isNullObject(param@TE)) TE <- symMatrix(free = param@TE, param = start@TE)
	if(!isNullObject(param@PS)) PS <- symMatrix(free = param@PS, param = start@PS)
	if(!isNullObject(param@LX)) LX <- simMatrix(free = param@LX, param = start@LX)
	if(!isNullObject(param@TX)) TX <- simVector(free = param@TX, param = start@TX)
	if(!isNullObject(param@GA)) GA <- simMatrix(free = param@GA, param = start@GA)
	if(!isNullObject(param@KA)) KA <- simVector(free = param@KA, param = start@KA)
	if(!isNullObject(param@TD)) TD <- symMatrix(free = param@TD, param = start@TD)
	if(!isNullObject(param@PH)) PH <- symMatrix(free = param@PH, param = start@PH)
	if(!isNullObject(param@TH)) TH <- simMatrix(free = param@TH, param = start@TH)
	modelType <- param@modelType
	result <- NULL
	if(modelType == "CFA") {
		result <- simSetCFA(LY=LY, TY=TY, AL=AL, TE=TE, PS=PS)
	} else if (modelType == "Path") {
		result <- simSetPath(BE=BE, AL=AL, PS=PS)
	} else if (modelType == "Path.exo") {
		result <- simSetPath(BE=BE, AL=AL, PS=PS, GA=GA, KA=KA, PH=PH, exo=TRUE)
	} else if (modelType == "SEM") {
		result <- simSetSEM(LY=LY, TY=TY, BE=BE, AL=AL, TE=TE, PS=PS)	
	} else if (modelType == "SEM.exo") {
		result <- simSetSEM(LY=LY, TY=TY, BE=BE, AL=AL, TE=TE, PS=PS, LX=LX, TX=TX, GA=GA, KA=KA, TD=TD, PH=PH, TH=TH, exo=TRUE)	
	} else {
		stop("Something is wrong!")
	}
	return(result)
}

#Examples:
#library(lavaan)
#hs <- HolzingerSwineford1939
#loading <- matrix(0, 9, 3)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:9, 3] <- NA
#model <- simParamCFA(LY=loading)
#SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
#out <- run(SimModel, hs)
#set <- toSimSet(out)

