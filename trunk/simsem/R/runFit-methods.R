# runFit
# Function -- simsem package
# Build a result object from analyzing real data
# Argument:
#	model:		Model object or model output object that will use to derive parameter (or standardized) coefficients
#	realdata:	Real data 
#	nRep:		Number of replications
#	misspec:	Misspecification object
#	empiricalMissing:	Use pattern of missing from the real data if TRUE. If FALSE, use model pattern of missing
#	missModel:	Missing object
#	usedStd:	If TRUE, use standardized parameters for result object. If FALSE, use unstandardized version.
#	For others, see simData and simResult functions
# Return: SimResult object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

setMethod("runFit", signature(model="SimModel"), definition=function(model, realdata, nRep=1000, misspec=new("NullSimMisspec"), conBeforeMis=TRUE, misfitBound=new("NullVector"), maxDraw=100, sequential=NA, facDist=new("NullSimDataDist"), errorDist=new("NullSimDataDist"), indDist=new("NullSimDataDist"), modelBoot=FALSE, seed=123321, silent=FALSE, multicore=FALSE, cluster=FALSE, numProc=NULL, empiricalMissing=TRUE, missModel=new("NullSimMissing"), usedStd=TRUE) {
	out <- run(model, realdata)
	if(empiricalMissing) {
		miss <- new("NullMatrix")
		if(isNullObject(model@indLab)) {
			miss <- is.na(realdata)
		} else {
			miss <- is.na(realdata[, model@indLab])
		}
		if(isNullObject(missModel)) {
			missModel <- simMissing(logical=miss)
		} else {
			missModel <- simMissing(numImps=missModel@numImps, logical=miss)
		}
	}
	SimData <- simData(out, misspec=misspec, conBeforeMis=conBeforeMis, misfitBound=misfitBound, maxDraw=maxDraw, sequential=sequential, facDist=facDist, errorDist=errorDist, indDist=indDist, usedStd=usedStd, realData=realdata)
	simOut <- simResult(nRep, SimData, model, objMissing=missModel, seed=seed, silent=silent, multicore=multicore, cluster=cluster, numProc=numProc)
	return(simOut)
}
)

# Example:
#library(lavaan)
#loading <- matrix(0, 9, 3)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:9, 3] <- NA
#model <- simParamCFA(LY=loading)
#SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
#u2 <- simUnif(-0.2, 0.2)
#loading.trivial <- matrix(NA, 9, 3)
#loading.trivial[is.na(loading)] <- 0
#LY.trivial <- simMatrix(loading.trivial, "u2")
#mis <- simMisspecCFA(LY = LY.trivial)
#Output <- runFit(SimModel, HolzingerSwineford1939, 20, mis)
#summary(Output)

setMethod("runFit", signature(model="SimModelOut"), definition=function(model, realdata=new("NullDataFrame"), nRep=1000, misspec=new("NullSimMisspec"), conBeforeMis=TRUE, misfitBound=new("NullVector"), maxDraw=100, sequential=NA, facDist=new("NullSimDataDist"), errorDist=new("NullSimDataDist"), indDist=new("NullSimDataDist"), modelBoot=FALSE, seed=123321, silent=FALSE, multicore=FALSE, cluster=FALSE, numProc=NULL, empiricalMissing=TRUE, missModel=new("NullSimMissing"), usedStd=TRUE) {
	SimData <- simData(model, misspec=misspec, conBeforeMis=conBeforeMis, misfitBound=misfitBound, maxDraw=maxDraw, sequential=sequential, facDist=facDist, errorDist=errorDist, indDist=indDist, usedStd=usedStd, modelBoot=modelBoot, realData=realdata)
	if(empiricalMissing) {
		miss <- new("NullMatrix")
		if(!isNullObject(realdata)) {
			if(isNullObject(model@indLab)) {
				miss <- is.na(realdata)
			} else {
				miss <- is.na(realdata[, model@indLab])
			}
			if(isNullObject(missModel)) {
				missModel <- simMissing(logical=miss)
			} else {
				missModel <- simMissing(numImps=missModel@numImps, logical=miss)
			}
		}
	}
	analyzeModel <- simModel(model@param, equalCon=model@equalCon, indLab=model@indLab) 
	simOut <- simResult(nRep, SimData, analyzeModel, objMissing=missModel, seed=seed, silent=silent, multicore=multicore, cluster=cluster, numProc=numProc)
	return(simOut)
}
)
