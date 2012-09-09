## Higher level wrapper function for createData and drawParameters. Takes a SimSem analysis/data generation template, and returns a raw data set,
## optionally with the drawn parameter values.

generate <- function(model, n, maxDraw=50,misfitBounds=NULL, misfitType="f0",
                     averageNumMisspec=FALSE, optMisfit=NULL, optDraws=50,
                     indDist=NULL, sequential=FALSE,
                     facDist=NULL, errorDist=NULL, indLab=NULL, modelBoot=FALSE, realData=NULL, params=FALSE) {
  if(is.null(indLab)) {
	  if(model@modelType == "Path") {
		indLab <- unique(model@pt$lhs)
	  } else {
		indLab <- unique(model@pt$rhs[model@pt$op=="=~"])
	  }
  }
  free <- max(model@pt$free)
  ngroups <- max(model@pt$group)

  # Wrap distributions in lists for mg
  if(!class(indDist) == "list") {indDist <- rep(list(indDist),ngroups) }
  if(!class(facDist) == "list") {facDist <- rep(list(facDist),ngroups) }
  if(!class(errorDist) == "list") {errorDist <- rep(list(errorDist),ngroups) }
  
  
  draws <- draw(model, maxDraw=maxDraw, misfitBounds=misfitBounds, misfitType=misfitType,
                averageNumMisspec=averageNumMisspec, optMisfit=optMisfit, optDraws=optDraws)
  datal <- mapply(FUN=createData,draws,indDist,facDist,errorDist,
                  MoreArgs=list(n=n, sequential=sequential, modelBoot=modelBoot,realData=realData,indLab=indLab), SIMPLIFY=FALSE)
  data <- do.call("rbind",datal)
  data <- cbind(data,group=rep(1:ngroups,each=n))
  colnames(data)[ncol(data)] <- model@groupLab
  
  if(params) {
    return(list(data=data,psl=draws))
  } else {
    return(data)
  }
  
}

popMisfitParams <- function(psl, df=NULL) {
	ngroups <- length(psl)
	real <- lapply(psl,"[[",1)
	realmis <- lapply(psl,"[[",2)
	macsreal <- lapply(real,createImpliedMACS)
	macsmis <- lapply(realmis,createImpliedMACS)
	misfit <- popMisfitMACS(paramM = lapply(macsreal,"[[",1),
						  paramCM = lapply(macsreal,"[[",2),
						  misspecM = lapply(macsmis, "[[",1),
						  misspecCM = lapply(macsmis,"[[",2),
						  fit.measures = "all", dfParam = df)
	return(misfit)
}
