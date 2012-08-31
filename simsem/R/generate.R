## Higher level wrapper function for createData and drawParameters. Takes a SimSem analysis/data generation template, and returns a raw data set,
## optionally with the drawn parameter values.

generate <- function(model, n, maxDraw=50,misfitBounds=NULL, misfitType="f0",
                     averageNumMisspec=FALSE, optMisfit=NULL, optDraws=50,
                     indDist=NULL, sequential=FALSE,
                     facDist=NULL, errorDist=NULL, indLab=NULL, modelBoot=FALSE, realData=NULL, params=FALSE) {

  
  if(model@modelType == "Path") {
    indLab <- unique(model@pt$lhs)
  } else {
    indLab <- unique(model@pt$rhs[model@pt$op=="=~"])
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
  
  if(params) {
    return(list(data=data,psl=draws))
  } else {
    return(data)
  }
  
}

## draws = list of paramSet, length = number of groups
## paramSet:
## [[1]] param: parameter draws, no misspecification
## [[2]] misParam: parameter draws with misspecification
## [[3]] mis: misspecification only
## paramSet$param: reduced list of model matrices with parameter draws, or a "GLIST"
##  GLIST: PS, BE, AL, TE, LY, TY
