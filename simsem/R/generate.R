## Higher level wrapper function for createData and drawParameters. Takes a SimSem analysis/data generation template, and returns a raw data set,
## optionally with the drawn parameter values.

generate <- function(model, n, maxDraw=20,misfitBounds=NULL, misfitType=NULL,
                     averageNumMisspec=FALSE, optMisfit=NULL, numIter=1,
                     indDist=NULL, sequential=FALSE,
                     facDist=NULL, errorDist=NULL, indLab=NULL, modelBoot=FALSE, realData=NULL, params=FALSE) {

  modelType <- model@modelType
  indLab <- unique(model@pt$rhs[pt$op=="=~"])
  free <- max(model@pt$free)
  ngroups <- max(model@pt$group)

  if(ngroups > 1) {
    psl <- lapply(model@dgen,drawParameters,free=free, modelType=modelType, maxDraw=maxDraw, misfitBounds=misfitBounds,misfitType=misfitType,
                  averageNumMisspec=averageNumMisspec, optMisfit=optMisfit, numIter=numIter)
    datal <- lapply(psl,createData,n,modelType=modelType,indDist=indDist,sequential=sequential,
                    facDist=facDist,errorDist=errorDist, indLab=indLab, modelBoot=modelBoot, realData=realData)
    data <- do.call("rbind",datal)
  } else {
    paramSet <- drawParameters(model@dgen, free=free, modelType=modelType, maxDraw=maxDraw, misfitBounds=misfitBounds,misfitType=misfitType,
                          averageNumMisspec=averageNumMisspec, optMisfit=optMisfit, numIter=numIter)
    
    data <- createData(paramSet,n,modelType=modelType,indDist=indDist,sequential=sequential,
                       facDist=facDist,errorDist=errorDist, indLab=indLab, modelBoot=modelBoot, realData=realData)
  }
  data <- cbind(data,group=rep(1:ngroups,each=n))
  if(params) {
    return(list(data=data,psl=psl))
  } else {
    return(data)
  }
  
}

## psl = list of paramSet, length = number of groups
## paramSet:
## [[1]] param: parameter draws, no misspecification
## [[2]] misParam: parameter draws with misspecification
## [[3]] mis: misspecification only
## paramSet$param: reduced list of model matrices with parameter draws, or a "GLIST"
##  GLIST: PS, BE, AL, TE, LY, TY
