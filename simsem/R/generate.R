# Higher level wrapper function for createData and drawParam. Everything happens in one step.

generate <- function(model, n, maxDraw=20,misfitBounds=NULL, misfitType=NULL,
                     averageNumMisspec=FALSE, optMisfit=NULL, numIter=1,
                     indDist=NULL, sequential=FALSE,
                     facDist=NULL, errorDist=NULL, indLab=NULL, modelBoot=FALSE, realData=NULL) {

  paramSet <- drawParam(model, maxDraw=maxDraw, misfitBounds=misfitBounds,misfitType=misfitType,
                        averageNumMisspec=averageNumMisspec, optMisfit=optMisfit, numIter=numIter)
  modelType <- model@modelType
  indLab <- unique(pt$rhs[pt$op=="=~"])
  data <- createData(paramSet,n,modelType=modelType,indDist=indDist,sequential=sequential,
                       facDist=facDist,errorDist=errorDist, indLab=indLab, modelBoot=modelBoot, realData=realData)
  

  return(data)
  
}
