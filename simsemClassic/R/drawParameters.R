# drawParameters: Create parameter sets (with or without model misspecification) from simData object

drawParameters <- function(object) {
    drawParametersMisspec(object@param, object@misspec, object@equalCon, object@maxDraw)
}

drawParametersMisspec <- function(objSet, objMisspec = new("NullSimMisspec"), objEqualCon = new("NullSimEqualCon"), maxDraw = 100) {
    param <- NULL
    misspec <- NULL
    misspecAdd <- NULL
    implied.CM.param <- NULL
    implied.CM.misspec <- NULL
    misfit <- NULL
    count <- 0
    repeat {
        if (!isNullObject(objMisspec)) {
            Output <- runMisspec(objSet, objMisspec, objEqualCon)
            param <- Output$param
            misspec <- Output$misspec
            misspecAdd <- Output$misspecAdd
            if (validateObject(param) | validateObject(misspec)) {
                param <- reduceMatrices(param)
                misspec <- reduceMatrices(misspec)
                if (!isNullObject(param) && !isNullObject(misspec)) {
                  implied.CM.param <- createImpliedMACS(param)
                  implied.CM.misspec <- createImpliedMACS(misspec)
                  if (all(is.finite(implied.CM.misspec$CM)) && (sum(eigen(implied.CM.misspec$CM)$values <= 0) == 0)) {
                    if (isNullObject(objMisspec@misfitBound)) {
                      break
                    } else {
                      p <- length(implied.CM.param$M)
                      nElements <- p + (p * (p + 1)/2)
                      nFree <- countFreeParameters(objSet)
                      if (!isNullObject(objEqualCon)) 
                        nFree <- nFree + countFreeParameters(objEqualCon)
                      dfParam <- nElements - nFree
                      misfit <- popMisfitMACS(implied.CM.param$M, implied.CM.param$CM, implied.CM.misspec$M, implied.CM.misspec$CM, fit.measures = objMisspec@misfitType, 
                        dfParam = dfParam)
                      if (objMisspec@averageNumMisspec) 
                        misfit <- misfit/countFreeParameters(objMisspec)
                      
                      if (!is.null(misfit) && (misfit > objMisspec@misfitBound[1] & misfit < objMisspec@misfitBound[2])) 
                        break
                    }
                  }
                }
            }
        } else {
            param <- run(objSet, equalCon = objEqualCon)
            if (validateObject(param)) {
                param <- reduceMatrices(param)
                if (!isNullObject(param)) {
                  implied.CM.param <- createImpliedMACS(param)
                  implied.CM.misspec <- implied.CM.param
                  if (sum(eigen(implied.CM.param$CM)$values <= 0) == 0) 
                    break
                }
            }
        }
        count <- count + 1
        if (count > maxDraw) 
            stop("The model cannot make a good set of parameters within limit of maximum random sampling of parameters")
    }
    return(list(real = param, misspec = misspec, misspecAdd = misspecAdd))
} 
