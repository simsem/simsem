## # runFitParam: Build a parameter result object from analyzing real data

## setMethod("runFitParam", signature(model = "SimModel"), definition = function(model, nRep = 1000, misspec = new("NullSimMisspec"), maxDraw = 100, seed = 123321, usedStd = TRUE, data) {
##     out <- run(model, data)
##     equalCon <- out@equalCon
##     usedParam <- toSimSet(out, usedStd = usedStd)
## 	paramOut <- simResultParam(nRep=nRep, object=usedParam, misspec = misspec, SimEqualCon = equalCon, seed = seed, 
##     maxDraw = maxDraw)
##     return(paramOut)
## })

## setMethod("runFitParam", signature(model = "SimModelOut"), definition = function(model, nRep = 1000, misspec = new("NullSimMisspec"), maxDraw = 100, seed = 123321, usedStd = TRUE) {
##     equalCon <- model@equalCon
##     usedParam <- toSimSet(model, usedStd = usedStd)
## 	paramOut <- simResultParam(nRep=nRep, object=usedParam, misspec = misspec, SimEqualCon = equalCon, seed = seed, 
##     maxDraw = maxDraw)
##     return(paramOut)
## }) 
