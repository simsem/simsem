
# Empty defaults to matrix with dimensions (0,0)
setClass("SimMatrix",
         representation(
                        free = "matrix",
                        popParam = "matrix",
                        misspec = "matrix",
                        symmetric = "logical"
                        #prior = "matrix"
                        )
         )

# Empty defaults to vector of length 0
setClass("SimVector",
         representation(
                        free = "vector",
                        popParam = "vector",
                        misspec = "vector"
                        #prior = "vector"
                        )
         )

setClass("SimSem",
   representation(
                  pt = "list",
                  #npt = "list",
                  dgen = "list",
                  modelType = "character",
				  groupLab = "character"
                  )
         )
          
 setClass("SimDataDist", representation(p = "numeric", margins = "character", paramMargins = "list", keepScale = "logical", reverse = "vector"), prototype(keepScale = TRUE, reverse = FALSE))

setClass("SimResult",
         representation(
                        modelType = "character",
                        nRep = "numeric",
                        coef = "data.frame",
                        se = "data.frame",
                        fit = "data.frame",
                        converged = "vector",
                        paramValue = "data.frame",
						misspecValue = "data.frame",
						popFit = "data.frame",
                        FMI1 = "data.frame",
                        FMI2 = "data.frame",
                        stdCoef = "data.frame",
                        seed = "numeric",
                        n = "vector",
                        pmMCAR = "vector",
                        pmMAR = "vector",
						extraOut = "list",
                        timing = "list"
                        )
         )

setClass("SimMissing",
         representation(
                        cov = "vector", pmMCAR = "numeric", pmMAR = "numeric",
                        nforms = "numeric", itemGroups = "list", twoMethod = "vector",
                        prAttr = "vector", package = "character",
                        timePoints = "numeric", ignoreCols = "vector", threshold = "numeric",
                        covAsAux = "logical", logical = "matrix", args="list"
                        )
         )
