# setPopulation: Set population parameter values

setPopulation <- function(target, population) {
	psl <- generate(population, n=20, params=TRUE)$psl
	paramSet <- lapply(psl,"[[",1)
	indLabGen <- NULL
	if(population@modelType == "Path") {
		indLabGen <- unique(population@pt$lhs)
	} else {
		indLabGen <- unique(population@pt$rhs[population@pt$op=="=~"])
	}
	facLabGen <- NULL
	if(population@modelType != "Path") {
		facLabGen <- unique(population@pt$lhs[population@pt$op=="=~"])
	}
	popParam <- reduceParamSet(paramSet,population@dgen, indLabGen, facLabGen)
    target@paramValue <- as.data.frame(t(data.frame(param = popParam)))
    return(target)
}
