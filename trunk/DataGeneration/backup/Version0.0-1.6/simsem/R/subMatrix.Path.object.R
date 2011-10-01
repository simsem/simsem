subMatrix.Path.object <- function(simMatrixSet, ..., exo = FALSE) {
	W <- get.keywords()
	if(simMatrixSet@Tag != "Path" & simMatrixSet@Tag != "Path.exo") {
		stop("The simMatrixSet tag is not either Path or Path.exo.")
	}
	if(exo == FALSE & simMatrixSet@Tag != "Path") stop("The simMatrixSet tag is not Path when there is no exogenous variable.")
	if(exo == TRUE & simMatrixSet@Tag != "Path.exo") stop("The simMatrixSet tag is not Path.exo when there is exogenous variables.")
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME)
	} else {
		keywords <- list(W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME, W$GA, W$PH, W$VPH, W$KA)
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), ifelse(!is.null.object(simMatrixSet@BE), BE <- List[position == 1], stop("BE is not indicated in simMatrixSet.")), BE <- list(new("nullSimMatrix")))
	ne <- ncol(run(simMatrixSet@BE[[1]]))
	ifelse(contain(2, position), ifelse(!is.null.object(simMatrixSet@PS), PS <- List[position == 2], stop("PS is not indicated in simMatrixSet.")), PS <- list(new("nullSymMatrix")))
	ifelse(contain(3, position), ifelse(!is.null.object(simMatrixSet@VPS), VPS <- List[position == 3], stop("VPS is not indicated in simMatrixSet.")), VPS <- list(new("nullSimVector")))
	ifelse(contain(4, position), ifelse(!is.null.object(simMatrixSet@VE), VE <- List[position == 4], stop("VE is not indicated in simMatrixSet.")), VE <- list(new("nullSimVector")))
	ifelse(contain(6, position), ifelse(!is.null.object(simMatrixSet@ME), ME <- List[position == 6], stop("ME is not indicated in simMatrixSet.")), ME <- list(new("nullSimVector")))
	ifelse(contain(5, position), ifelse(!is.null.object(simMatrixSet@AL), AL <- List[position == 5], stop("AL is not indicated in simMatrixSet.")), AL <- list(new("nullSimVector")))
	Output <- NULL
	if(exo) {
		ifelse(contain(7, position), ifelse(!is.null.object(simMatrixSet@GA), GA <- List[position == 7], stop("GA is not indicated in simMatrixSet.")), GA <- list(new("nullSimMatrix")))
		nk <- ncol(run(simMatrixSet@GA[[1]]))
		ifelse(contain(8, position), ifelse(!is.null.object(simMatrixSet@PH), PH <- List[position == 8], stop("PH is not indicated in simMatrixSet.")), PH <- list(new("nullSymMatrix")))
		ifelse(contain(9, position), ifelse(!is.null.object(simMatrixSet@VPH), VPH <- List[position == 9], stop("VPH is not indicated in simMatrixSet.")), VPH <- list(new("nullSimVector")))
		ifelse(contain(10, position), ifelse(!is.null.object(simMatrixSet@KA), KA <- List[position == 10], stop("KA is not indicated in simMatrixSet.")), KA <- list(new("nullSimVector")))
		Output <- new("subMatrixSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], Tag="Path.exo")	
	} else {
		Output <- new("subMatrixSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="Path")
	}
	return(Output)
}
