subMatrix.SEM.object <- function(simMatrixSet, ..., exo = FALSE) {
	W <- get.keywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$LY, W$TE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME)
	} else {
		keywords <- list(W$LY, W$TE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME, W$LX, W$TD, W$VTD, W$VX, W$TX, W$MX, W$GA, W$PH, W$VPH, W$KA, W$TH)
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), ifelse(!is.null.object(simMatrixSet@LY), LY <- List[position == 1], stop("LY is not indicated in simMatrixSet.")), LY <- list(new("nullSimMatrix")))
	ny <- nrow(run(simMatrixSet@LY[[1]]))
	ne <- ncol(run(simMatrixSet@LY[[1]]))
	ifelse(contain(2, position), ifelse(!is.null.object(simMatrixSet@BE), TE <- List[position == 2], stop("TE is not indicated in simMatrixSet.")), TE <- list(new("nullSymMatrix")))
	ifelse(contain(3, position), ifelse(!is.null.object(simMatrixSet@BE), VTE <- List[position == 3], stop("VTE is not indicated in simMatrixSet.")), VTE <- list(new("nullSimVector")))
	ifelse(contain(4, position), ifelse(!is.null.object(simMatrixSet@BE), VY <- List[position == 4], stop("VY is not indicated in simMatrixSet.")), VY <- list(new("nullSimVector")))
	ifelse(contain(6, position), ifelse(!is.null.object(simMatrixSet@BE), MY <- List[position == 6], stop("MY is not indicated in simMatrixSet.")), MY <- list(new("nullSimVector")))
	ifelse(contain(5, position), ifelse(!is.null.object(simMatrixSet@BE), TY <- List[position == 5], stop("TY is not indicated in simMatrixSet.")), TY <- list(new("nullSimVector")))
	ifelse(contain(7, position), ifelse(!is.null.object(simMatrixSet@BE), BE <- List[position == 7], stop("BE is not indicated in simMatrixSet.")), BE <- list(new("nullSimMatrix")))
	ifelse(contain(8, position), ifelse(!is.null.object(simMatrixSet@BE), PS <- List[position == 8], stop("PS is not indicated in simMatrixSet.")), PS <- list(new("nullSymMatrix")))
	ifelse(contain(9, position), ifelse(!is.null.object(simMatrixSet@BE), VPS <- List[position == 9], stop("VPS is not indicated in simMatrixSet.")), VPS <- list(new("nullSimVector")))
	ifelse(contain(10, position), ifelse(!is.null.object(simMatrixSet@BE), VE <- List[position == 10], stop("VE is not indicated in simMatrixSet.")), VE <- list(new("nullSimVector")))
	ifelse(contain(12, position), ifelse(!is.null.object(simMatrixSet@BE), ME <- List[position == 12], stop("ME is not indicated in simMatrixSet.")), ME <- list(new("nullSimVector")))
	ifelse(contain(11, position), ifelse(!is.null.object(simMatrixSet@BE), AL <- List[position == 11], stop("AL is not indicated in simMatrixSet.")), AL <- list(new("nullSimVector")))
	Output <- NULL
	if(exo) {
		ifelse(contain(13, position), ifelse(!is.null.object(simMatrixSet@BE), LX <- List[position == 13], stop("LX is not indicated in simMatrixSet.")), LX <- list(new("nullSimMatrix")))
		nx <- nrow(run(simMatrixSet@LX[[1]]))
		nk <- ncol(run(simMatrixSet@LX[[1]]))
		ifelse(contain(14, position), ifelse(!is.null.object(simMatrixSet@BE), TD <- List[position == 14], stop("TD is not indicated in simMatrixSet.")), TD <- list(new("nullSymMatrix")))
		ifelse(contain(15, position), ifelse(!is.null.object(simMatrixSet@BE), VTD <- List[position == 15], stop("VTD is not indicated in simMatrixSet.")), VTD <- list(new("nullSimVector")))
		ifelse(contain(16, position), ifelse(!is.null.object(simMatrixSet@BE), VX <- List[position == 16], stop("VX is not indicated in simMatrixSet.")), VX <- list(new("nullSimVector")))
		ifelse(contain(18, position), ifelse(!is.null.object(simMatrixSet@BE), MX <- List[position == 18], stop("MX is not indicated in simMatrixSet.")), MX <- list(new("nullSimVector")))
		ifelse(contain(17, position), ifelse(!is.null.object(simMatrixSet@BE), TX <- List[position == 17], stop("TX is not indicated in simMatrixSet.")), TX <- list(new("nullSimVector")))
		
		ifelse(contain(19, position), ifelse(!is.null.object(simMatrixSet@BE), GA <- List[position == 19], stop("GA is not indicated in simMatrixSet.")), GA <- list(new("nullSimMatrix")))
		ifelse(contain(20, position), ifelse(!is.null.object(simMatrixSet@BE), PH <- List[position == 20], stop("PH is not indicated in simMatrixSet.")), PH <- list(new("nullSymMatrix")))
		ifelse(contain(21, position), ifelse(!is.null.object(simMatrixSet@BE), VPH <- List[position == 21], stop("VPH is not indicated in simMatrixSet.")), VPH <- list(new("nullSimVector")))
		ifelse(contain(22, position), ifelse(!is.null.object(simMatrixSet@BE), KA <- List[position == 22], stop("KA is not indicated in simMatrixSet.")), KA <- list(new("nullSimVector")))
		if(contain(23, position)) {
			if(!is.null.object(simMatrixSet@TH)) {
				TH <- List[position == 23]
				temp <- run(simMatrixSet@TH[[1]])
				if(!((nrow(temp) == nx) & (ncol(temp) == ny))) stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
			} else {
				stop("TH is not indicated in simMatrixSet.")
			}
		} else {
			TH <- list(new("nullSimMatrix"))
		}
		Output <- new("subMatrixSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]],
					LX=LX[[1]], TD=TD[[1]], VTD=VTD[[1]], VX=VX[[1]], MX=MX[[1]], TX=TX[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], TH=TH[[1]], Tag="SEM.exo")	
	} else {
		Output <- new("subMatrixSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="SEM")	
	}
	return(Output)
}