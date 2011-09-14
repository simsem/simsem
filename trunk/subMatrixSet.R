setClass("subMatrixSet", 
	representation(
		Tag="character", #Path, Path.exo, CFA, SEM, SEM.exo
		LY="simMatrix",
		TE="symMatrix",
		VTE="simVector",
		PS="symMatrix",
		VPS="simVector",
		BE="simMatrix",
		TY="simVector",
		AL="simVector",
		ME="simVector",
		MY="simVector",
		VE="simVector",
		VY="simVector",
		LX="simMatrix",
		TD="symMatrix",
		VTD="simVector",
		PH="symMatrix",
		GA="simMatrix",
		TX="simVector",
		KA="simVector",
		MX="simVector",
		VPH="simVector",
		VX="simVector",
		TH="simMatrix"), #Delta on rows, epsilon on columns
	prototype(
		LY=.NULL.simMatrix,
		TE=.NULL.symMatrix,
		VTE=.NULL.simVector,
		PS=.NULL.symMatrix,
		VPS=.NULL.simVector,
		BE=.NULL.simMatrix,
		TY=.NULL.simVector,
		AL=.NULL.simVector,
		ME=.NULL.simVector,
		MY=.NULL.simVector,
		VE=.NULL.simVector,
		VY=.NULL.simVector,
		LX=.NULL.simMatrix,
		TD=.NULL.symMatrix,
		VTD=.NULL.simVector,
		PH=.NULL.symMatrix,
		GA=.NULL.simMatrix,
		TX=.NULL.simVector,
		KA=.NULL.simVector,
		MX=.NULL.simVector,
		VPH=.NULL.simVector,
		VX=.NULL.simVector,
		TH=.NULL.simMatrix)
)



subMatrix.CFA.object <- function(simMatrixSet, ...) { #loading, latent.cor, error.cor, latent.var = NULL, error.var = NULL, indicator.var = NULL, intercept = NULL, indicator.mean = NULL, factor.mean = NULL) {
	if(simMatrixSet@Tag != "CFA") stop("The simMatrixSet tag is not CFA.")
	List <- list(...)
	Names <- names(List)
	keywords <- list(.loading, .error, .latent.cor, c("Variance of Measurement Error", .VTD, .VTE), c("Variance of Indicators", .VX, .VY), .intercept, .factor.mean, c("Means of Indicators", .MX, .MY), c("Variance of Factors", .VE, .VPS, .VPH))
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), ifelse(!is.null.object(simMatrixSet@LY), LY <- List[position == 1], stop("LY is not indicated in simMatrixSet.")), LY <- list(.NULL.simMatrix))
	ni <- nrow(run(simMatrixSet@LY[[1]]))
	nk <- ncol(run(simMatrixSet@LY[[1]]))
	ifelse(contain(2, position), ifelse(!is.null.object(simMatrixSet@TE), TE <- List[position == 2], stop("TE is not indicated in simMatrixSet.")), TE <- list(.NULL.symMatrix))
	ifelse(contain(3, position), ifelse(!is.null.object(simMatrixSet@PS), PS <- List[position == 3], stop("PS is not indicated in simMatrixSet.")), PS <- list(.NULL.symMatrix))
	ifelse(contain(4, position), ifelse(!is.null.object(simMatrixSet@VTE), VTE <- List[position == 4], stop("VTE is not indicated in simMatrixSet.")), VTE <- list(.NULL.simVector))
	ifelse(contain(5, position), ifelse(!is.null.object(simMatrixSet@VY), VY <- List[position == 5], stop("VY is not indicated in simMatrixSet.")), VY <- list(.NULL.simVector))
	ifelse(contain(8, position), ifelse(!is.null.object(simMatrixSet@MY), MY <- List[position == 8], stop("MY is not indicated in simMatrixSet.")), MY <- list(.NULL.simVector))
	ifelse(contain(6, position), ifelse(!is.null.object(simMatrixSet@TY), TY <- List[position == 6], stop("TY is not indicated in simMatrixSet.")), TY <- list(.NULL.simVector))
	ifelse(contain(7, position), ifelse(!is.null.object(simMatrixSet@ME), ME <- List[position == 7], stop("ME is not indicated in simMatrixSet.")), ME <- list(.NULL.simVector))
	ifelse(contain(9, position), ifelse(!is.null.object(simMatrixSet@VE), VE <- List[position == 9], stop("VE is not indicated in simMatrixSet.")), VE <- list(.NULL.simVector))
	Output <- new("subMatrixSet", LY=LY[[1]], PS=PS[[1]], TE=TE[[1]], VE=VE[[1]], VPS=VE[[1]], VTE=VTE[[1]], VY=VY[[1]], TY=TY[[1]], MY=MY[[1]], ME=ME[[1]], AL=ME[[1]], Tag="CFA")
	return(Output)
}

subMatrix.Path.object <- function(simMatrixSet, ..., exo = FALSE) {
	if(simMatrixSet@Tag != "Path" & simMatrixSet@Tag != "Path.exo") {
		stop("The simMatrixSet tag is not either Path or Path.exo.")
	}
	if(exo == FALSE & simMatrixSet@Tag != "Path") stop("The simMatrixSet tag is not Path when there is no exogenous variable.")
	if(exo == TRUE & simMatrixSet@Tag != "Path.exo") stop("The simMatrixSet tag is not Path.exo when there is exogenous variables.")
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(.BE, .PS, .VPS, .VE, .AL, .ME)
	} else {
		keywords <- list(.BE, .PS, .VPS, .VE, .AL, .ME, .GA, .PH, .VPH, .KA)
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), ifelse(!is.null.object(simMatrixSet@BE), BE <- List[position == 1], stop("BE is not indicated in simMatrixSet.")), BE <- list(.NULL.simMatrix))
	ne <- ncol(run(simMatrixSet@BE[[1]]))
	ifelse(contain(2, position), ifelse(!is.null.object(simMatrixSet@PS), PS <- List[position == 2], stop("PS is not indicated in simMatrixSet.")), PS <- list(.NULL.symMatrix))
	ifelse(contain(3, position), ifelse(!is.null.object(simMatrixSet@VPS), VPS <- List[position == 3], stop("VPS is not indicated in simMatrixSet.")), VPS <- list(.NULL.simVector))
	ifelse(contain(4, position), ifelse(!is.null.object(simMatrixSet@VE), VE <- List[position == 4], stop("VE is not indicated in simMatrixSet.")), VE <- list(.NULL.simVector))
	ifelse(contain(6, position), ifelse(!is.null.object(simMatrixSet@ME), ME <- List[position == 6], stop("ME is not indicated in simMatrixSet.")), ME <- list(.NULL.simVector))
	ifelse(contain(5, position), ifelse(!is.null.object(simMatrixSet@AL), AL <- List[position == 5], stop("AL is not indicated in simMatrixSet.")), AL <- list(.NULL.simVector))
	Output <- NULL
	if(exo) {
		ifelse(contain(7, position), ifelse(!is.null.object(simMatrixSet@GA), GA <- List[position == 7], stop("GA is not indicated in simMatrixSet.")), GA <- list(.NULL.simMatrix))
		nk <- ncol(run(simMatrixSet@GA[[1]]))
		ifelse(contain(8, position), ifelse(!is.null.object(simMatrixSet@PH), PH <- List[position == 8], stop("PH is not indicated in simMatrixSet.")), PH <- list(.NULL.symMatrix))
		ifelse(contain(9, position), ifelse(!is.null.object(simMatrixSet@VPH), VPH <- List[position == 9], stop("VPH is not indicated in simMatrixSet.")), VPH <- list(.NULL.simVector))
		ifelse(contain(10, position), ifelse(!is.null.object(simMatrixSet@KA), KA <- List[position == 10], stop("KA is not indicated in simMatrixSet.")), KA <- list(.NULL.simVector))
		Output <- new("subMatrixSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], Tag="Path.exo")	
	} else {
		Output <- new("subMatrixSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="Path")
	}
	return(Output)
}

subMatrix.SEM.object <- function(simMatrixSet, ..., exo = FALSE) {
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(.LY, .TE, .VTE, .VY, .TY, .MY, .BE, .PS, .VPS, .VE, .AL, .ME)
	} else {
		keywords <- list(.LY, .TE, .VTE, .VY, .TY, .MY, .BE, .PS, .VPS, .VE, .AL, .ME, .LX, .TD, .VTD, .VX, .TX, .MX, .GA, .PH, .VPH, .KA, .TH)
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), ifelse(!is.null.object(simMatrixSet@LY), LY <- List[position == 1], stop("LY is not indicated in simMatrixSet.")), LY <- list(.NULL.simMatrix))
	ny <- nrow(run(simMatrixSet@LY[[1]]))
	ne <- ncol(run(simMatrixSet@LY[[1]]))
	ifelse(contain(2, position), ifelse(!is.null.object(simMatrixSet@BE), TE <- List[position == 2], stop("TE is not indicated in simMatrixSet.")), TE <- list(.NULL.symMatrix))
	ifelse(contain(3, position), ifelse(!is.null.object(simMatrixSet@BE), VTE <- List[position == 3], stop("VTE is not indicated in simMatrixSet.")), VTE <- list(.NULL.simVector))
	ifelse(contain(4, position), ifelse(!is.null.object(simMatrixSet@BE), VY <- List[position == 4], stop("VY is not indicated in simMatrixSet.")), VY <- list(.NULL.simVector))
	ifelse(contain(6, position), ifelse(!is.null.object(simMatrixSet@BE), MY <- List[position == 6], stop("MY is not indicated in simMatrixSet.")), MY <- list(.NULL.simVector))
	ifelse(contain(5, position), ifelse(!is.null.object(simMatrixSet@BE), TY <- List[position == 5], stop("TY is not indicated in simMatrixSet.")), TY <- list(.NULL.simVector))
	ifelse(contain(7, position), ifelse(!is.null.object(simMatrixSet@BE), BE <- List[position == 7], stop("BE is not indicated in simMatrixSet.")), BE <- list(.NULL.simMatrix))
	ifelse(contain(8, position), ifelse(!is.null.object(simMatrixSet@BE), PS <- List[position == 8], stop("PS is not indicated in simMatrixSet.")), PS <- list(.NULL.symMatrix))
	ifelse(contain(9, position), ifelse(!is.null.object(simMatrixSet@BE), VPS <- List[position == 9], stop("VPS is not indicated in simMatrixSet.")), VPS <- list(.NULL.simVector))
	ifelse(contain(10, position), ifelse(!is.null.object(simMatrixSet@BE), VE <- List[position == 10], stop("VE is not indicated in simMatrixSet.")), VE <- list(.NULL.simVector))
	ifelse(contain(12, position), ifelse(!is.null.object(simMatrixSet@BE), ME <- List[position == 12], stop("ME is not indicated in simMatrixSet.")), ME <- list(.NULL.simVector))
	ifelse(contain(11, position), ifelse(!is.null.object(simMatrixSet@BE), AL <- List[position == 11], stop("AL is not indicated in simMatrixSet.")), AL <- list(.NULL.simVector))
	Output <- NULL
	if(exo) {
		ifelse(contain(13, position), ifelse(!is.null.object(simMatrixSet@BE), LX <- List[position == 13], stop("LX is not indicated in simMatrixSet.")), LX <- list(.NULL.simMatrix))
		nx <- nrow(run(simMatrixSet@LX[[1]]))
		nk <- ncol(run(simMatrixSet@LX[[1]]))
		ifelse(contain(14, position), ifelse(!is.null.object(simMatrixSet@BE), TD <- List[position == 14], stop("TD is not indicated in simMatrixSet.")), TD <- list(.NULL.symMatrix))
		ifelse(contain(15, position), ifelse(!is.null.object(simMatrixSet@BE), VTD <- List[position == 15], stop("VTD is not indicated in simMatrixSet.")), VTD <- list(.NULL.simVector))
		ifelse(contain(16, position), ifelse(!is.null.object(simMatrixSet@BE), VX <- List[position == 16], stop("VX is not indicated in simMatrixSet.")), VX <- list(.NULL.simVector))
		ifelse(contain(18, position), ifelse(!is.null.object(simMatrixSet@BE), MX <- List[position == 18], stop("MX is not indicated in simMatrixSet.")), MX <- list(.NULL.simVector))
		ifelse(contain(17, position), ifelse(!is.null.object(simMatrixSet@BE), TX <- List[position == 17], stop("TX is not indicated in simMatrixSet.")), TX <- list(.NULL.simVector))
		
		ifelse(contain(19, position), ifelse(!is.null.object(simMatrixSet@BE), GA <- List[position == 19], stop("GA is not indicated in simMatrixSet.")), GA <- list(.NULL.simMatrix))
		ifelse(contain(20, position), ifelse(!is.null.object(simMatrixSet@BE), PH <- List[position == 20], stop("PH is not indicated in simMatrixSet.")), PH <- list(.NULL.symMatrix))
		ifelse(contain(21, position), ifelse(!is.null.object(simMatrixSet@BE), VPH <- List[position == 21], stop("VPH is not indicated in simMatrixSet.")), VPH <- list(.NULL.simVector))
		ifelse(contain(22, position), ifelse(!is.null.object(simMatrixSet@BE), KA <- List[position == 22], stop("KA is not indicated in simMatrixSet.")), KA <- list(.NULL.simVector))
		if(contain(23, position)) {
			if(!is.null.object(simMatrixSet@TH)) {
				TH <- List[position == 23]
				temp <- run(simMatrixSet@TH[[1]])
				if(!((nrow(temp) == nx) & (ncol(temp) == ny))) stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
			} else {
				stop("TH is not indicated in simMatrixSet.")
			}
		} else {
			TH <- list(.NULL.simMatrix)
		}
		Output <- new("subMatrixSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]],
					LX=LX[[1]], TD=TD[[1]], VTD=VTD[[1]], VX=VX[[1]], MX=MX[[1]], TX=TX[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], TH=TH[[1]], Tag="SEM.exo")	
	} else {
		Output <- new("subMatrixSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="SEM")	
	}
	return(Output)
}