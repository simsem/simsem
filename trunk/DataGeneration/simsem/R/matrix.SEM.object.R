matrix.SEM.object <-
function(..., exo = FALSE) {
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
	ifelse(contain(1, position), LY <- List[position == 1], stop("No loading object of indicator.Y from factor.ETA in SEM"))
	ny <- nrow(run(LY[[1]]))
	ne <- ncol(run(LY[[1]]))
	ifelse(contain(2, position), TE <- List[position == 2], stop("No measurement error correlation object between indicator.Y"))
	ifelse(contain(3, position), VTE <- List[position == 3], VTE <- list(.NULL.simVector))
	ifelse(contain(4, position), VY <- List[position == 4], ifelse(is.null.object(VTE[[1]]), { VY <- list(constant.vector(1, ny)); comment(VY[[1]]) <- "default"}, VY <- list(.NULL.simVector)))
	ifelse(contain(6, position), MY <- List[position == 6], MY <- list(.NULL.simVector))
	ifelse(contain(5, position), TY <- List[position == 5], ifelse(is.null.object(MY[[1]]), { TY <- list(constant.vector(0, ny)); comment(TY[[1]]) <- "default"}, TY <- list(.NULL.simVector)))
	ifelse(contain(7, position), BE <- List[position == 7], stop("No path coefficient object between factor.ETA"))
	ifelse(contain(8, position), PS <- List[position == 8], stop("No residual correlation object between factor.ETA"))
	ifelse(contain(9, position), VPS <- List[position == 9], VPS <- list(.NULL.simVector))
	ifelse(contain(10, position), VE <- List[position == 10], ifelse(is.null.object(VPS[[1]]), { VE <- list(constant.vector(1, ne)); comment(VE[[1]]) <- "default"}, VE <- list(.NULL.simVector)))
	ifelse(contain(12, position), ME <- List[position == 12], ME <- list(.NULL.simVector))
	ifelse(contain(11, position), AL <- List[position == 11], ifelse(is.null.object(ME[[1]]), { AL <- list(constant.vector(0, ne)); comment(AL[[1]]) <- "default"}, AL <- list(.NULL.simVector)))
	Output <- NULL
	if(exo) {
		ifelse(contain(13, position), LX <- List[position == 13], stop("No loading object of indicator.X from factor.KSI in SEM"))
		nx <- nrow(run(LX[[1]]))
		nk <- ncol(run(LX[[1]]))
		ifelse(contain(14, position), TD <- List[position == 14], stop("No measurement error correlation object between indicator.Y"))
		ifelse(contain(15, position), VTD <- List[position == 15], VTD <- list(.NULL.simVector))
		ifelse(contain(16, position), VX <- List[position == 16], ifelse(is.null.object(VTD[[1]]), { VX <- list(constant.vector(1, nx)); comment(VX[[1]]) <- "default"}, VX <- list(.NULL.simVector)))
		ifelse(contain(18, position), MX <- List[position == 18], MX <- list(.NULL.simVector))
		ifelse(contain(17, position), TX <- List[position == 17], ifelse(is.null.object(MX[[1]]), { TX <- list(constant.vector(0, nx)); comment(TX[[1]]) <- "default"}, TX <- list(.NULL.simVector)))
		
		ifelse(contain(19, position), GA <- List[position == 19], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
		ifelse(contain(20, position), PH <- List[position == 20], stop("No correlation object between factor.KSI"))
		ifelse(contain(21, position), VPH <- List[position == 21], { VPH <- list(constant.vector(1, nk)); comment(VPH[[1]]) <- "default"})
		ifelse(contain(22, position), KA <- List[position == 22], { KA <- list(constant.vector(0, nk)); comment(KA[[1]]) <- "default"})
		if(contain(23, position)) {
			TH <- List[position == 23]
			temp <- run(TH[[1]])
			if(!((nrow(temp) == nx) & (ncol(temp) == ny))) stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
		} else {
			TH.Data <- matrix(0, nx, ny)
			TH.Labels <- matrix(NA, nx, ny)
			TH <- list(new("simMatrix", Data=TH.Data, Labels=TH.Labels))
			comment(TH[[1]]) <- "default"
		}
		Output <- new("simMatrixSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]],
					LX=LX[[1]], TD=TD[[1]], VTD=VTD[[1]], VX=VX[[1]], MX=MX[[1]], TX=TX[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], TH=TH[[1]], Tag="SEM.exo")	
	} else {
		Output <- new("simMatrixSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="SEM")	
	}
	return(Output)
}

