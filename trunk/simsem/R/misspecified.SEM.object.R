misspecified.SEM.object <- function(..., exo = FALSE) {
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
	ifelse(contain(1, position), LY <- List[position == 1], LY <- list(new("nullSimMatrix")))
	ifelse(contain(2, position), TE <- List[position == 2], TE <- list(new("nullSymMatrix")))
	ifelse(contain(3, position), VTE <- List[position == 3], VTE <- list(new("nullSimVector")))
	ifelse(contain(4, position), VY <- List[position == 4], VY <- list(new("nullSimVector")))
	if(!is.null.object(VTE[[1]]) & !is.null.object(VY[[1]])) stop("Please assign either VTE or VY, not both")
	ifelse(contain(6, position), MY <- List[position == 6], MY <- list(new("nullSimVector")))
	ifelse(contain(5, position), TY <- List[position == 5], TY <- list(new("nullSimVector")))
	if(!is.null.object(MY[[1]]) & !is.null.object(TY[[1]])) stop("Please assign either MY or TY, not both")
	ifelse(contain(7, position), BE <- List[position == 7], BE <- list(new("nullSimMatrix")))
	ifelse(contain(8, position), PS <- List[position == 8], PS <- list(new("nullSymMatrix")))
	ifelse(contain(9, position), VPS <- List[position == 9], VPS <- list(new("nullSimVector")))
	ifelse(contain(10, position), VE <- List[position == 10], VE <- list(new("nullSimVector")))
	if(!is.null.object(VPS[[1]]) & !is.null.object(VE[[1]])) stop("Please assign either VPS or VE, not both")
	ifelse(contain(12, position), ME <- List[position == 12], ME <- list(new("nullSimVector")))
	ifelse(contain(11, position), AL <- List[position == 11], AL <- list(new("nullSimVector")))
	if(!is.null.object(ME[[1]]) & !is.null.object(AL[[1]])) stop("Please assign either ME or AL, not both")
	Output <- NULL
	if(exo) {
		ifelse(contain(13, position), LX <- List[position == 13], LX <- list(new("nullSimMatrix")))
		ifelse(contain(14, position), TD <- List[position == 14], TD <- list(new("nullSymMatrix")))
		ifelse(contain(15, position), VTD <- List[position == 15], VTD <- list(new("nullSimVector")))
		ifelse(contain(16, position), VX <- List[position == 16], VX <- list(new("nullSimVector")))
		if(!is.null.object(VTD[[1]]) & !is.null.object(VX[[1]])) stop("Please assign either VTD or VX, not both")
		ifelse(contain(18, position), MX <- List[position == 18], MX <- list(new("nullSimVector")))
		ifelse(contain(17, position), TX <- List[position == 17], TX <- list(new("nullSimVector")))
		if(!is.null.object(MX[[1]]) & !is.null.object(TX[[1]])) stop("Please assign either MX or TX, not both")
		ifelse(contain(19, position), GA <- List[position == 19], GA <- list(new("nullSimMatrix")))
		ifelse(contain(20, position), PH <- List[position == 20], PH <- list(new("nullSymMatrix")))
		ifelse(contain(21, position), VPH <- List[position == 21], VPH <- list(new("nullSimVector")))
		ifelse(contain(22, position), KA <- List[position == 22], KA <- list(new("nullSimVector")))
		ifelse(contain(23, position), TH <- List[position == 23], TH <- list(new("nullSimMatrix")))
		Output <- new("simMisspecifiedSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]],
					LX=LX[[1]], TD=TD[[1]], VTD=VTD[[1]], VX=VX[[1]], MX=MX[[1]], TX=TX[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], TH=TH[[1]], Tag="SEM.exo")	
	} else {
		Output <- new("simMisspecifiedSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="SEM")	
	}
	return(Output)
}
