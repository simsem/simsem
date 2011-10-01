misspecified.Path.object <- function(..., exo = FALSE) {
	W <- get.keywords()
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
	ifelse(contain(1, position), BE <- List[position == 1], BE <- list(new("nullSimMatrix")))
	ifelse(contain(2, position), PS <- List[position == 2], PS <- list(new("nullSymMatrix")))
	ifelse(contain(3, position), VPS <- List[position == 3], VPS <- list(new("nullSimVector")))
	ifelse(contain(4, position), VE <- List[position == 4], VE <- list(new("nullSimVector")))
	if(!is.null.object(VPS[[1]]) & !is.null.object(VE[[1]])) stop("Please assign either VPS or VE, not both")
	ifelse(contain(6, position), ME <- List[position == 6], ME <- list(new("nullSimVector")))
	ifelse(contain(5, position), AL <- List[position == 5], AL <- list(new("nullSimVector")))
	if(!is.null.object(ME[[1]]) & !is.null.object(AL[[1]])) stop("Please assign either ME or AL, not both")
	Output <- NULL
	if(exo) {
		ifelse(contain(7, position), GA <- List[position == 7], GA <- list(new("nullSimMatrix")))
		ifelse(contain(8, position), PH <- List[position == 8], PH <- list(new("nullSymMatrix")))
		ifelse(contain(9, position), VPH <- List[position == 9], VPH <- list(new("nullSimVector")))
		ifelse(contain(10, position), KA <- List[position == 10], KA <- list(new("nullSimVector")))
		Output <- new("simMisspecifiedSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], Tag="Path.exo")	
	} else {
		Output <- new("simMisspecifiedSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="Path")
	}
	return(Output)
}
