simMisspecPath <- function(..., exo = FALSE) {
	W <- get.keywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME)
	} else {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$GA, W$RPH, W$VPH, W$KA)
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), BE <- List[position == 1], BE <- list(new("NullSimMatrix")))
	ifelse(contain(2, position), RPS <- List[position == 2], RPS <- list(new("NullSymMatrix")))
	ifelse(contain(3, position), VPS <- List[position == 3], VPS <- list(new("NullSimVector")))
	ifelse(contain(4, position), VE <- List[position == 4], VE <- list(new("NullSimVector")))
	if(!is.null.object(VPS[[1]]) & !is.null.object(VE[[1]])) stop("Please assign either VPS or VE, not both")
	ifelse(contain(6, position), ME <- List[position == 6], ME <- list(new("NullSimVector")))
	ifelse(contain(5, position), AL <- List[position == 5], AL <- list(new("NullSimVector")))
	if(!is.null.object(ME[[1]]) & !is.null.object(AL[[1]])) stop("Please assign either ME or AL, not both")
	Output <- NULL
	if(exo) {
		ifelse(contain(7, position), GA <- List[position == 7], GA <- list(new("NullSimMatrix")))
		ifelse(contain(8, position), RPH <- List[position == 8], RPH <- list(new("NullSymMatrix")))
		ifelse(contain(9, position), VPH <- List[position == 9], VPH <- list(new("NullSimVector")))
		ifelse(contain(10, position), KA <- List[position == 10], KA <- list(new("NullSimVector")))
		Output <- new("SimMisspec", BE=BE[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], GA=GA[[1]], RPH=RPH[[1]], VPH=VPH[[1]], KA=KA[[1]], modelType="Path.exo")	
	} else {
		Output <- new("SimMisspec", BE=BE[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], modelType="Path")
	}
	return(Output)
}
