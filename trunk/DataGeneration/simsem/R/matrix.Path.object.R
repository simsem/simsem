matrix.Path.object <-
function(..., exo = FALSE) {
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
	ifelse(contain(1, position), BE <- List[position == 1], stop("No path coefficient object between factor.ETA"))
	ne <- ncol(run(BE[[1]]))
	ifelse(contain(2, position), PS <- List[position == 2], stop("No residual correlation object between factor.ETA"))
	ifelse(contain(3, position), VPS <- List[position == 3], VPS <- list(.NULL.simVector))
	ifelse(contain(4, position), VE <- List[position == 4], ifelse(is.null.object(VPS[[1]]), { VE <- list(constant.vector(1, ne)); comment(VE[[1]]) <- "default"}, VE <- list(.NULL.simVector)))
	ifelse(contain(6, position), ME <- List[position == 6], ME <- list(.NULL.simVector))
	ifelse(contain(5, position), AL <- List[position == 5], ifelse(is.null.object(ME[[1]]), { AL <- list(constant.vector(0, ne)); comment(AL[[1]]) <- "default"}, AL <- list(.NULL.simVector)))
	Output <- NULL
	if(exo) {
		ifelse(contain(7, position), GA <- List[position == 7], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
		nk <- ncol(run(GA[[1]]))
		ifelse(contain(8, position), PH <- List[position == 8], stop("No correlation object between factor.KSI"))
		ifelse(contain(9, position), VPH <- List[position == 9], { VPH <- list(constant.vector(1, nk)); comment(VPH[[1]]) <- "default"})
		ifelse(contain(10, position), KA <- List[position == 10], { KA <- list(constant.vector(0, nk)); comment(KA[[1]]) <- "default"})
		Output <- new("simMatrixSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], Tag="Path.exo")	
	} else {
		Output <- new("simMatrixSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="Path")
	}
	return(Output)
}

