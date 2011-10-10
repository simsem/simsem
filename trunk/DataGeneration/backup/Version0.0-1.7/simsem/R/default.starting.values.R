default.starting.values <- function(object) {
	ifelse(is.null.object(object@LY), LY <- new("nullMatrix"), {LY <- matrix(NA, nrow(object@LY), ncol(object@LY)); LY[is.na(object@LY)] <- 0.7})
	ifelse(is.null.object(object@TE), TE <- new("nullMatrix"), {TE <- matrix(NA, nrow(object@TE), ncol(object@TE)); TE[is.na(object@TE)] <- 0.49; TE[is.na(object@TE) & (upper.tri(TE) | lower.tri(TE))] <- 0.2})
	ifelse(is.null.object(object@PS), PS <- new("nullMatrix"), {PS <- matrix(NA, nrow(object@PS), ncol(object@PS)); PS[is.na(object@PS)] <- 1; PS[is.na(object@PS) & (upper.tri(PS) | lower.tri(PS))] <- 0.2})
	ifelse(is.null.object(object@BE), BE <- new("nullMatrix"), {BE <- matrix(NA, nrow(object@BE), ncol(object@BE)); BE[is.na(object@BE)] <- 0.3})
	ifelse(is.null.object(object@TY), TY <- new("nullVector"), {TY <- rep(NA, length(object@TY)); TY[is.na(object@TY)] <- 0})
	ifelse(is.null.object(object@AL), AL <- new("nullVector"), {AL <- rep(NA, length(object@AL)); AL[is.na(object@AL)] <- 0})
	ifelse(is.null.object(object@LX), LX <- new("nullMatrix"), {LX <- matrix(NA, nrow(object@LX), ncol(object@LX)); LX[is.na(object@LX)] <- 0.7})
	ifelse(is.null.object(object@TD), TD <- new("nullMatrix"), {TD <- matrix(NA, nrow(object@TD), ncol(object@TD)); TD[is.na(object@TD)] <- 0.49; TD[is.na(object@TD) & (upper.tri(TD) | lower.tri(TD))] <- 0.2})
	ifelse(is.null.object(object@PH), PH <- new("nullMatrix"), {PH <- matrix(NA, nrow(object@PH), ncol(object@PH)); PH[is.na(object@PH)] <- 1; PH[is.na(object@PH) & (upper.tri(PH) | lower.tri(PH))] <- 0.2})
	ifelse(is.null.object(object@GA), GA <- new("nullMatrix"), {GA <- matrix(NA, nrow(object@GA), ncol(object@GA)); GA[is.na(object@GA)] <- 0.3})
	ifelse(is.null.object(object@TX), TX <- new("nullVector"), {TX <- rep(NA, length(object@TX)); TX[is.na(object@TX)] <- 0})
	ifelse(is.null.object(object@KA), KA <- new("nullVector"), {KA <- rep(NA, length(object@KA)); KA[is.na(object@KA)] <- 0})
	ifelse(is.null.object(object@TH), TH <- new("nullMatrix"), {TH <- matrix(NA, nrow(object@TH), ncol(object@TH)); TH[is.na(object@TH)] <- 0.7})
	return(new("reducedMatrixSet", LY=LY, TE=TE, BE=BE, PS=PS, AL=AL, TY=TY,
			LX=LX, TD=TD, TX=TX, GA=GA, PH=PH, KA=KA, TH=TH, Tag=object@Tag))
}
