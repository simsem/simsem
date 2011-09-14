setClass("blankReducedMatrixSet", 
	representation(
		Tag="character",
		LY="matrix",
		TE="matrix",
		PS="matrix",
		BE="matrix",
		TY="vector",
		AL="vector",
		LX="matrix",
		TD="matrix",
		PH="matrix",
		GA="matrix",
		TX="vector",
		KA="vector",
		TH="matrix"),
	prototype(
		LY=.NULL.matrix,
		TE=.NULL.matrix,
		PS=.NULL.matrix,
		BE=.NULL.matrix,
		TY=.NULL.vector,
		AL=.NULL.vector,
		LX=.NULL.matrix,
		TD=.NULL.matrix,
		PH=.NULL.matrix,
		GA=.NULL.matrix,
		TX=.NULL.vector,
		KA=.NULL.vector,
		TH=.NULL.matrix)
)

collapse.exo <- function(object, value=0, label=FALSE) {
	if(!is.null.object(object@GA)) {
		nk <- ncol(object@GA)
		ne <- nrow(object@GA)
		temp.BE <- combine.path.exo.endo(object@GA, object@BE, value)
		temp.PS <- combine.latent.cor.exo.endo(object@PH, object@PS, value)
		temp.AL <- combine.vector.exo.endo(object@KA, object@AL)
		temp.LY <- .NULL.matrix
		temp.TE <- .NULL.matrix
		temp.TY <- .NULL.vector
		if(object@Tag == "SEM.exo") {
			temp.LY <- combine.loading.exo.endo(object@LX, object@LY, value)
			temp.TE <- combine.measurement.error.exo.endo(object@TD, object@TE, object@TH)
			temp.TY <- combine.vector.exo.endo(object@TX, object@TY)
		}
		if(label) {
			colnames(temp.BE) <- c(colnames(object@GA), colnames(object@BE))
			colnames(temp.PS) <- c(colnames(object@GA), colnames(object@BE))
			rownames(temp.BE) <- c(colnames(object@GA), colnames(object@BE))
			rownames(temp.PS) <- c(colnames(object@GA), colnames(object@BE))
			names(temp.AL) <- c(colnames(object@GA), colnames(object@BE))
			if(object@Tag == "SEM.exo") {
				colnames(temp.LY) <- c(colnames(object@GA), colnames(object@BE))
				colnames(temp.TE) <- c(colnames(object@TD), colnames(object@TE))
				rownames(temp.LY) <- c(colnames(object@TD), colnames(object@TE))
				rownames(temp.TE) <- c(colnames(object@TD), colnames(object@TE))
				names(temp.TY) <- c(colnames(object@TD), colnames(object@TE))
			}			
		} else {
			colnames(temp.BE) <- NULL
			colnames(temp.PS) <- NULL
			rownames(temp.BE) <- NULL
			rownames(temp.PS) <- NULL
			names(temp.AL) <- NULL	
			if(object@Tag == "SEM.exo") {
				colnames(temp.LY) <- NULL
				colnames(temp.TE) <- NULL
				rownames(temp.LY) <- NULL
				rownames(temp.TE) <- NULL
				names(temp.TY) <- NULL
			}			
		}
		return(new(is(object)[1], BE=temp.BE, PS=temp.PS, AL=temp.AL, LY=temp.LY, TE=temp.TE, TY=temp.TY, Tag=object@Tag))
	} else {
		return(object)
	}
}

setClass("freeParamSet", 
	contains="blankReducedMatrixSet"
)

setMethod("summary", signature="freeParamSet", definition=function(object) {
		cat("SET OF ESTIMATED PARAMETERS\n")
		cat("Type\n")
		print(object@Tag)		
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		cat("-- Exogeneous Variable --\n")
		print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
		print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
		print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
		print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
		print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
		print.if.not.null(object@KA, "\nKA: Mean of Factor.KSI")
		print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
		cat("-------------------------------------------------", "\n")
	}
)

setClass("labelsSet", 
	contains="blankReducedMatrixSet"
)

setMethod("summary", signature="labelsSet", definition=function(object) {
		cat("SET OF ESTIMATED PARAMETERS\n")
		cat("Type\n")
		print(object@Tag)		
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		cat("-- Exogeneous Variable --\n")
		print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
		print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
		print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
		print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
		print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
		print.if.not.null(object@KA, "\nKA: Mean of Factor.KSI")
		print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
		cat("-------------------------------------------------", "\n")
	}
)

setMethod("make.labels", signature="freeParamSet", definition=function(object, package) {
	LY <- make.labels(object@LY, "LY", package)
	TE <- make.labels(object@TE, "TE", package, symmetric=TRUE)
	PS <- make.labels(object@PS, "PS", package, symmetric=TRUE)
	BE <- make.labels(object@BE, "BE", package)
	TY <- make.labels(object@TY, "TY", package)
	AL <- make.labels(object@AL, "AL", package)
	LX <- make.labels(object@LX, "LX", package)
	TD <- make.labels(object@TD, "TD", package, symmetric=TRUE)
	PH <- make.labels(object@PH, "PH", package, symmetric=TRUE)
	GA <- make.labels(object@GA, "GA", package)
	TX <- make.labels(object@TX, "TX", package)
	KA <- make.labels(object@KA, "KA", package)
	TH <- make.labels(object@TH, "TH", package)
	return(new("labelsSet", LY=LY, TE=TE, BE=BE, PS=PS, AL=AL, TY=TY,
			LX=LX, TD=TD, TX=TX, GA=GA, PH=PH, KA=KA, TH=TH, Tag=object@Tag))
})

default.starting.values <- function(object) {
	ifelse(is.null.object(object@LY), LY <- .NULL.matrix, {LY <- matrix(NA, nrow(object@LY), ncol(object@LY)); LY[is.na(object@LY)] <- 0.7})
	ifelse(is.null.object(object@TE), TE <- .NULL.matrix, {TE <- matrix(NA, nrow(object@TE), ncol(object@TE)); TE[is.na(object@TE)] <- 0.49; TE[is.na(object@TE) & (upper.tri(TE) | lower.tri(TE))] <- 0.2})
	ifelse(is.null.object(object@PS), PS <- .NULL.matrix, {PS <- matrix(NA, nrow(object@PS), ncol(object@PS)); PS[is.na(object@PS)] <- 1; PS[is.na(object@PS) & (upper.tri(PS) | lower.tri(PS))] <- 0.2})
	ifelse(is.null.object(object@BE), BE <- .NULL.matrix, {BE <- matrix(NA, nrow(object@BE), ncol(object@BE)); BE[is.na(object@BE)] <- 0.3})
	ifelse(is.null.object(object@TY), TY <- .NULL.vector, {TY <- rep(NA, length(object@TY)); TY[is.na(object@TY)] <- 0})
	ifelse(is.null.object(object@AL), AL <- .NULL.vector, {AL <- rep(NA, length(object@AL)); AL[is.na(object@AL)] <- 0})
	ifelse(is.null.object(object@LX), LX <- .NULL.matrix, {LX <- matrix(NA, nrow(object@LX), ncol(object@LX)); LX[is.na(object@LX)] <- 0.7})
	ifelse(is.null.object(object@TD), TD <- .NULL.matrix, {TD <- matrix(NA, nrow(object@TD), ncol(object@TD)); TD[is.na(object@TD)] <- 0.49; TD[is.na(object@TD) & (upper.tri(TD) | lower.tri(TD))] <- 0.2})
	ifelse(is.null.object(object@PH), PH <- .NULL.matrix, {PH <- matrix(NA, nrow(object@PH), ncol(object@PH)); PH[is.na(object@PH)] <- 1; PH[is.na(object@PH) & (upper.tri(PH) | lower.tri(PH))] <- 0.2})
	ifelse(is.null.object(object@GA), GA <- .NULL.matrix, {GA <- matrix(NA, nrow(object@GA), ncol(object@GA)); GA[is.na(object@GA)] <- 0.3})
	ifelse(is.null.object(object@TX), TX <- .NULL.vector, {TX <- rep(NA, length(object@TX)); TX[is.na(object@TX)] <- 0})
	ifelse(is.null.object(object@KA), KA <- .NULL.vector, {KA <- rep(NA, length(object@KA)); KA[is.na(object@KA)] <- 0})
	ifelse(is.null.object(object@TH), TH <- .NULL.matrix, {TH <- matrix(NA, nrow(object@TH), ncol(object@TH)); TH[is.na(object@TH)] <- 0.7})
	return(new("reducedMatrixSet", LY=LY, TE=TE, BE=BE, PS=PS, AL=AL, TY=TY,
			LX=LX, TD=TD, TX=TX, GA=GA, PH=PH, KA=KA, TH=TH, Tag=object@Tag))
}

setMethod("tag.headers", signature="blankReducedMatrixSet", definition=function(object) {
	ny <- NULL
	nx <- NULL
	nk <- NULL
	ne <- NULL
	Tag <- object@Tag
	if(Tag == "CFA") {
		ne <- ncol(object@LY)
		ny <- nrow(object@LY)
	} else if(Tag == "Path") {
		ny <- nrow(object@PS)
	} else if(Tag == "Path.exo") {
		nx <- ncol(object@GA)
		ny <- nrow(object@PS)
	} else if(Tag == "SEM") {
		ne <- ncol(object@LY)
		ny <- nrow(object@LY)
	} else if(Tag == "SEM.exo") {
		ne <- ncol(object@LY)
		ny <- nrow(object@LY)
		nk <- ncol(object@LX)
		nx <- nrow(object@LX)
	} 
	names.y <- NULL
	names.x <- NULL
	names.e <- NULL
	names.k <- NULL
	if(!is.null(ny)) {
		for(i in 1:ny) {
			temp <- paste("y", i, sep="")
			names.y <- c(names.y, temp)
		}
	}
	if(!is.null(nx)) {
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			names.x <- c(names.x, temp)
		}
	}
	if(!is.null(ne)) {
		for(i in 1:ne) {
			temp <- paste("e", i, sep="")
			names.e <- c(names.e, temp)
		}
	}
	if(!is.null(nk)) {
		for(i in 1:nk) {
			temp <- paste("k", i, sep="")
			names.k <- c(names.k, temp)
		}
	}
	if(!is.null.object(object@LY)) {
		colnames(object@LY) <- names.e
		rownames(object@LY) <- names.y
	}
	if(!is.null.object(object@TE)) {
		colnames(object@TE) <- names.y
		rownames(object@TE) <- names.y
	}
	if(!is.null.object(object@PS)) {
		if(Tag == "Path" | Tag == "Path.exo") {
			colnames(object@PS) <- names.y
			rownames(object@PS) <- names.y
		} else {
			colnames(object@PS) <- names.e
			rownames(object@PS) <- names.e
		}
	}
	if(!is.null.object(object@BE)) {
		if(Tag == "Path" | Tag == "Path.exo") {
			colnames(object@BE) <- names.y
			rownames(object@BE) <- names.y
		} else {
			colnames(object@BE) <- names.e
			rownames(object@BE) <- names.e
		}
	}
	if(!is.null.object(object@TY)) {
		names(object@TY) <- names.y
	}
	if(!is.null.object(object@AL)) {
		if(Tag == "Path" | Tag == "Path.exo") {
			names(object@AL) <- names.y
		} else {
			names(object@AL) <- names.e
		}
	}
	if(!is.null.object(object@LX)) {
		colnames(object@LX) <- names.k
		rownames(object@LX) <- names.x
	}
	if(!is.null.object(object@TD)) {
		colnames(object@TD) <- names.x
		rownames(object@TD) <- names.x
	}
	if(!is.null.object(object@PH)) {
		if(Tag == "Path" | Tag == "Path.exo") {
			colnames(object@PH) <- names.x
			rownames(object@PH) <- names.x
		} else {
			colnames(object@PH) <- names.k
			rownames(object@PH) <- names.k
		}
	}
	if(!is.null.object(object@GA)) {
		if(Tag == "Path" | Tag == "Path.exo") {
			colnames(object@GA) <- names.x
			rownames(object@GA) <- names.y
		} else {
			colnames(object@GA) <- names.k
			rownames(object@GA) <- names.e
		}
	}
	if(!is.null.object(object@TX)) {
		names(object@TX) <- names.x
	}
	if(!is.null.object(object@KA)) {
		if(Tag == "Path" | Tag == "Path.exo") {
			names(object@KA) <- names.x
		} else {
			names(object@KA) <- names.k
		}
	}
	if(!is.null.object(object@TH)) {
		colnames(object@TH) <- names.y
		rownames(object@TH) <- names.x
	}
	return(object)
})