setClass("simMatrixSet", 
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

.NULL.simMatrixSet <- new("simMatrixSet", LY=.NULL.simMatrix,
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
		TH=.NULL.simMatrix, 
		Tag="NA")

setMethod("is.null.object", signature="simMatrixSet", definition=function(target) {
		target@Tag == "NA"
	}
)



# List of Keywords

.LY <- c("LY", "ly", "Ly")		#Factor Loading of Y from E
.TE <- c("TE", "te", "Te")		#Measurement Error Correlation
.PS <- c("PS", "ps", "Ps")		#Factor Residual Correlation
.BE <- c("BE", "be", "Be")		#Path within endogeneous factors
.VY <- c("VY", "vy", "Vy")		#Variance of indicators
.VPS <- c("VPS", "Vps", "vps")	#Variance of factor residual
.VE <- c("VE", "Ve", "ve")		#Variance of factors
.TY <- c("TY", "ty", "Ty")		#Measurement Intercept
.ME <- c("ME", "Me", "me")		#Mean of Factor
.VTE <- c("VTE", "vte", "Vte")	#Variance of measurement error
.AL <- c("AL", "Al", "al")		#Intercept of latent residuals
.MY <- c("MY", "My", "my")		#Mean of indicators

.LX <- c("LX", "Lx", "lx")		#Exo Factor Loading
.TD <- c("TD", "td", "Td")		#Factor Measurement Error Correlation
.PH <- c("PH", "ph", "Ph")		#Exo factor correlation
.GA <- c("GA", "ga", "Ga")		#Path from Exo to Endo
.VX <- c("VX", "Vx", "vx")		#Variance of exo indicators
.VPH <- c("VK", "vk", "Vk", "Vph", "VPH", "vph")		#Variance of exo factors
.TX <- c("TX", "Tx", "tx")		#Exo measurement intercept
.KA <- c("KA", "Ka", "ka", "MK", "mk", "Mk")		#Exo factor mean
.VTD <- c("VTD", "Vtd", "vtd")	#Variance of exo measurement error
.MX <- c("MX", "mx", "Mx")		#Exo indicator mean
.TH <- c("TH", "th", "Th")		#Correlated error of exo (row) and endo (column)

.loading <- c(.LY, .LX, "loading", "Loading", "Factor Loading")
.error <- c(.TE, .TD, "error", "Error", "Error covariance")
.latent.cor <- c(.PS, .PH, "latent.cor", "Latent.cor", "Latent.cov", "latent.cov", "Factor Covariance")
.intercept <- c(.TY, .TX, "intercept", "Intercept", "Measurement Intercept")
.factor.mean <- c(.ME, .KA, .AL, "Factor mean", "factor mean", "Factor Mean")

match.keyword <- function(Names, keywords) {
	Length <- length(Names)
	Result <- rep(NA, Length)
	for(i in 1:Length) {
		temp <- 0
		for(j in 1:length(keywords)) {
			temp.compare <- keywords[[j]]
			if(sum(temp.compare == Names[i]) != 0) temp <- j
		}
		Result[i] <- temp
	}
	return(Result)
}

contain <- function(element, Vector) {
	ifelse(sum(Vector == element) > 0, return(TRUE), return(FALSE))
}
		
matrix.CFA.object <- function(...) { #loading, latent.cor, error.cor, latent.var = NULL, error.var = NULL, indicator.var = NULL, intercept = NULL, indicator.mean = NULL, factor.mean = NULL) {
	List <- list(...)
	Names <- names(List)
	keywords <- list(.loading, .error, .latent.cor, c("Variance of Measurement Error", .VTD, .VTE), c("Variance of Indicators", .VX, .VY), .intercept, .factor.mean, c("Means of Indicators", .MX, .MY), c("Variance of Factors", .VE, .VPS, .VPH))
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1], stop("No loading object in CFA"))
	ni <- nrow(run(LY[[1]]))
	nk <- ncol(run(LY[[1]]))
	ifelse(contain(2, position), TE <- List[position == 2], stop("No error correlation object in CFA"))
	ifelse(contain(3, position), PS <- List[position == 3], stop("No latent variables correlation object in CFA"))
	ifelse(contain(4, position), VTE <- List[position == 4], VTE <- list(.NULL.simVector))
	ifelse(contain(5, position), VY <- List[position == 5], ifelse(is.null.object(VTE[[1]]), { VY <- list(constant.vector(1, ni)); comment(VY[[1]]) <- "default"}, VY <- list(.NULL.simVector)))
	ifelse(contain(8, position), MY <- List[position == 8], MY <- list(.NULL.simVector))
	ifelse(contain(6, position), TY <- List[position == 6], ifelse(is.null.object(MY[[1]]), { TY <- list(constant.vector(0, ni)); comment(TY[[1]]) <- "default"}, TY <- list(.NULL.simVector)))
	ifelse(contain(7, position), ME <- List[position == 7], { ME <- list(constant.vector(0, nk)); comment(ME[[1]]) <- "default"})
	ifelse(contain(9, position), VE <- List[position == 9], { VE <- list(constant.vector(1, nk)); comment(VE[[1]]) <- "default"})
	Output <- new("simMatrixSet", LY=LY[[1]], PS=PS[[1]], TE=TE[[1]], VE=VE[[1]], VPS=VE[[1]], VTE=VTE[[1]], VY=VY[[1]], TY=TY[[1]], MY=MY[[1]], ME=ME[[1]], AL=ME[[1]], Tag="CFA")
	return(Output)
}

matrix.Path.object <- function(..., exo = FALSE) {
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

matrix.SEM.object <- function(..., exo = FALSE) {
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

print.if.not.null <- function(object, name) {
	if(!is.null.object(object)) {
		cat(name, "\n")
		#print(name, quote=FALSE)
		summary.short(object)	
	}
}

setMethod("summary", signature="simMatrixSet", definition= function(object) {
		cat("SET OF MODEL MATRICES\n")
		cat("Type\n")
		print(object@Tag)
		cat("-- Endogeneous Variable --\n")
		print.if.not.null(object@LY, "\nLY: Loading of Indicator.Y on Factor.ETA")
		print.if.not.null(object@VTE, "\nVTE: Variance of Measurement.Error.EPSILON")
		print.if.not.null(object@TE, "\nTE: Correlation of Measurement.Error.EPSILON")
		print.if.not.null(object@VY, "\nVY: Variance of Indicator.Y")
		print.if.not.null(object@TY, "\nTY: Measurement Intercept of Indicator.Y")
		print.if.not.null(object@MY, "\nMY: Mean of Indicator.Y")
		print.if.not.null(object@BE, "\nBE: Regression Coefficient among Factor.ETA")
		print.if.not.null(object@VPS, "\nVPS: Variance of Regression.Residual.PSI")
		print.if.not.null(object@PS, "\nPS: Correlation of Regression.Residual.PSI")
		print.if.not.null(object@VE, "\nVE: Variance of Factor.ETA")
		print.if.not.null(object@AL, "\nAL: Regression Intercept of Factor.ETA")
		print.if.not.null(object@ME, "\nME: Mean of Factor.ETA")
		cat("-------------------------------------------------", "\n")
		cat("-- Exogeneous Variable --\n")
		print.if.not.null(object@LX, "\nLX: Loading of Indicator.X on Factor.KSI")
		print.if.not.null(object@VTD, "\nVTD: Variance of Measurement.Error.DELTA")
		print.if.not.null(object@TD, "\nTD: Correlation of Measurement.Error.DELTA")
		print.if.not.null(object@VX, "\nVX: Variance of Indicator.X")
		print.if.not.null(object@TX, "\nTX: Measurement Intercept of Indicator.X")
		print.if.not.null(object@MX, "\nMX: Mean of Indicator.X")
		print.if.not.null(object@GA, "\nGA: Regression Coefficient of Factor.ETA on Factor.KSI")
		print.if.not.null(object@VPH, "\nVPH: Variance of Factor.KSI")
		print.if.not.null(object@PH, "\nPH: Correlation of Factor.KSI")
		print.if.not.null(object@KA, "\nKA: Mean of Factor.KSI")
		print.if.not.null(object@TH, "\nTH: Correlation of Measurement.Error.DELTA and Measurement.Error.EPSILON")
		cat("-------------------------------------------------", "\n")
	}
)

is.default <- function(object) {
	#browser()
	if(is.null.object(object)) return(FALSE)
	if(is.null(comment(object))) return(FALSE)
	ifelse(comment(object) == "default", return(TRUE), return(FALSE))
}

create.free.parameters <- function(object) {
	if(!is(object, "simMatrixSet")) stop("The attribute is not a simMatrixSet object.")
	LY <- object@LY@Data
	is.measurement.Y <- !(is.null.object(LY))
	TE <- object@TE@Data
	if(is.measurement.Y) {
		VTE <- object@VTE@Data
		ifelse(is.null.object(VTE), diag(TE) <- NA, diag(TE) <- VTE)
	}
	TY <- object@TY@Data
	if(is.measurement.Y & is.default(object@TY)) TY <- rep(NA, nrow(LY))
	BE <- object@BE@Data
	PS <- object@PS@Data 
	VPS <- object@VPS@Data
	ifelse(is.null.object(VPS), ifelse(is.measurement.Y, diag(PS) <- 1, diag(PS) <- NA), diag(PS) <- VPS)
	AL <- object@AL@Data
	if(is.default(object@AL)) {
		ifelse(is.measurement.Y, AL <- rep(0, ncol(PS)), AL <- rep(NA, ncol(PS)))
	}
	#-- Exogeneous Variable --
	LX <- object@LX@Data
	TD <- object@TD@Data 
	GA <- object@GA@Data
	PH <- object@PH@Data	
	KA <- object@KA@Data
	TX <- object@TX@Data
	TH <- object@TH@Data
	if(!is.null.object(PH)) {
		VPH <- object@VPH@Data
		is.measurement.X <- !is.null.object(LX)
		ifelse(is.null.object(VPH) | (sum(VPH != 1) == 0), ifelse(is.measurement.X, diag(PH) <- 1, diag(PH) <- NA), diag(PH) <- VPH)
		if(is.default(object@KA)) ifelse(is.measurement.X, KA <- rep(0, ncol(PH)), KA <- rep(NA, ncol(PH)))
		if(is.measurement.X) {
			VTD <- object@VTD@Data
			ifelse(is.null.object(VTD), diag(TD) <- NA, diag(TD) <- VTD)
			if(is.default(object@TX)) TX <- rep(NA, nrow(LX))
		}
	}
	Output <- new("freeParamSet", LY=LY, TE=TE, BE=BE, PS=PS, AL=AL, TY=TY,
			LX=LX, TD=TD, TX=TX, GA=GA, PH=PH, KA=KA, TH=TH, Tag=object@Tag)	
}

setMethod("count.random.object", signature="simMatrixSet", definition=function(object) {
	return(sum(c(count.random.object(object@LY),
		count.random.object(object@TE),
		count.random.object(object@VTE),
		count.random.object(object@PS),
		count.random.object(object@VPS),
		count.random.object(object@BE),
		count.random.object(object@TY),
		count.random.object(object@AL),
		count.random.object(object@ME),
		count.random.object(object@MY),
		count.random.object(object@VE),
		count.random.object(object@VY),
		count.random.object(object@LX),
		count.random.object(object@TD),
		count.random.object(object@VTD),
		count.random.object(object@PH),
		count.random.object(object@GA),
		count.random.object(object@TX),
		count.random.object(object@KA),
		count.random.object(object@MX),
		count.random.object(object@VPH),
		count.random.object(object@VX),
		count.random.object(object@TH))))
	}
)