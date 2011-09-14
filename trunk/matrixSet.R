setClass("matrixSet", 
	representation(
		Tag="character",
		LY="matrix",
		TE="matrix",
		VTE="vector",
		PS="matrix",
		VPS="vector",
		BE="matrix",
		TY="vector",
		AL="vector",
		ME="vector",
		MY="vector",
		VE="vector",
		VY="vector",
		LX="matrix",
		TD="matrix",
		VTD="vector",
		PH="matrix",
		GA="matrix",
		TX="vector",
		KA="vector",
		MX="vector",
		VPH="vector",
		VX="vector",
		TH="matrix"),
	prototype(
		LY=.NULL.matrix,
		TE=.NULL.matrix,
		VTE=.NULL.vector,
		PS=.NULL.matrix,
		VPS=.NULL.vector,
		BE=.NULL.matrix,
		TY=.NULL.vector,
		AL=.NULL.vector,
		ME=.NULL.vector,
		MY=.NULL.vector,
		VE=.NULL.vector,
		VY=.NULL.vector,
		LX=.NULL.matrix,
		TD=.NULL.matrix,
		VTD=.NULL.vector,
		PH=.NULL.matrix,
		GA=.NULL.matrix,
		TX=.NULL.vector,
		KA=.NULL.vector,
		MX=.NULL.vector,
		VPH=.NULL.vector,
		VX=.NULL.vector,
		TH=.NULL.matrix)
)

setMethod("run", signature(object="simMatrixSet"), definition=function(object, simConstraint=.NULL.simConstraint) {
		Parameters <- list(LY = run(object@LY),
			VTE = run(object@VTE),
			TE = run(object@TE),
			VY = run(object@VY),
			TY = run(object@TY),
			MY = run(object@MY),
			BE = run(object@BE),
			VPS = run(object@VPS),
			PS = run(object@PS),
			VE = run(object@VE),
			AL = run(object@AL),
			ME = run(object@ME),
			LX = run(object@LX),
			VTD = run(object@VTD),
			TD = run(object@TD),
			VX = run(object@VX),
			TX = run(object@TX),
			MX = run(object@MX),
			GA = run(object@GA),
			VPH = run(object@VPH),
			PH = run(object@PH),
			KA = run(object@KA),
			TH = run(object@TH))
		if(!is.null.object(simConstraint)) {
			if(object@Tag != simConstraint@Tag) stop("Please provide same tags of simMatrixSet and constraint")
			Parameters <- constrain.matrices(Parameters, simConstraint, object@Tag)
		}
		LY <- Parameters$LY
		VTE <- Parameters$VTE
		TE <- Parameters$TE
		VY <- Parameters$VY
		TY <- Parameters$TY
		MY <- Parameters$MY
		BE <- Parameters$BE
		VPS <- Parameters$VPS
		PS <- Parameters$PS
		VE <- Parameters$VE
		AL <- Parameters$AL
		ME <- Parameters$ME
		LX <- Parameters$LX
		VTD <- Parameters$VTD
		TD <- Parameters$TD
		VX <- Parameters$VX
		TX <- Parameters$TX
		MX <- Parameters$MX
		GA <- Parameters$GA
		VPH <- Parameters$VPH
		PH <- Parameters$PH
		KA <- Parameters$KA
		TH <- Parameters$TH
		if(object@Tag == "CFA") {
			if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
			if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
			if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
			if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		} else if(object@Tag == "Path") {
			if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
			if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
			if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
			if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		} else if(object@Tag =="Path.exo") {
			nx <- ncol(GA)
			ny <- nrow(GA)
			temp.BE <- combine.path.exo.endo(GA, BE)
			temp.PS <- combine.latent.cor.exo.endo(PH, PS)
			if(is.null.object(VPS)) {
				temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
				VPS <- temp.VPS[(nx + 1):(nx + ny)]
			}
			if(is.null.object(VE)) {
				temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
				VE <- temp.VE[(nx + 1):(nx + ny)]
			}
			if(is.null.object(ME)) {
				temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
				ME <- temp.ME[(nx + 1):(nx + ny)]
			}
			if(is.null.object(AL)) {
				temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
				AL <- temp.AL[(nx + 1):(nx + ny)]
			}
		} else if(object@Tag == "SEM") {
			#browser()
			if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
			if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
			if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
			if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
			#browser()
			if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
			if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
			if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
			if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		} else if(object@Tag == "SEM.exo") {
			nk <- ncol(GA)
			ne <- nrow(GA)
			temp.BE <- combine.path.exo.endo(GA, BE)
			temp.PS <- combine.latent.cor.exo.endo(PH, PS)
			if(is.null.object(VPS)) {
				temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
				VPS <- temp.VPS[(nk + 1):(nk + ne)]
			}
			if(is.null.object(VE)) {
				temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
				VE <- temp.VE[(nk + 1):(nk + ne)]
			}
			if(is.null.object(ME)) {
				temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
				ME <- temp.ME[(nk + 1):(nk + ne)]
			}
			if(is.null.object(AL)) {
				temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
				AL <- temp.AL[(nk + 1):(nk + ne)]
			}
			if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
			if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
			if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
			if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
			if(is.null.object(VTD)) VTD <- find.measurement.error.var(LX, PH, VX, VPH)
			if(is.null.object(VX)) VX <- find.indicator.var(LX, PH, VTD, VPH)
			if(is.null.object(MX)) MX <- find.indicator.mean(LX, KA, TX)
			if(is.null.object(TX)) TX <- find.measurement.intercept(LX, KA, MX)
		}
		return(new("matrixSet", Tag=object@Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH))
	}
)

run.misspecified <- function(object, Misspecified, simConstraint=.NULL.simConstraint, Constrain.Parameters.Only=FALSE, seed=NULL) {
	if(!is.null(seed)) set.seed(seed)
	Parameters <- list(LY = run(object@LY),
		VTE = run(object@VTE),
		TE = run(object@TE),
		VY = run(object@VY),
		TY = run(object@TY),
		MY = run(object@MY),
		BE = run(object@BE),
		VPS = run(object@VPS),
		PS = run(object@PS),
		VE = run(object@VE),
		AL = run(object@AL),
		ME = run(object@ME),
		LX = run(object@LX),
		VTD = run(object@VTD),
		TD = run(object@TD),
		VX = run(object@VX),
		TX = run(object@TX),
		MX = run(object@MX),
		GA = run(object@GA),
		VPH = run(object@VPH),
		PH = run(object@PH),
		KA = run(object@KA),
		TH = run(object@TH))
	if(!is.null.object(simConstraint) & (Constrain.Parameters.Only=TRUE)) {
		if(object@Tag != simConstraint@Tag) stop("Please provide same tags of simMatrixSet and constraint")
		Parameters <- constrain.matrices(Parameters, simConstraint, object@Tag)
	}
	LY <- Parameters$LY
	VTE <- Parameters$VTE
	TE <- Parameters$TE
	VY <- Parameters$VY
	TY <- Parameters$TY
	MY <- Parameters$MY
	BE <- Parameters$BE
	VPS <- Parameters$VPS
	PS <- Parameters$PS
	VE <- Parameters$VE
	AL <- Parameters$AL
	ME <- Parameters$ME
	LX <- Parameters$LX
	VTD <- Parameters$VTD
	TD <- Parameters$TD
	VX <- Parameters$VX
	TX <- Parameters$TX
	MX <- Parameters$MX
	GA <- Parameters$GA
	VPH <- Parameters$VPH
	PH <- Parameters$PH
	KA <- Parameters$KA
	TH <- Parameters$TH
	if(object@Tag == "CFA") {
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@Tag == "Path") {
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
	} else if(object@Tag =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
			VPS <- temp.VPS[(nx + 1):(nx + ny)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
			VE <- temp.VE[(nx + 1):(nx + ny)]
		}
		if(is.null.object(ME)) {
			temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nx + 1):(nx + ny)]
		}
		if(is.null.object(AL)) {
			temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nx + 1):(nx + ny)]
		}
	} else if(object@Tag == "SEM") { 
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@Tag == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
			VPS <- temp.VPS[(nk + 1):(nk + ne)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
			VE <- temp.VE[(nk + 1):(nk + ne)]
		}
		if(is.null.object(ME)) {
			temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nk + 1):(nk + ne)]
		}
		if(is.null.object(AL)) {
			temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nk + 1):(nk + ne)]
		}
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		if(is.null.object(VTD)) VTD <- find.measurement.error.var(LX, PH, VX, VPH)
		if(is.null.object(VX)) VX <- find.indicator.var(LX, PH, VTD, VPH)
		if(is.null.object(MX)) MX <- find.indicator.mean(LX, KA, TX)
		if(is.null.object(TX)) TX <- find.measurement.intercept(LX, KA, MX)
	}
	Output1 <- new("matrixSet", Tag=object@Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH)
	Mis <- run(Misspecified)
	Parameters <- combine.object(Parameters, Mis)
	if(!is.null.object(simConstraint) & (Constrain.Parameters.Only=FALSE)) {
		if(object@Tag != simConstraint@Tag) stop("Please provide same tags of simMatrixSet and constraint")
		Parameters <- constrain.matrices(Parameters, simConstraint, object@Tag)
	}
	LY <- Parameters$LY
	VTE <- Parameters$VTE
	TE <- Parameters$TE
	VY <- Parameters$VY
	TY <- Parameters$TY
	MY <- Parameters$MY
	BE <- Parameters$BE
	VPS <- Parameters$VPS
	PS <- Parameters$PS
	VE <- Parameters$VE
	AL <- Parameters$AL
	ME <- Parameters$ME
	LX <- Parameters$LX
	VTD <- Parameters$VTD
	TD <- Parameters$TD
	VX <- Parameters$VX
	TX <- Parameters$TX
	MX <- Parameters$MX
	GA <- Parameters$GA
	VPH <- Parameters$VPH
	PH <- Parameters$PH
	KA <- Parameters$KA
	TH <- Parameters$TH
	if(object@Tag == "CFA") {
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@Tag == "Path") {
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
	} else if(object@Tag =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
			VPS <- temp.VPS[(nx + 1):(nx + ny)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
			VE <- temp.VE[(nx + 1):(nx + ny)]
		}
		if(is.null.object(ME)) {
			temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nx + 1):(nx + ny)]
		}
		if(is.null.object(AL)) {
			temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nx + 1):(nx + ny)]
		}
	} else if(object@Tag == "SEM") { 
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@Tag == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
			VPS <- temp.VPS[(nk + 1):(nk + ne)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
			VE <- temp.VE[(nk + 1):(nk + ne)]
		}
		if(is.null.object(ME)) {
			temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nk + 1):(nk + ne)]
		}
		if(is.null.object(AL)) {
			temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nk + 1):(nk + ne)]
		}
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		if(is.null.object(VTD)) VTD <- find.measurement.error.var(LX, PH, VX, VPH)
		if(is.null.object(VX)) VX <- find.indicator.var(LX, PH, VTD, VPH)
		if(is.null.object(MX)) MX <- find.indicator.mean(LX, KA, TX)
		if(is.null.object(TX)) TX <- find.measurement.intercept(LX, KA, MX)
	}
	Output2 <- new("matrixSet", Tag=object@Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH)
	return(list(Parameters=Output1, Misspecified=Output2))
}


setMethod("constrain.matrices", signature(object="list", simConstraint="simConstraint"), definition=function(object, simConstraint, Tag) {
	label.selection <- NULL
	if(Tag == "CFA") {
		label.selection <- c("LY", "TE", "TY", "PS", "AL", "VTE", "VY", "VPS", "MY")
	} else if(Tag == "Path") {
		label.selection <- c("BE", "PS", "AL", "VPS", "VE", "ME")	
	} else if(Tag == "Path.exo") {
		label.selection <- c("BE", "PS", "AL", "GA", "PH", "KA", "VPS", "VE", "ME", "VPH")	
	} else if(Tag == "SEM") {
		label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL", "VTE", "VY", "MY", "VPS", "VE", "ME")
	} else if(Tag == "SEM.exo") {
		label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL", "LX", "TD", "TX", "GA", "PH", "KA", "TH", "VTE", "VY", "MY", "VPS", "VE", "ME", "VTD", "VX", "MX", "VPH")
	} else {
		stop("Do not have an appropriate tag")
	}
	matrices <- object
	constraint <- simConstraint@Equality
	n.constraint <- length(constraint)
	for(j in 1:n.constraint) {
		temp.constraint <- constraint[[j]]
		temp.matrix <- rownames(temp.constraint)[1]
		num <- equal.which(temp.matrix, label.selection)
		fixedvalue <- NA
		if(is.mean.constraint(rownames(temp.constraint))) {
			fixedvalue <- matrices[[num]][temp.constraint[1, 2]]		
		} else {
			fixedvalue <- matrices[[num]][temp.constraint[1, 2], temp.constraint[1, 3]]
		}		
		for(i in 2:nrow(temp.constraint)) {
			temp.matrix2 <- rownames(temp.constraint)[i]
			num <- equal.which(temp.matrix2, label.selection)
			if(is.mean.constraint(rownames(temp.constraint))) {
				matrices[[num]][temp.constraint[i, 2]] <- fixedvalue
			} else {
				matrices[[num]][temp.constraint[i, 2], temp.constraint[i, 3]] <- fixedvalue
				if(is.cor.matrix(matrices[[num]])) matrices[[num]][temp.constraint[i,2], temp.constraint[i,1]] <- fixedvalue
			}
		}
	}
	#Output <- new("reducedMatrixSet", Tag=object@Tag, PS=matrices$PS, BE=matrices$BE, AL=matrices$AL, TE=matrices$TE, LY=matrices$LY, TY=matrices$TY,
	#	PH=matrices$PH, GA=matrices$GA, KA=matrices$KA, TD=matrices$TD, LX=matrices$LX, TX=matrices$TX, TH=matrices$TH)
	return(matrices)
})

setMethod("combine.object", signature(object1="vector", object2="vector"), definition=function(object1, object2) {
		if(is.null.object(object1)) {
			if(is.null.object(object2)) {
				return(.NULL.vector)
			} else {
				stop("Please make sure that \n
					1) The trivially misspecified matrix set is put as a second argument. \n
					2) Any of trivially misspecified matrices are not null in the main set.")
			}
		} else {
			if(is.null.object(object2)) {
				return(object1)
			} else {
				ifelse(length(object1) == length(object2), return(object1 + object2), stop("Length of vectors are not equal."))
			}
		}
	}
)

setMethod("combine.object", signature(object1="matrix", object2="matrix"), definition=function(object1, object2, correlation = FALSE) {
		if(is.null.object(object1)) {
			if(is.null.object(object2)) {
				return(.NULL.matrix)
			} else {
				stop("Please make sure that \n
					1) The trivially misspecified matrix set is put as a second argument. \n
					2) Any of trivially misspecified matrices are not null in the main set.")
			}
		} else {
			if(is.null.object(object2)) {
				return(object1)
			} else {
				if(sum(dim(object1) != dim(object2)) == 0) {
					if(correlation == TRUE) {
						temp <- object1 + object2
						diag(temp) <- 1
						return(temp)
					} else {
						return(object1 + object2)
					}
				} else {
					stop("Dimension of matrices are not equal.")
				}
			}
		}
	}
)

setMethod("combine.object", signature(object1="matrixSet", object2="matrixSet"), definition=function(object1, object2) {
		LY <- combine.object(object1@LY, object2@LY)
		VTE <- combine.object(object1@VTE, object2@VTE)
		TE <- combine.object(object1@TE, object2@TE, correlation = TRUE)
		VY <- combine.object(object1@VY, object2@VY)
		TY <- combine.object(object1@TY, object2@TY) 
		MY <- combine.object(object1@MY, object2@MY)
		BE <- combine.object(object1@BE, object2@BE)
		VPS <- combine.object(object1@VPS, object2@VPS)
		PS <- combine.object(object1@PS, object2@PS, correlation = TRUE)
		VE <- combine.object(object1@VE, object2@VE) 
		AL <- combine.object(object1@AL, object2@AL) 
		ME <- combine.object(object1@ME, object2@ME) 
		LX <- combine.object(object1@LX, object2@LX) 
		VTD <- combine.object(object1@VTD, object2@VTD) 
		TD <- combine.object(object1@TD, object2@TD, correlation = TRUE)
		VX <- combine.object(object1@VX, object2@VX)
		TX <- combine.object(object1@TX, object2@TX)
		MX <- combine.object(object1@MX, object2@MX)
		GA <- combine.object(object1@GA, object2@GA)
		VPH <- combine.object(object1@VPH, object2@VPH)
		PH <- combine.object(object1@PH, object2@PH, correlation = TRUE)
		KA <- combine.object(object1@KA, object2@KA)
		TH <- combine.object(object1@TH, object2@TH)
		Output <- new("matrixSet", Tag=object1@Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
			BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
			LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
			GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH)
		return(Output)
	}
)

setMethod("divide.object", signature(object="vector", constant="numeric"), definition=function(object, constant) {
		if(is.null.object(object)) {
			return(.NULL.vector)
		} else {
			return(object / constant)
		}
	}
)

setMethod("divide.object", signature(object="matrix", constant="numeric"), definition=function(object, constant, correlation = FALSE) {
		if(is.null.object(object)) {
			return(.NULL.matrix)
		} else {
			if(correlation) {
				temp <- diag(object)
				object <- object / constant
				diag(object) <- temp
				return(object)
			} else {
				return(object / constant)
			}
		}
	}
)

setMethod("divide.object", signature(object="matrixSet", constant="numeric"), definition=function(object, constant) {
		#browser()
		LY <- divide.object(object@LY, constant)
		VTE <- divide.object(object@VTE, constant)
		TE <- divide.object(object@TE, constant, correlation = TRUE)
		VY <- divide.object(object@VY, constant)
		TY <- divide.object(object@TY, constant)
		MY <- divide.object(object@MY, constant)
		BE <- divide.object(object@BE, constant)
		VPS <- divide.object(object@VPS, constant)
		PS <- divide.object(object@PS, constant, correlation = TRUE)
		VE <- divide.object(object@VE, constant) 
		AL <- divide.object(object@AL, constant)
		ME <- divide.object(object@ME, constant) 
		LX <- divide.object(object@LX, constant) 
		VTD <- divide.object(object@VTD, constant)
		TD <- divide.object(object@TD, constant, correlation = TRUE)
		VX <- divide.object(object@VX, constant)
		TX <- divide.object(object@TX, constant)
		MX <- divide.object(object@MX, constant)
		GA <- divide.object(object@GA, constant)
		VPH <- divide.object(object@VPH, constant)
		PH <- divide.object(object@PH, constant, correlation = TRUE)
		KA <- divide.object(object@KA, constant)
		TH <- divide.object(object@TH, constant)
		Output <- new("matrixSet", Tag=object@Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
			BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
			LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
			GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH)
		return(Output)
	}
)

setMethod("starting.values", signature(object="simMatrixSet"), definition=function(object, trial, reduced=FALSE) {
		result <- run(object)
		#browser()
		if(trial > 1) {
			for(i in 2:trial) {
				temp <- run(object)
				result <- combine.object(result, temp)
			}
			#browser()
			result <- divide.object(result, trial)
		}
		#browser()
		result@Tag <- object@Tag
		if(reduced == TRUE) result <- reduce.matrices(result)
		return(result)
	}
)

shuffle.position <- function(square.matrix, var1, var2) {
	name <- colnames(square.matrix)
	name.var1 <- name[var1]
	name.var2 <- name[var2]
	name[var2] <- name.var1
	name[var1] <- name.var2
	temp.row1 <- square.matrix[var1,]
	temp.row2 <- square.matrix[var2,]
	square.matrix[var2,] <- temp.row1
	square.matrix[var1,] <- temp.row2
	temp.col1 <- square.matrix[,var1]
	temp.col2 <- square.matrix[,var2]
	square.matrix[,var2] <- temp.col1
	square.matrix[,var1] <- temp.col2
	colnames(square.matrix) <- name
	rownames(square.matrix) <- name
	return(square.matrix)
}

find.row.zero <- function(square.matrix, is.row.fixed = FALSE) {
	#browser()
	ni <- nrow(square.matrix)
	if(length(is.row.fixed) == 1) {
		if(is.row.fixed == FALSE) is.row.fixed <- rep(FALSE, ni)
	}
	result <- NULL
	desired.zero <- sum(!is.row.fixed)
	for(i in 1:ni) {
		if(is.row.fixed[i] == FALSE) {	
			temp <- sum(square.matrix[i,!is.row.fixed] == 0, na.rm = TRUE)
			if(temp == desired.zero) result <- c(result, i)
		}
	}
	return(result)
}

find.recursive.set <- function(square.matrix) {
	result <- list()
	ni <- nrow(square.matrix)
	fix.variable <- rep(FALSE, ni)
	ni.sofar <- 0
	i <- 1
	#browser()
	while(ni.sofar < ni) {
		temp <- find.row.zero(square.matrix, fix.variable)
		if(is.null(temp)) stop("The matrix is not recursive.")
		fix.variable[temp] <- TRUE
		result[[i]] <- temp
		i <- i + 1
		ni.sofar <- ni.sofar + length(temp)
	}
	return(result)
}

find.possible.latent.cor <- function(path.matrix) {
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	psi <- matrix(0, ni, ni)
	diag(psi) <- 1
	for(i in 1:length(set)) {
		temp.set <- set[[i]]
		if(length(temp.set) > 1) {
			for(j in 2:length(temp.set)) {
				for(k in 1:(j - 1)) {
					psi[temp.set[j], temp.set[k]] <- NA
					psi[temp.set[k], temp.set[j]] <- NA
				}
			}
		}
	}
	return(psi)
}

cor2cov <- function(correlation, stdev) {
	ni <- nrow(correlation)
	if(!is.matrix(stdev)) {
		temp <- matrix(0, ni, ni)
		diag(temp) <- stdev
		stdev <- temp
	}
	covariance <- stdev %*% correlation %*% stdev
	return(covariance)
}


find.latent.error.var <- function(path.matrix, latent.cor.matrix, factor.var = NULL) {
	#browser()
	if(sum(diag(latent.cor.matrix)) == 0) diag(latent.cor.matrix) <- 1
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	error.var <- rep(1, ni)
	if(is.null(factor.var)) factor.var <- rep(1, ni)
	error.var[set[[1]]] <- factor.var[set[[1]]]
	iv <- NULL
	iv.cor <- latent.cor.matrix[set[[1]], set[[1]]]
	start.var <- factor.var[set[[1]]]
	iv.cov <- cor2cov(as.matrix(iv.cor), sqrt(start.var))
	for(i in 1:(length(set) - 1)) {
		iv <- c(iv, set[[i]])
		dv <- set[[i + 1]]
		temp.path <- matrix(path.matrix[dv, iv], nrow = length(dv), ncol = length(iv))
		var.reg <- (temp.path %*% iv.cov %*% t(temp.path))
		psi <- latent.cor.matrix[dv, dv]
		psi.sd <- matrix(0, length(dv), length(dv))
		for(j in 1:length(dv)) {
			error.var[dv[j]]  <- factor.var[dv[j]] - var.reg[j, j]
			psi.sd[j, j] <- sqrt(error.var[dv[j]])
		}
		if(i < (length(set) - 1)) {
			psi <- cor2cov(psi, psi.sd)
			real.psi <- matrix(0, length(iv) + length(dv), length(iv) + length(dv))
			real.psi[1:length(iv), 1:length(iv)] <- iv.cov
			real.psi[(length(iv) + 1):(length(iv) + length(dv)), (length(iv) + 1):(length(iv) + length(dv))] <- psi
			agg <- c(iv, dv)
			blank.path <- matrix(0, nrow = length(iv), ncol = length(agg))
			temp.path2 <- path.matrix[dv, agg]
			temp.path2 <- rbind(blank.path, temp.path2)
			ID <- matrix(0, length(agg), length(agg))
			diag(ID) <- 1
			iv.cov <- solve(ID - temp.path2) %*% real.psi %*% t(solve(ID - temp.path2))
		}		
	}
	return(as.vector(error.var))
}

find.factor.var <- function(path.matrix, latent.cor.matrix, error.var) {
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	real.psi <- cor2cov(latent.cor.matrix, sqrt(error.var))
	ID <- matrix(0, ni, ni)
	diag(ID) <- 1
	iv.cov <- solve(ID - path.matrix) %*% real.psi %*% t(solve(ID - path.matrix))
	factor.var <- diag(iv.cov)
	return(as.vector(factor.var))
}

find.latent.intercept <- function(path.matrix, factor.mean = NULL) {
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	intercept <- rep(0, ni)
	if(is.null(factor.mean)) factor.mean <- rep(0, ni)
	intercept[set[[1]]] <- factor.mean[set[[1]]]
	iv <- NULL
	iv.mean <- factor.mean[set[[1]]]
	for(i in 1:(length(set) - 1)) {
		iv <- c(iv, set[[i]])
		dv <- set[[i + 1]]
		temp.path <- matrix(path.matrix[dv, iv], nrow = length(dv), ncol = length(iv))
		mean.reg <- (temp.path %*% iv.mean)
		dv.mean <- factor.mean[dv]
		intercept[dv] <- dv.mean - mean.reg
		if(i < (length(set) - 1)) {
			agg <- c(iv, dv)
			iv.mean <- factor.mean[agg]
		}
	}
	return(as.vector(intercept))
}

find.factor.mean <- function(path.matrix, intercept = NULL) {
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	factor.mean <- rep(0, ni)
	if(is.null(intercept)) intercept <- rep(0, ni)
	factor.mean[set[[1]]] <- intercept[set[[1]]]
	iv <- NULL
	iv.mean <- factor.mean[set[[1]]]
	for(i in 1:(length(set) - 1)) {
		iv <- c(iv, set[[i]])
		dv <- set[[i + 1]]
		temp.path <- matrix(path.matrix[dv, iv], nrow = length(dv), ncol = length(iv))
		mean.reg <- (temp.path %*% iv.mean)
		factor.mean[dv] <- intercept[dv] + mean.reg
		if(i < (length(set) - 1)) {
			agg <- c(iv, dv)
			iv.mean <- factor.mean[agg]
		}
	}
	return(as.vector(factor.mean))
}

find.measurement.error.var <- function(loading, latent.cor, indicator.var = NULL, factor.var = NULL) {
	#browser()
	if(sum(diag(latent.cor)) == 0) diag(latent.cor) <- 1
	ni <- nrow(loading)
	nk <- ncol(loading)
	if(is.null(factor.var)) factor.var <- rep(1, nk)
	if(is.null(indicator.var)) indicator.var <- rep(1, ni)
	factor.cov <- cor2cov(latent.cor, sqrt(factor.var))
	factor.part <- loading %*% factor.cov %*% t(loading)
	error.var <- indicator.var - diag(factor.part)
	return(as.vector(error.var))
}

find.indicator.var <- function(loading, latent.cor, error.var, factor.var = NULL) {
	ni <- nrow(loading)
	nk <- ncol(loading)
	if(is.null(factor.var)) factor.var <- rep(1, nk)
	factor.cov <- cor2cov(latent.cor, sqrt(factor.var))
	factor.part <- loading %*% factor.cov %*% t(loading)
	indicator.var <- diag(factor.part) + error.var
	return(as.vector(indicator.var))
}

find.measurement.intercept <- function(loading, factor.mean = NULL, indicator.mean = NULL) {
	ni <- nrow(loading)
	nk <- ncol(loading)
	if(is.null(factor.mean)) factor.mean <- rep(0, nk)
	if(is.null(indicator.mean)) indicator.mean <- rep(0, ni)
	factor.part <- loading %*% factor.mean
	intercept <- indicator.mean - factor.part
	return(as.vector(intercept))
}

find.indicator.mean <- function(loading, factor.mean = NULL, intercept = NULL) {
	ni <- nrow(loading)
	nk <- ncol(loading)
	if(is.null(factor.mean)) factor.mean <- rep(0, nk)
	if(is.null(intercept)) intercept <- rep(0, ni)
	factor.part <- loading %*% factor.mean
	indicator.mean <- intercept + factor.part
	return(as.vector(indicator.mean))
}

combine.path.exo.endo <- function(GA, BE, value=0) {
	nk <- ncol(GA)
	ne <- nrow(GA)
	part1 <- matrix(value, nk, nk+ne)
	part2 <- cbind(GA, BE)
	Result <- rbind(part1, part2)
	return(Result)
}

combine.latent.cor.exo.endo <- function(PH, PS, value=0) {
	nk <- ncol(PH)
	ne <- ncol(PS)
	part1.2 <- matrix(value, nk, ne)
	part2.1 <- matrix(value, ne, nk)
	part1 <- cbind(PH, part1.2)
	part2 <- cbind(part2.1, PS)
	Result <- rbind(part1, part2)
	return(Result)
}

combine.measurement.error.exo.endo <- function(TD, TE, TH) {
	part1 <- cbind(TD, TH)
	part2 <- cbind(t(TH), TE)
	Result <- rbind(part1, part2)
	return(Result)
}

combine.loading.exo.endo <- function(LX, LY, value = 0) {
	nx <- nrow(LX)
	ny <- nrow(LY)
	nk <- ncol(LX)
	ne <- ncol(LY)
	part1.2 <- matrix(value, nx, ne)
	part2.1 <- matrix(value, ny, nk)
	part1 <- cbind(LX, part1.2)
	part2 <- cbind(part2.1, LY)
	Result <- rbind(part1, part2)
	return(Result)
}

combine.vector.exo.endo <- function(exo, endo) {
	c(exo, endo)
}

setMethod("summary.short", signature="vector", definition=function(object) {
		print(object)
	}
)

setMethod("summary.short", signature="matrix", definition=function(object) {
		print(object)
	}
)

setMethod("summary", signature="matrixSet", definition=function(object) {
		cat("RANDOM NUMBERS OF MODEL MATRICES\n")
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

validate.covariance <- function(variance, covariance, variance2 = NULL) {
	if(!isSymmetric(covariance)) return(FALSE)
	if(sum(variance < 0) > 0) return(FALSE)
	zero.row <- variance == 0
	if(sum(zero.row) > 0) {
		target.rows <- object@covariance[zero.row, ]
		for(i in 1:nrow(target.rows)) {
			temp <- target.rows[i, -zero.row[i]]
			if(sum(temp != 0) > 0) return(FALSE)
		}
		if(det(covariance < 0)) return(FALSE)
	} else {
		if(det(covariance) <= 0) return(FALSE)
	}
	if(!is.null(variance2)) {
		if(sum(variance2 < 0) > 0) return(FALSE)
	}
	return(TRUE)
}

validate.path <- function(path, var.iv, var.dv) {
	inv.var.iv <- 1/var.iv
	max.path <- sqrt(var.dv) %o% sqrt(inv.var.iv)
	abs.path <- abs(path)
	ifelse(sum(abs.path > max.path) > 0, return(FALSE), return(TRUE))
}

validate.object <- function(object, detail = FALSE) {
	#browser()
	if(!is(object, "matrixSet")) stop("The object is not a matrixSet object")
	if(validate.covariance(object@VPS, object@PS, object@VE) == FALSE) return(FALSE)
	if(object@Tag == "Path" | object@Tag == "Path.exo" | object@Tag == "SEM" | object@Tag == "SEM.exo") {
		if(validate.path(object@BE, object@VE, object@VE) == FALSE) return(FALSE)
	}
	if(object@Tag == "CFA" | object@Tag == "SEM" | object@Tag == "SEM.exo") {
		if(validate.covariance(object@VTE, object@TE, object@VY) == FALSE) return(FALSE)
		if(validate.path(object@LY, object@VE, object@VY) == FALSE) return(FALSE)
	}
	if(object@Tag == "Path.exo" | object@Tag == "SEM.exo") {
		if(validate.covariance(object@VPH, object@PH) == FALSE) return(FALSE)
		if(validate.path(object@GA, object@VPH, object@VE) == FALSE) return(FALSE)
	}
	if(object@Tag == "SEM.exo") {
		if(validate.covariance(object@VTD, object@TD, object@VX) == FALSE) return(FALSE)
		if(validate.path(object@LX, object@VPH, object@VX) == FALSE) return(FALSE)
		maximum.TH <- sqrt(object@VTD) %o% sqrt(object@VTE)
		abs.TH <- abs(object@TH)
		if(sum(abs.TH > maximum.TH) > 0) return(FALSE)
	}
	return(TRUE)
}

setMethod("create.implied.MACS", signature="matrixSet", definition=function(object) {
		new.object <- reduce.matrices(object)
		result <- create.implied.MACS(new.object)
		return(result)
	}
)

expand.matrices <- function(object) {
	if(!is(object, "reducedMatrixSet")) stop("The object is not a reducedMatrixSet object")
	Tag <- object@Tag
	BE <- object@BE
	AL <- object@AL
	TE <- .NULL.matrix
	LY <- object@LY
	TY <- object@TY
	PH <- .NULL.matrix
	GA <- object@GA
	KA <- object@KA
	TD <- .NULL.matrix
	LX <- object@LX
	TX <- object@TX
	TH <- object@TH
	PS <- cov2cor(object@PS)
	VPS <- diag(object@PS)
	VPH <- .NULL.vector
	VTD <- .NULL.vector
	VTE <- .NULL.vector
	VX <- .NULL.vector
	VY <- .NULL.vector
	VE <- .NULL.vector
	MX <- .NULL.vector
	MY <- .NULL.vector
	ME <- .NULL.vector
	if(Tag == "CFA" | Tag == "SEM" | Tag == "SEM.exo") {
		VTE <- diag(object@TE)
		TE <- cov2cor(object@TE)
	} 
	if(Tag == "Path.exo" | Tag == "SEM.exo") {
		VPH <- diag(object@PH)
		PH <- cov2cor(object@PH)
	} 
	if(Tag == "SEM.exo") {
		VTD <- diag(object@TD)
		TD <- cov2cor(object@TD)
	} 
	if(Tag == "CFA") {
		ME <- AL
		VE <- VPS
		VY <- find.indicator.var(LY, PS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
	} else if(Tag == "Path") {
		VE <- find.factor.var(BE, PS, VPS)
		ME <- find.factor.mean(BE, AL)
	} else if(Tag =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
		VE <- temp.VE[(nx + 1):(nx + ny)]
		temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
		ME <- temp.ME[(nx + 1):(nx + ny)]
	} else if(Tag == "SEM") { 
		VE <- find.factor.var(BE, PS, VPS)
		ME <- find.factor.mean(BE, AL)
		VY <- find.indicator.var(LY, PS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
	} else if(Tag == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
		VE <- temp.VE[(nk + 1):(nk + ne)]
		temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
		ME <- temp.ME[(nk + 1):(nk + ne)]
		VY <- find.indicator.var(LY, PS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
		VX <- find.indicator.var(LX, PH, VTD, VPH)
		MX <- find.indicator.mean(LX, KA, TX)
	}
	return(new("matrixSet", Tag=Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
	BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
	LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
	GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH))
}

setMethod("make.labels", signature="vector", definition=function(object, name, package) {
	if(is.null.object(object)) {
		return(.NULL.vector)
	} else {
		Length <- length(object)
		if(package == "OpenMx") {
			for(i in 1:Length) {
				ifelse(is.na(object[i]), object[i] <- paste(name, i, sep = ""), object[i] <- NA)
			}
			return(object)
		} else if(package == "lavaan") {
			object[] <- ""
			return(object)
		}
	}
})

setMethod("make.labels", signature="matrix", definition=function(object, name, package, symmetric=FALSE) {
	if(is.null.object(object)) {
		return(.NULL.matrix)
	} else {
		np <- nrow(object)
		nq <- ncol(object)
		if(package == "OpenMx") {
			if(symmetric) {
				for(i in 1:np) {
					for(j in 1:i) {
						if(is.na(object[i,j])) {
							object[i, j] <- paste(name, i, "_", j, sep = "")
						} else {
							object[i, j] <- NA
						}
						if(i != j) object[j, i] <- object[i,j]
					}
				}
			} else {
				for(i in 1:np) {
					for(j in 1:nq) {
						if(is.na(object[i,j])) {
							object[i, j] <- paste(name, i, "_", j, sep ="")
						} else {
							object[i, j] <- NA
						}
					}
				}
			}
		} else if(package == "lavaan") {
			object[,] <- ""
		}
		return(object)
	}
})

#Create Parameters to estimate


