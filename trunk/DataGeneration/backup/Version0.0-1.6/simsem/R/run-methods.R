setMethod("run",
    signature(object = "Rnorm"),
    function (object) 
    {
        rnorm(1,object@Mean, object@SD)
    }
)

setMethod("run",
    signature(object = "Runif"),
    function (object) 
    {
        runif(1,object@Lower, object@Upper)
    }
)

setMethod("run",
    signature(object = "simMatrix"),
    function (object) 
    {
		Matrix <- object@Data
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:Ncol) {
				if(is.na(Matrix[i, j]) & !is.nan(Matrix[i, j])) {
					temp <- suppressWarnings(as.numeric(object@Labels[i,j]))
					if(is.na(temp)) {
						Matrix[i, j] <- run(get(object@Labels[i,j])) #first, second)
					} else {
						Matrix[i, j] <- temp
					}
				}
			}
		}
		return(Matrix)
    }
)

setMethod("run", signature="symMatrix", definition= function(object) {
		if(is.null.object(object)) return(new("nullMatrix"))
		Matrix <- object@Data
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:i) {
				if(is.na(Matrix[i, j]) & !is.nan(Matrix[i, j])) {
					temp <- suppressWarnings(as.numeric(object@Labels[i,j]))
					if(is.na(temp)) {
						Matrix[i, j] <- run(get(object@Labels[i,j])) #first, second)
					} else {
						Matrix[i, j] <- temp
					}
					Matrix[j, i] <- Matrix[i, j]
				}
			}
		}
		return(Matrix)
	}
)

setMethod("run", signature="simVector", definition= function(object) {
		if(is.null.object(object)) return(new("nullVector"))
		Vector <- object@Data
		Length <- length(Vector)
		for(i in 1:Length) {
			if(is.na(Vector[i]) & !is.nan(Vector[i])) { 
				temp <- suppressWarnings(as.numeric(object@Labels[i]))
				if(is.na(temp)) {
					Vector[i] <- run(get(object@Labels[i]))  #first, second)
				} else {
					Vector[i] <- temp
				}
			}
		}
		return(Vector)
	}
)

setMethod("run",
    signature(object = "nullSimMatrix"),
    function (object) 
    {
		return(new("nullMatrix"))
    }
)

setMethod("run", signature="nullSymMatrix", definition= function(object) {
		return(new("nullMatrix"))
	}
)

setMethod("run", signature="nullSimVector", definition= function(object) {
		return(new("nullVector"))
	}
)

setMethod("run", signature(object="simMatrixSet"), definition=function(object, simConstraint=new("nullSimConstraint")) {
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

setMethod("run", signature(object="simMisspecifiedSet"), definition=function(object) {
		LY <- run(object@LY)
		VTE <- run(object@VTE)
		TE <- run(object@TE)
		VY <- run(object@VY)
		TY <- run(object@TY)
		MY <- run(object@MY)
		BE <- run(object@BE)
		VPS <- run(object@VPS)
		PS <- run(object@PS)
		VE <- run(object@VE)
		AL <- run(object@AL)
		ME <- run(object@ME)
		LX <- run(object@LX)
		VTD <- run(object@VTD)
		TD <- run(object@TD)
		VX <- run(object@VX)
		TX <- run(object@TX)
		MX <- run(object@MX)
		GA <- run(object@GA)
		VPH <- run(object@VPH)
		PH <- run(object@PH)
		KA <- run(object@KA)
		TH <- run(object@TH)
		return(new("misspecifiedSet", Tag=object@Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH))
	}
)

setMethod("run", signature="simData", definition=function(object, N=NULL) {
	if(!require(MASS)) stop("Please install MASS package")
	Tag <- object@Tag
	if(is.null(N)) N <- object@N
	Parameters <- NULL
	Misspecified <- NULL
	implied.CM.Parameters <- NULL
	implied.CM.Misspecified <- NULL
	misfit <- NULL
	count <- 0
	repeat {
		#browser()
		if(!is.null.object(object@Misspecified)) {
			Output <- run.misspecified(object@Parameters, object@Misspecified, object@Constraint, object@Constrain.Parameters.Only)
			Parameters <- Output$Parameters
			Misspecified <- Output$Misspecified
			if(validate.object(Parameters) | validate.object(Misspecified)) {
				Parameters <- reduce.matrices(Parameters)
				Misspecified <- reduce.matrices(Misspecified)
				implied.CM.Parameters <- create.implied.MACS(Parameters)
				implied.CM.Misspecified <- create.implied.MACS(Misspecified)
				misfit <- average.misfit(implied.CM.Misspecified$M, implied.CM.Misspecified$CM, 
					implied.CM.Parameters$M, implied.CM.Parameters$CM, count.random.object(object@Misspecified))
				Parameters <- Misspecified # Pretend Misspecified as real parameters for data generation
				if(is.null.object(object@Misfit.bound)) break
				if(misfit > object@Misfit.bound[1] & misfit < object@Misfit.bound[2]) break
			}
		} else {
			Parameters <- run(object@Parameters, simConstraint=object@Constraint)
			if(validate.object(Parameters)) {
				Parameters <- reduce.matrices(Parameters)
				implied.CM.Parameters <- create.implied.MACS(Parameters)
				implied.CM.Misspecified <- implied.CM.Parameters
				break
			}
		}
		count <- count + 1
		if(count > object@Maximum.random) stop("The model cannot make a good set of parameters within limit of maximum random sampling of parameters")
	}
	# if(Tag == "CFA") {
		# factor.score <- mvrnorm(N, Parameters@AL, Parameters@PS)
		# error.score <- mvrnorm(N, rep(0, length(Parameters@TY), Parameters@TE)
		# intercept <- as.data.frame(matrix(Parameters@TY, ncol=length(Parameters@TY), byrow=TRUE))
		# Data <- (factor.score %*% t(Parameters@LY)) + error.score + intercept
	# } else if (Tag == "Path") {
		# error.score <- mvrnorm(N, Parameters@AL, Parameters@PS)
		# ID <- matrix(0, nrow(Parameters@BE), ncol(Parameters@BE))
		# diag(ID) <- 1
		# data <- error.score %*% t(solve(ID - Parameters@BE))
	# } else if (Tag == "Path.exo") {
	
	# } else if (Tag == "SEM") {
	
	# } else if (Tag == "SEM.exo") {
	
	# }
	Data <- mvrnorm(N, implied.CM.Parameters$M, implied.CM.Parameters$CM)
	varnames <- NULL
	if(Tag == "Path.exo") {
		nx <- ncol(run(object@Parameters@PH))
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else if(Tag == "SEM.exo") {
		nx <- nrow(run(object@Parameters@LX))
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}	
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else {
		for(i in 1:ncol(Data)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	}
	colnames(Data) <- varnames
	return(Data)
})

setMethod("run", signature="simModel", definition=function(object, Data) {
	Output <- NULL
	if(object@Program == "OpenMx") {
		Output <- runOpenMx(object, Data)
	} else if (object@Program == "lavaan") {
		Output <- runLavaan(object, Data)
	}
	return(Output)
})
