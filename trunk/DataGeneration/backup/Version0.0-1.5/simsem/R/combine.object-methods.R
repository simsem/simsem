setMethod("combine.object", signature(object1="simMatrix", object2="simMatrix"), definition= function(object1, object2) {
		type <- "simMatrix"
		if(is(object1, "symMatrix") && is(object2, "symMatrix")) type <- "symMatrix"
		Labels1 <- object1@Labels
		Labels2 <- object2@Labels
		Nrow <- nrow(Labels1)
		Ncol <- ncol(Labels2)
		new.Labels <- matrix(NA, Nrow, Ncol)
		new.Data <- matrix(NA, Nrow, Ncol)
		if((Nrow != nrow(Labels2)) | (Ncol != ncol(Labels2))) stop("The dimension of objects are not equal")
		for(i in 1:Nrow) {
			for(j in 1:Ncol) {
				if(is.na(Labels1[i, j])) {
					if(is.na(Labels2[i, j])) {
						new.Data[i, j] <- object1@Data[i, j]
					} else {
						new.Labels[i, j] <- Labels2[i, j]
					}
				} else {
					if(is.na(Labels2[i, j])) {
						new.Labels[i, j] <- Labels1[i, j]
					} else {
						new.Labels[i, j] <- Labels2[i, j]
					}
				}
			}
		}
		return(new(type, Data = new.Data, Labels = new.Labels))
	}
)

setMethod("combine.object", signature(object1="simVector", object2="simVector"), definition= function(object1, object2) {
		Labels1 <- object1@Labels
		Labels2 <- object2@Labels
		Length <- length(Labels1)
		new.Labels <- rep(NA, Length)
		new.Data <- rep(NA, Length)
		if(Length != length(Labels2))  stop("The dimension of objects are not equal")
		for(i in 1:Length) {
			if(is.na(Labels1[i])) {
				if(is.na(Labels2[i])) {
					new.Data[i] <- object1@Data[i]
				} else {
					new.Labels[i] <- Labels2[i]
				}
			} else {
				if(is.na(Labels2[i])) {
					new.Labels[i] <- Labels1[i]
				} else {
					new.Labels[i] <- Labels2[i]
				}
			}
		}
		return(new(type, Data = new.Data, Labels = new.Labels))
	}
)

setMethod("combine.object", signature(object1="vector", object2="vector"), definition=function(object1, object2) {
		if(is.null.object(object1)) {
			if(is.null.object(object2)) {
				return(new("nullVector"))
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
				return(new("nullMatrix"))
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
