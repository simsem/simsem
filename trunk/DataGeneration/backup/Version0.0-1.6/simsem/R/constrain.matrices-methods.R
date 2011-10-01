setMethod("constrain.matrices", signature(object="blankReducedMatrixSet"), definition=function(object, simConstraint, package) {
	Tag <- object@Tag
	if(Tag != simConstraint@Tag) stop("simConstraint and blankReducedMatrixSet do not have the same tag")
	if(is(simConstraint, "simConstraint")) simConstraint <- reduce.constraint(simConstraint)
	if(!is(simConstraint, "simReducedConstraint")) stop("Please put simConstraint or simReducedConstraint in the simConstraint attribute")
		label.selection <- NULL
	if(Tag == "CFA") {
		label.selection <- c("LY", "TE", "TY", "PS", "AL")
	} else if(Tag == "Path") {
		label.selection <- c("BE", "PS", "AL")	
	} else if(Tag == "Path.exo") {
		label.selection <- c("BE", "PS", "AL", "GA", "PH", "KA")	
	} else if(Tag == "SEM") {
		label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL")
	} else if(Tag == "SEM.exo") {
		label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL", "LX", "TD", "TX", "GA", "PH", "KA", "TH")
	} else {
		stop("Do not have an appropriate tag")
	}
	matrices <- list(LY=object@LY, TE=object@TE, TY=object@TY, BE=object@BE, PS=object@PS, AL=object@AL, LX=object@LX, TD=object@TD, TX=object@TX, GA=object@GA, PH=object@PH, KA=object@KA, TH=object@TH)
	constraint <- simConstraint@Equality
	n.constraint <- length(constraint)
	if(package == "OpenMx") {
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
					if(temp.matrix2 == "PS" | temp.matrix2 == "PH" | temp.matrix2 == "TD" | temp.matrix2 == "TE") matrices[[num]][temp.constraint[i,2], temp.constraint[i,1]] <- fixedvalue
				}
			}
		}
	} else if (package == "lavaan") {
		for(j in 1:n.constraint) {
			temp.constraint <- constraint[[j]]
			temp.matrix <- rownames(temp.constraint)[1]
			fixedvalue <- write.lavaan.constraint(temp.constraint[1,], temp.matrix)	
			for(i in 2:nrow(temp.constraint)) {
				temp.matrix2 <- rownames(temp.constraint)[i]
				num <- equal.which(temp.matrix2, label.selection)
				if(is.mean.constraint(rownames(temp.constraint))) {
					matrices[[num]][temp.constraint[i, 2]] <- fixedvalue
				} else {
					matrices[[num]][temp.constraint[i, 2], temp.constraint[i, 3]] <- fixedvalue
					if(temp.matrix2 == "PS" | temp.matrix2 == "PH" | temp.matrix2 == "TD" | temp.matrix2 == "TE") matrices[[num]][temp.constraint[i,2], temp.constraint[i,1]] <- fixedvalue
				}
			}
		}
	}
	Output <- new(is(object)[1], Tag=object@Tag, PS=matrices$PS, BE=matrices$BE, AL=matrices$AL, TE=matrices$TE, LY=matrices$LY, TY=matrices$TY,
		PH=matrices$PH, GA=matrices$GA, KA=matrices$KA, TD=matrices$TD, LX=matrices$LX, TX=matrices$TX, TH=matrices$TH)
	return(Output)
})

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
