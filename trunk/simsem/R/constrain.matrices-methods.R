# constrain.matrices
# Methods -- simsem package
# Impose equality constraint in an object
# Generic Function: constrain.matrices(object, simConstraint, ...)
# Argument:
#	object: Desired object that would like to be constrained, such as set of labels matrices or list of parameter matrices
# 	simConstraint: simConstraint.c or simReducedConstraint.c
# 	... : Other arguments, such as analysis package
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("constrain.matrices", signature(object="blankReducedMatrixSet", simConstraint="simReducedConstraint"), definition=function(object, simConstraint, package) {
	Tag <- object@Tag
	if(Tag != simConstraint@Tag) stop("simConstraint and blankReducedMatrixSet do not have the same tag")
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
#Arguments: 
#	object: 		The desired object constraints impose on. This function put blankReducedMatrixSet.c. This class is generic class including freeParamSet.c,
#					labelsSet.c, and reducedMatrixSet.c. Usually labelsSet.c is used. 
#	simConstraint:	simReducedConstraint.c that save desired constraints.
#Description: 	This function will impose constraint codes in the input object. How to impose the constraint depends on analysis package. This will impose labels for OpenMx and will impose equal statement in lavaan.
#Return: blankReducedMatrixSet.c that have constraint codes.

setMethod("constrain.matrices", signature(object="blankReducedMatrixSet", simConstraint="simConstraint"), definition=function(object, simConstraint, package) {
	Tag <- object@Tag
	if(Tag != simConstraint@Tag) stop("simConstraint and blankReducedMatrixSet do not have the same tag")
	simReducedConstraint <- reduce.constraint(simConstraint)
	Output <- constrain.matrices(object, simReducedConstraint, package)
	return(Output)
})
#Arguments: 
#	object: 		The desired object constraints impose on. This function put blankReducedMatrixSet.c. This class is generic class including freeParamSet.c,
#					labelsSet.c, and reducedMatrixSet.c. Usually labelsSet.c is used. 
#	simConstraint:	simConstraint.c that save desired constraints.
#Description: 	This function will combine variance and correlation into covariance matrix labels as simReducedConstraint and pass to the function that uses simReducedConstraint
#Return: blankReducedMatrixSet.c that have constraint codes.

setMethod("constrain.matrices", signature(object="list", simConstraint="simConstraint"), definition=function(object, simConstraint, Tag) {
	label.selection <- NULL
	label.selection <- c("LY", "VTE", "TE", "VY", "TY", "MY", "BE", "VPS", "PS", "VE", "AL", "ME", "LX", "VTD", "TD", "VX", "TX", "MX", "GA", "VPH", "PH", "KA", "TH")
	# if(Tag == "CFA") {
		# label.selection <- c("LY", "TE", "TY", "PS", "AL", "VTE", "VY", "VPS", "MY")
	# } else if(Tag == "Path") {
		# label.selection <- c("BE", "PS", "AL", "VPS", "VE", "ME")	
	# } else if(Tag == "Path.exo") {
		# label.selection <- c("BE", "PS", "AL", "GA", "PH", "KA", "VPS", "VE", "ME", "VPH")	
	# } else if(Tag == "SEM") {
		# label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL", "VTE", "VY", "MY", "VPS", "VE", "ME")
	# } else if(Tag == "SEM.exo") {
		# label.selection <- c("LY", "VTE", "TE", "VY", "TY", "MY", "BE", "VPS", "PS", "VE", "AL", "ME", "LX", "VTD", "TD", "VX", "TX", "MX", "GA", "VPH", "PH", "KA", "TH")
	# } else {
		# stop("Do not have an appropriate tag")
	# }
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
#Arguments: 
#	object: 		List of parameter matrices
#	simConstraint:	simConstraint.c that save desired constraints.
#Description: 	This function will impose constraint codes in the input object such that the number of the first element in the constraint will be copied to other elements in the constraint. 
#Return: List of parameter matrices that have constraint.
