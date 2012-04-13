# constrainMatrices
# Methods -- simsem package
# Impose equality constraint in an object
# Generic Function: constrainMatrices(object, SimEqualCon, ...)
# Argument:
#	object: Desired object that would like to be constrained, such as set of labels matrices or list of parameter matrices
# 	SimEqualCon: SimEqualCon.c or SimREqualCon.c
# 	... : Other arguments, such as analysis package
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

setMethod("constrainMatrices", signature(object="VirtualRSet", SimEqualCon="SimREqualCon"), definition=function(object, SimEqualCon, package) {
	modelType <- object@modelType
	if(modelType != SimEqualCon@modelType) stop("SimEqualCon and VirtualRSet do not have the same tag")
	label.selection <- NULL
	if(modelType == "CFA") {
		label.selection <- c("LY", "TE", "TY", "PS", "AL")
	} else if(modelType == "Path") {
		label.selection <- c("BE", "PS", "AL")	
	} else if(modelType == "Path.exo") {
		label.selection <- c("BE", "PS", "AL", "GA", "PH", "KA")	
	} else if(modelType == "SEM") {
		label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL")
	} else if(modelType == "SEM.exo") {
		label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL", "LX", "TD", "TX", "GA", "PH", "KA", "TH")
	} else {
		stop("Do not have an appropriate tag")
	}
	matrices <- list(LY=object@LY, TE=object@TE, TY=object@TY, BE=object@BE, PS=object@PS, AL=object@AL, LX=object@LX, TD=object@TD, TX=object@TX, GA=object@GA, PH=object@PH, KA=object@KA, TH=object@TH)
	constraint <- SimEqualCon@con
	n.constraint <- length(constraint)
	if(package == "OpenMx") {
		for(j in 1:n.constraint) {
			temp.constraint <- constraint[[j]]
			temp.matrix <- rownames(temp.constraint)[1]
			num <- equalWhich(temp.matrix, label.selection)
			fixedvalue <- NA
			if(isMeanConstraint(rownames(temp.constraint))) {
				fixedvalue <- matrices[[num]][temp.constraint[1, 2]]		
			} else {
				fixedvalue <- matrices[[num]][temp.constraint[1, 2], temp.constraint[1, 3]]
			}		
			for(i in 2:nrow(temp.constraint)) {
				temp.matrix2 <- rownames(temp.constraint)[i]
				num <- equalWhich(temp.matrix2, label.selection)
				if(isMeanConstraint(rownames(temp.constraint))) {
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
			fixedvalue <- writeLavaanConstraint(temp.constraint[1,], temp.matrix)	
			for(i in 2:nrow(temp.constraint)) {
				temp.matrix2 <- rownames(temp.constraint)[i]
				num <- equalWhich(temp.matrix2, label.selection)
				if(isMeanConstraint(rownames(temp.constraint))) {
					matrices[[num]][temp.constraint[i, 2]] <- fixedvalue
				} else {
					matrices[[num]][temp.constraint[i, 2], temp.constraint[i, 3]] <- fixedvalue
					if(temp.matrix2 == "PS" | temp.matrix2 == "PH" | temp.matrix2 == "TD" | temp.matrix2 == "TE") matrices[[num]][temp.constraint[i,2], temp.constraint[i,1]] <- fixedvalue
				}
			}
		}
	}
	Output <- new(is(object)[1], modelType=object@modelType, PS=matrices$PS, BE=matrices$BE, AL=matrices$AL, TE=matrices$TE, LY=matrices$LY, TY=matrices$TY,
		PH=matrices$PH, GA=matrices$GA, KA=matrices$KA, TD=matrices$TD, LX=matrices$LX, TX=matrices$TX, TH=matrices$TH)
	return(Output)
})
#Arguments: 
#	object: 		The desired object constraints impose on. This function put VirtualRSet.c. This class is generic class including SimParam.c,
#					SimLabels.c, and SimRSet.c. Usually SimLabels.c is used. 
#	SimEqualCon:	SimREqualCon.c that save desired constraints.
#Description: 	This function will impose constraint codes in the input object. How to impose the constraint depends on analysis package. This will impose labels for OpenMx and will impose equal statement in lavaan.
#Return: VirtualRSet.c that have constraint codes.

setMethod("constrainMatrices", signature(object="VirtualRSet", SimEqualCon="SimEqualCon"), definition=function(object, SimEqualCon, package) {
	modelType <- object@modelType
	if(modelType != SimEqualCon@modelType) stop("SimEqualCon and VirtualRSet do not have the same tag")
	SimREqualCon <- reduceConstraint(SimEqualCon)
	Output <- constrainMatrices(object, SimREqualCon, package)
	return(Output)
})
#Arguments: 
#	object: 		The desired object constraints impose on. This function put VirtualRSet.c. This class is generic class including SimParam.c,
#					SimLabels.c, and SimRSet.c. Usually SimLabels.c is used. 
#	SimEqualCon:	SimEqualCon.c that save desired constraints.
#Description: 	This function will combine variance and correlation into covariance matrix labels as SimREqualCon and pass to the function that uses SimREqualCon
#Return: VirtualRSet.c that have constraint codes.

#setMethod("constrainMatrices", signature(object="list", SimEqualCon="SimEqualCon"), definition=function(object, SimEqualCon, modelType) {
#	label.selection <- NULL
#	label.selection <- c("LY", "VTE", "TE", "RTE", "VY", "TY", "MY", "BE", "VPS", "PS", "RPS", "VE", "AL", "ME", "LX", "VTD", "TD", "RTD", "VX", "TX", "MX", "GA", "VPH", "PH", "RPH", "KA", "TH", "RTH")
#	matrices <- object
#	constraint <- SimEqualCon@con
#	n.constraint <- length(constraint)
#	for(j in 1:n.constraint) {
#		temp.constraint <- constraint[[j]]
#		temp.matrix <- rownames(temp.constraint)[1]
#		num <- equalWhich(temp.matrix, label.selection)
#		if(num == 0) stop("Cannot recognize the matrix name in the equality constraint")
#		fixedvalue <- NA
#		if(isMeanConstraint(rownames(temp.constraint))) {
#			fixedvalue <- matrices[[temp.matrix]][temp.constraint[1, 2]]		
#		} else {
#			fixedvalue <- matrices[[temp.matrix]][temp.constraint[1, 2], temp.constraint[1, 3]]
#		}		
#		for(i in 2:nrow(temp.constraint)) {
#			temp.matrix2 <- rownames(temp.constraint)[i]
#			num <- equalWhich(temp.matrix2, label.selection)
#			if(num == 0) stop("Cannot recognize the matrix name in the equality constraint")
#			if(isMeanConstraint(rownames(temp.constraint))) {
#				matrices[[temp.matrix2]][temp.constraint[i, 2]] <- fixedvalue
#			} else {
#				matrices[[temp.matrix2]][temp.constraint[i, 2], temp.constraint[i, 3]] <- fixedvalue
#				if(isCorMatrix(matrices[[temp.matrix2]])) matrices[[temp.matrix2]][temp.constraint[i,2], temp.constraint[i,1]] <- fixedvalue
#			}
#		}
#	}
#	return(matrices)
#})
#Arguments: 
#	object: 		List of parameter matrices
#	SimEqualCon:	SimEqualCon.c that save desired constraints.
#Description: 	This function will impose constraint codes in the input object such that the number of the first element in the constraint will be copied to other elements in the constraint. 
#Return: List of parameter matrices that have constraint.

setMethod("constrainMatrices", signature(object="MatrixSet", SimEqualCon="SimEqualCon"), definition=function(object, SimEqualCon) {
	label.selection <- NULL
	label.selection <- c("LY", "VTE", "TE", "RTE", "VY", "TY", "MY", "BE", "VPS", "PS", "RPS", "VE", "AL", "ME", "LX", "VTD", "TD", "RTD", "VX", "TX", "MX", "GA", "VPH", "PH", "RPH", "KA", "TH", "RTH")
	matrices <- object
	constraint <- SimEqualCon@con
	n.constraint <- length(constraint)
	for(j in 1:n.constraint) {
		temp.constraint <- constraint[[j]]
		temp.matrix <- rownames(temp.constraint)[1]
		num <- equalWhich(temp.matrix, label.selection)
		if(num == 0) stop("Cannot recognize the matrix name in the equality constraint")
		fixedvalue <- NA
		if(isMeanConstraint(rownames(temp.constraint))) {
			fixedvalue <- slot(matrices, temp.matrix)[temp.constraint[1, 2]]		
		} else {
			fixedvalue <- slot(matrices, temp.matrix)[temp.constraint[1, 2], temp.constraint[1, 3]]
		}		
		for(i in 2:nrow(temp.constraint)) {
			temp.matrix2 <- rownames(temp.constraint)[i]
			num <- equalWhich(temp.matrix2, label.selection)
			if(num == 0) stop("Cannot recognize the matrix name in the equality constraint")
			if(isMeanConstraint(rownames(temp.constraint))) {
				slot(matrices, temp.matrix2)[temp.constraint[i, 2]] <- fixedvalue
			} else {
				slot(matrices, temp.matrix2)[temp.constraint[i, 2], temp.constraint[i, 3]] <- fixedvalue
				if(isCorMatrix(slot(matrices, temp.matrix2))) slot(matrices, temp.matrix2)[temp.constraint[i,2], temp.constraint[i,1]] <- fixedvalue
			}
		}
	}
	return(matrices)
})
#Arguments: 
#	object: 		List of parameter matrices
#	SimEqualCon:	SimEqualCon.c that save desired constraints.
#Description: 	This function will impose constraint codes in the input object such that the number of the first element in the constraint will be copied to other elements in the constraint. 
#Return: List of parameter matrices that have constraint.
