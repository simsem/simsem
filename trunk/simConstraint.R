setClass("simConstraint", 
	representation(
		Equality="list",
		Tag="character")
)

.NULL.simConstraint <- new("simConstraint", Equality=list(NA), Tag="NA")

setMethod("is.null.object", signature="simConstraint", definition=function(target){
		ifelse(!(is.matrix(target@Equality[[1]])) && is.na(target@Equality[[1]][1]) && target@Tag == "NA", return(TRUE), return(FALSE))
	}
)

is.mean.constraint <- function(Name) {
	keywords <- c(.TX, .TY, .KA, .AL, .MX, .MY, .ME)
	result <- rep(0, length(Name))
	for(i in 1:length(Name)) {
		result[i] <- sum(Name[i] == keywords)
		ifelse(result[i] > 0, result[i] <- 1, result[i] <- 0)
	}
	if(sum(result) == length(Name)) {
		return(TRUE)
	} else if(sum(result) == 0) {
		return(FALSE)
	} else {
		stop("A constraint matrix was mixed between mean and other types of elements.")
	}
}

is.variance.constraint <- function(Name) {
	keywords <- c(.VTE, .VTD, .VPH, .VPS, .VX, .VY, .VE)
	result <- rep(0, length(Name))
	for(i in 1:length(Name)) {
		result[i] <- sum(Name[i] == keywords)
		ifelse(result[i] > 0, result[i] <- 1, result[i] <- 0)
	}
	if(sum(result) == length(Name)) {
		return(TRUE)
	} else if(sum(result) == 0) {
		return(FALSE)
	} else {
		stop("A constraint matrix was mixed between variance and other types of elements.")
	}
}

reassign.names <- function(Tag, Name) {
	result <- rep(NA, length(Name))
	keywords <- NULL
	if(Tag == "CFA") {
		keywords <- list(.loading, .error, .latent.cor, .intercept, .factor.mean, c(.VTE, .VTD), c(.VY, .VX), c(.VPS, .VPH, .VE), c(.MY, .MX))
	} else if(Tag == "Path") {
		keywords <- list(.BE, .PS, .AL, .VPS, .VE, .ME)
	} else if(Tag == "Path.exo") {
		keywords <- list(.BE, .PS, .VPS, .VE, .AL, .ME, .GA, .PH, .VPH, .KA)
	} else if(Tag == "SEM") {
		keywords <- list(.LY, .TE, .VTE, .VY, .TY, .MY, .BE, .PS, .VPS, .VE, .AL, .ME)
	} else if(Tag == "SEM.exo") {
		keywords <- list(.LY, .TE, .VTE, .VY, .TY, .MY, .BE, .PS, .VPS, .VE, .AL, .ME, .LX, .TD, .VTD, .VX, .TX, .MX, .GA, .PH, .VPH, .KA, .TH)
	} else {
		stop("Cannot recognize the Tag name.")
	}
	position <- match.keyword(Name, keywords)
	if(sum(position == 0) > 0) stop(paste("Some matrices' names cannot be assigned in", Tag, "groups"))
	for(i in 1:length(Name)) {
		result[i] <- keywords[[position[i]]][1]
	}
	return(result)
}

constraint.object <- function(..., Tag) {
	List <- list(...)
	Length <- length(List)
	Result <- NULL
	for(i in 1:Length) {
		temp.result <- NULL
		temp.matrix <- List[[i]]
		if(is.matrix(temp.matrix) == FALSE) {
			temp.matrix2 <- as.matrix(temp.matrix)
			rownames(temp.matrix2) <- names(temp.matrix)
			temp.matrix <- temp.matrix2
		}
		rownames(temp.matrix) <- reassign.names(Tag, rownames(temp.matrix))
		if(is.mean.constraint(rownames(temp.matrix)) | is.variance.constraint(rownames(temp.matrix))) {
			temp.result <- matrix(NA, nrow(temp.matrix), 2)
			colnames(temp.result) <- c("Group", "Element")
			rownames(temp.result) <- rownames(temp.matrix)
			if(ncol(temp.matrix) == 1) {
				temp.result[,2] <- temp.matrix[,1]
			} else if(ncol(temp.matrix) == 2) {
				temp.result[,1:2] <- temp.matrix[,1:2]			
			}
		} else {
			temp.result <- matrix(NA, nrow(temp.matrix), 3)
			colnames(temp.result) <- c("Group", "Row", "Column")
			rownames(temp.result) <- rownames(temp.matrix)
			if(ncol(temp.matrix) == 2) {
				temp.result[,2:3] <- temp.matrix[,1:2]		
			} else if(ncol(temp.matrix) == 3) {
				temp.result[,1:3] <- temp.matrix[,1:3]			
			}
		}
		Result[[i]] <- as.matrix(temp.result)
	}
	return(new("simConstraint", Equality=Result, Tag=Tag))
}

equal.which <- function(x, vec) {
	n.elem <- length(vec)
	result <- 0
	for(i in 1:n.elem) {
		if(sum(vec[i] == x) > 0) return(i)
	}
	return(result)
}

is.cor.matrix <- function(matrixA) {
	if(dim(matrixA)[1] != dim(matrixA)[2]) {
		return(FALSE)
	} else if(sum(is.na(diag(matrixA))) > 0) {
		return(FALSE)
	} else {
		result <- TRUE
		for(i in 1:dim(matrixA)[1]) {
			if(matrixA[i, i] != 1) result <- FALSE
		}
		return(result)
	}
}

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


write.lavaan.constraint <- function(object, temp.matrix) {
	output <- NULL
	if(!is.na(object[1])) output <- paste("g", object[1], ".", sep="")
	if(length(object) == 2) {
		if(temp.matrix == "AL") {
			output <- paste(output, "e", object[2], " ~ 1", sep = "")
		} else if(temp.matrix == "KA") {
			output <- paste(output, "k", object[2], " ~ 1", sep = "")
		} else if(temp.matrix == "TX") {
			output <- paste(output, "x", object[2], " ~ 1", sep = "")
		} else if(temp.matrix == "TY") {
			output <- paste(output, "y", object[2], " ~ 1", sep = "")
		}
	} else if(length(object) == 3) {
		if(temp.matrix == "LY") {
			output <- paste(output, "e", object[3], " =~ ", "y", object[2], sep = "")
		} else if(temp.matrix == "TE") {
			output <- paste(output, "y", object[3], " ~~ ", "y", object[2], sep = "")
		} else if(temp.matrix == "BE") {
			output <- paste(output, "e", object[2], " ~ ", "e", object[3], sep = "")
		} else if(temp.matrix == "PS") {
			output <- paste(output, "e", object[3], " ~~ ", "e", object[2], sep = "")
		} else if(temp.matrix == "LX") {
			output <- paste(output, "k", object[3], " =~ ", "x", object[2], sep = "")
		} else if(temp.matrix == "TD") {
			output <- paste(output, "x", object[3], " ~~ ", "x", object[2], sep = "")
		} else if(temp.matrix == "GA") {
			output <- paste(output, "e", object[2], " ~ ", "k", object[3], sep = "")
		} else if(temp.matrix == "PH") {
			output <- paste(output, "k", object[3], " ~~ ", "k", object[2], sep = "")
		} else if(temp.matrix == "TH") {
			output <- paste(output, "y", object[3], " ~~ ", "x", object[2], sep = "")
		}
	}
	the.front <- 'equal("'
	the.end <- '")*'
	output <- paste(the.front, output, the.end, sep="")
	return(output)
}
			
# constrain.matrices <- function(object, simConstraint) {
	# if(object@Tag != simConstraint@Tag) stop("Please provide same tags of matrixSet and constraint")
	# Tag <- object@Tag
	# label.selection <- NULL
	# if(Tag == "CFA") {
		# label.selection <- c("LY", "TE", "TY", "PS", "AL")
	# } else if(Tag == "Path") {
		# label.selection <- c("BE", "PS", "AL")	
	# } else if(Tag == "Path.exo") {
		# label.selection <- c("BE", "PS", "AL", "GA", "PH", "KA")	
	# } else if(Tag == "SEM") {
		# label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL")
	# } else if(Tag == "SEM.exo") {
		# label.selection <- c("LY", "TE", "TY", "BE", "PS", "AL", "LX", "TD", "TX", "GA", "PH", "KA", "TH")
	# } else {
		# stop("Do not have an appropriate tag")
	# }
	# matrices <- list(LY=object@LY, TE=object@TE, TY=object@TY, BE=object@BE, PS=object@PS, AL=object@AL, 
		# LX=object@LX, TD=object@TD, TX=object@TX, GA=object@GA, PH=object@PH, KA=object@KA, TH=object@TH)
	# constraint <- simConstraint@Equality
	# n.constraint <- length(constraint)
	# for(j in 1:n.constraint) {
		# temp.constraint <- constraint[[j]]
		# temp.matrix <- rownames(temp.constraint)[1]
		# num <- equal.which(temp.matrix, label.selection)
		# fixedvalue <- NA
		# if(is.mean.constraint(rownames(temp.constraint))) {
			# fixedvalue <- matrices[[num]][temp.constraint[1, 2]]		
		# } else {
			# fixedvalue <- matrices[[num]][temp.constraint[1, 2], temp.constraint[1, 3]]
		# }		
		# for(i in 2:nrow(temp.constraint)) {
			# temp.matrix2 <- rownames(temp.constraint)[i]
			# num <- equal.which(temp.matrix2, label.selection)
			# if(is.mean.constraint(rownames(temp.constraint))) {
				# matrices[[num]][temp.constraint[i, 2]] <- fixedvalue
			# } else {
				# matrices[[num]][temp.constraint[i, 2], temp.constraint[i, 3]] <- fixedvalue
				# if(is.cor.matrix(matrices[[num]])) matrices[[num]][temp.constraint[i,2], temp.constraint[i,1]] <- fixedvalue
			# }
		# }
	# }
	# Output <- new("reducedMatrixSet", Tag=object@Tag, PS=matrices$PS, BE=matrices$BE, AL=matrices$AL, TE=matrices$TE, LY=matrices$LY, TY=matrices$TY,
		# PH=matrices$PH, GA=matrices$GA, KA=matrices$KA, TD=matrices$TD, LX=matrices$LX, TX=matrices$TX, TH=matrices$TH)
	# return(Output)
# }

setClass("simReducedConstraint", 
	representation(
		Equality="list",
		Tag="character")
)

.NULL.simReducedConstraint <- new("simReducedConstraint", Equality=list(NA), Tag="NA")

setMethod("is.null.object", signature="simReducedConstraint", definition=function(target){
		ifelse(!(is.matrix(target@Equality[[1]])) && is.na(target@Equality[[1]][1]) && target@Tag == "NA", return(TRUE), return(FALSE))
	}
)

reduce.constraint <- function(simConstraint) {
	Tag <- simConstraint@Tag
	Constraint <- simConstraint@Equality
	Length <- length(Constraint)
	Result <- NULL
	runnum <- 1
	for(i in 1:Length) {
		temp.result <- NULL
		temp.matrix <- Constraint[[i]]
		name <- rownames(temp.matrix)
		if(is.mean.constraint(name)) {
			if(sum(!is.element(name, c("ME", "MX", "MY"))) > 0) temp.result <- temp.matrix
		} else if (is.variance.constraint(name)) {
			if(sum(is.element(name, c("VE", "VX", "VY"))) > 0) {
				temp.result <- matrix(0, nrow(temp.matrix), 3)
				temp.result[,1] <- temp.matrix[,1]
				temp.result[,2] <- temp.matrix[,2]
				temp.result[,3] <- temp.matrix[,2]
				for(j in 1:length(name)) {
					if(name[j] == "VTD") name[j] == "TD"
					if(name[j] == "VTE") name[j] == "TE"
					if(name[j] == "VPH") name[j] == "PH"
					if(name[j] == "VPS") name[j] == "PS"
				}
			}
		} else {
			temp.result <- temp.matrix
		}
		if(!is.null(temp.result)) {
			Result[[runnum]] <- as.matrix(temp.result)
			runnum <- runnum + 1
		}
	}
	if(is.null(Result)) Result <- list(NULL)
	return(new("simReducedConstraint", Equality=Result, Tag=simConstraint@Tag))
}

setMethod("summary", signature="simConstraint", definition=function(object){
	cat("CONSTRAINT OBJECT\n")
	cat("Type\n")
	print(object@Tag)
	cat("-------------Constraint----------------\n")
	for(i in 1:length(object@Equality)) {
		cat(i, ".\n", sep="")
		print(object@Equality[[i]])
		cat("---------------------------------------\n")
	}
})
#Check TD, TE, PS, PH -> No diag elements
#Why equal TX does not converge
#summary simReducedConstraint