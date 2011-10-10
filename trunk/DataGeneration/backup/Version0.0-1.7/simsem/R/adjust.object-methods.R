# adjust.object
# Methods -- simsem package
# This function will adjust an element in a target object. The specified element may be set to be free parameter with number 
#	or distribution object as starting values. Alternatively, the element can be fixed to be a value (such as 0).
# Generic Function: adjust.object(target, simDist, position, constant.fixed)
# Argument:
#	Target: 	The target object that you would like to adjust
#	simDist:	The name of simDist.c that you would like to specify (put as character with single or double quotation) 
#				or number that represents fixed values or starting values.
#	position:	The position of element that you would like to adjust, such as \code{"c(1,2)"} for the element in Row 1 and Column 2 in the specified matrix.
#	constant.fixed:	This argument is used when the simDist argument was specified as number. If TRUE (as default), the number is treated as fixed parameters. 
#					If FALSE, the number is treated as a starting value and the element is set to be free parameter.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("adjust.object", signature(target="simMatrix"), definition=function(target, simDist, position, constant.fixed=TRUE) {
		if(is.vector(position) && (length(position) == 2)) position <- matrix(position, ncol=2)
		for(i in 1:nrow(position)) {
			if(is.character(simDist)) {
				target@Labels[position[i,1], position[i,2]] <- simDist
				target@Data[position[i,1], position[i,2]] <- NA
			} else if(is.numeric(simDist)) {
				if(constant.fixed) {
					target@Labels[position[i,1], position[i,2]] <- ""
					target@Data[position[i,1], position[i,2]] <- simDist
				} else {
					target@Labels[position[i,1], position[i,2]] <- as.character(simDist)
					target@Data[position[i,1], position[i,2]] <- NA
				}
			} else {
				stop("Please put a number or a name of random distribution object to the simDist attribute")
			}
		}
		return(target) #new("simMatrix", Data=target@Data, Labels=target@Labels))
	}
)
#Arguments: 
#	target:		simMatrix.c that users wish adjust an element in it.
# 	simDist:	As descibed in the beginning of the file.
#	position:	As descibed in the beginning of the file.
#	constant.fixed:	As descibed in the beginning of the file.
#Description: This function will substitute the target element to the thing specified in simDist attribute
#Return: 	adjusted simMatrix.c
#Examples:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#LX <- matrix.object(loading, 0.7)
#summary(LX)
#run(LX)
#u34 <- runif.object(0.3, 0.4)
#LX <- adjust.object(LX, "u34", c(2, 1))
#summary(LX)
#run(LX)
#LX <- adjust.object(LX, 0, c(2,1))
#LX <- adjust.object(LX, 0.5, c(2,2), FALSE)
#summary(LX)
#run(LX)

setMethod("adjust.object", signature(target="symMatrix"), definition=function(target, simDist, position, constant.fixed=TRUE) {
		if(is.vector(position) && (length(position) == 2)) position <- matrix(position, ncol=2)
		for(i in 1:nrow(position)) {
			if(is.character(simDist)) {
					target@Labels[position[i,1], position[i,2]] <- simDist
					target@Data[position[i,1], position[i,2]] <- NA
					target@Labels[position[i,2], position[i,1]] <- simDist
					target@Data[position[i,2], position[i,1]] <- NA
			} else if(is.numeric(simDist)) {
				if(constant.fixed) {
					target@Labels[position[i,1], position[i,2]] <- ""
					target@Data[position[i,1], position[i,2]] <- simDist
					target@Labels[position[i,2], position[i,1]] <- ""
					target@Data[position[i,2], position[i,1]] <- simDist
				} else {
					target@Labels[position[i,1], position[i,2]] <- as.character(simDist)
					target@Data[position[i,1], position[i,2]] <- NA
					target@Labels[position[i,2], position[i,1]] <- as.character(simDist)
					target@Data[position[i,2], position[i,1]] <- NA
				}
			} else {
				stop("Please put a number or a name of random distribution object to the simDist attribute")
			}
		}
		return(target) #new("simMatrix", Data=target@Data, Labels=target@Labels))
	}
)
#Arguments: 
#	target:		symMatrix.c that users wish adjust an element in it.
# 	simDist:	As descibed in the beginning of the file.
#	position:	As descibed in the beginning of the file.
#	constant.fixed:	As descibed in the beginning of the file.
#Description: This function will substitute the target element to the thing specified in simDist attribute. 
#		After adjustment, the opposite element across the diagonal line will be adjusted too.
#Return: 	adjusted symMatrix.c

setMethod("adjust.object", signature(target="simVector"), definition=function(target, simDist, position, constant.fixed=TRUE) {
		for(i in 1:length(position)) {
			if(is.character(simDist)) {
				target@Labels[position[i]] <- simDist
				target@Data[position[i]] <- NA
			} else if(is.numeric(simDist)) {
				if(constant.fixed) {
				target@Labels[position[i]] <- ""
				target@Data[position[i]] <- simDist
				} else {
				target@Labels[position[i]] <- as.character(simDist)
				target@Data[position[i]] <- NA
				}
			} else {
				stop("Please put a number or a name of random distribution object to the simDist attribute")
			}
		}
		return(target) 
	}
)
#Arguments: 
#	target:		simVector.c that users wish adjust an element in it.
# 	simDist:	As descibed in the beginning of the file.
#	position:	As descibed in the beginning of the file.
#	constant.fixed:	As descibed in the beginning of the file.
#Description: This function will substitute the target element to the thing specified in simDist attribute
#Return: 	adjusted simVector.c
#Examples:
#factor.mean <- rep(NA, 2)
#factor.mean.starting <- c(5, 2)
#AL <- vector.object(factor.mean, factor.mean.starting)
#run(AL)
#summary(AL)
#n01 <- rnorm.object(0, 1)
#AL <- adjust.object(AL, "n01", 2)
#run(AL)
#summary(AL)

#################################
#	adjust.object("simMatrixSet") is needed