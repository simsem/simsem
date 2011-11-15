# adjust
# Methods -- simsem package
# This function will adjust an element in a target object. The specified element may be set to be free parameter with number 
#	or distribution object as starting values. Alternatively, the element can be fixed to be a value (such as 0).
# Generic Function: adjust(target, VirtualDist, position, constant.fixed)
# Argument:
#	Target: 	The target object that you would like to adjust
#	VirtualDist:	The name of VirtualDist.c that you would like to specify (put as character with single or double quotation) 
#				or number that represents fixed values or starting values.
#	position:	The position of element that you would like to adjust, such as \code{"c(1,2)"} for the element in Row 1 and Column 2 in the specified matrix.
#	constant.fixed:	This argument is used when the VirtualDist argument was specified as number. If TRUE (as default), the number is treated as fixed parameters. 
#					If FALSE, the number is treated as a starting value and the element is set to be free parameter.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("adjust", signature(target="SimMatrix"), definition=function(target, VirtualDist, position, constant.fixed=TRUE) {
		if(is.vector(position) && (length(position) == 2)) position <- matrix(position, ncol=2)
		for(i in 1:nrow(position)) {
			if(is.character(VirtualDist)) {
				target@param[position[i,1], position[i,2]] <- VirtualDist
				target@free[position[i,1], position[i,2]] <- NA
			} else if(is.numeric(VirtualDist)) {
				if(constant.fixed) {
					target@param[position[i,1], position[i,2]] <- ""
					target@free[position[i,1], position[i,2]] <- VirtualDist
				} else {
					target@param[position[i,1], position[i,2]] <- as.character(VirtualDist)
					target@free[position[i,1], position[i,2]] <- NA
				}
			} else {
				stop("Please put a number or a name of random distribution object to the VirtualDist attribute")
			}
		}
		return(target) #new("SimMatrix", free=target@free, param=target@param))
	}
)
#Arguments: 
#	target:		SimMatrix.c that users wish adjust an element in it.
# 	VirtualDist:	As descibed in the beginning of the file.
#	position:	As descibed in the beginning of the file.
#	constant.fixed:	As descibed in the beginning of the file.
#Description: This function will substitute the target element to the thing specified in VirtualDist attribute
#Return: 	adjusted SimMatrix.c
#Examples:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#LX <- simMatrix(loading, 0.7)
#summary(LX)
#run(LX)
#u34 <- simUnif(0.3, 0.4)
#LX <- adjust(LX, "u34", c(2, 1))
#summary(LX)
#run(LX)
#LX <- adjust(LX, 0, c(2,1))
#LX <- adjust(LX, 0.5, c(2,2), FALSE)
#summary(LX)
#run(LX)

setMethod("adjust", signature(target="SymMatrix"), definition=function(target, VirtualDist, position, constant.fixed=TRUE) {
		if(is.vector(position) && (length(position) == 2)) position <- matrix(position, ncol=2)
		for(i in 1:nrow(position)) {
			if(is.character(VirtualDist)) {
					target@param[position[i,1], position[i,2]] <- VirtualDist
					target@free[position[i,1], position[i,2]] <- NA
					target@param[position[i,2], position[i,1]] <- VirtualDist
					target@free[position[i,2], position[i,1]] <- NA
			} else if(is.numeric(VirtualDist)) {
				if(constant.fixed) {
					target@param[position[i,1], position[i,2]] <- ""
					target@free[position[i,1], position[i,2]] <- VirtualDist
					target@param[position[i,2], position[i,1]] <- ""
					target@free[position[i,2], position[i,1]] <- VirtualDist
				} else {
					target@param[position[i,1], position[i,2]] <- as.character(VirtualDist)
					target@free[position[i,1], position[i,2]] <- NA
					target@param[position[i,2], position[i,1]] <- as.character(VirtualDist)
					target@free[position[i,2], position[i,1]] <- NA
				}
			} else {
				stop("Please put a number or a name of random distribution object to the VirtualDist attribute")
			}
		}
		return(target) #new("SimMatrix", free=target@free, param=target@param))
	}
)
#Arguments: 
#	target:		SymMatrix.c that users wish adjust an element in it.
# 	VirtualDist:	As descibed in the beginning of the file.
#	position:	As descibed in the beginning of the file.
#	constant.fixed:	As descibed in the beginning of the file.
#Description: This function will substitute the target element to the thing specified in VirtualDist attribute. 
#		After adjustment, the opposite element across the diagonal line will be adjusted too.
#Return: 	adjusted SymMatrix.c

setMethod("adjust", signature(target="SimVector"), definition=function(target, VirtualDist, position, constant.fixed=TRUE) {
		for(i in 1:length(position)) {
			if(is.character(VirtualDist)) {
				target@param[position[i]] <- VirtualDist
				target@free[position[i]] <- NA
			} else if(is.numeric(VirtualDist)) {
				if(constant.fixed) {
				target@param[position[i]] <- ""
				target@free[position[i]] <- VirtualDist
				} else {
				target@param[position[i]] <- as.character(VirtualDist)
				target@free[position[i]] <- NA
				}
			} else {
				stop("Please put a number or a name of random distribution object to the VirtualDist attribute")
			}
		}
		return(target) 
	}
)
#Arguments: 
#	target:		SimVector.c that users wish adjust an element in it.
# 	VirtualDist:	As descibed in the beginning of the file.
#	position:	As descibed in the beginning of the file.
#	constant.fixed:	As descibed in the beginning of the file.
#Description: This function will substitute the target element to the thing specified in VirtualDist attribute
#Return: 	adjusted SimVector.c
#Examples:
#factor.mean <- rep(NA, 2)
#factor.mean.starting <- c(5, 2)
#AL <- simVector(factor.mean, factor.mean.starting)
#run(AL)
#summary(AL)
#n01 <- simNorm(0, 1)
#AL <- adjust(AL, "n01", 2)
#run(AL)
#summary(AL)

#################################
#	adjust("SimSet") is needed