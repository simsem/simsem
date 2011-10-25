# summary.short
# Methods -- simsem package
# Provide short summary if it is available. Otherwise, it is an alias for summary.
# Generic Function: summary.short(object, ...)
# Argument:
#	object: The target object that is used to summarize
# 	... : Other arguments (None is specified currently)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

setMethod("summary.short", signature="ANY", definition = function(object) {
		summary(object)
	}
)
#Arguments: 
#	object:	Anything
#Description: Alias for summary function.
#Return: 	NONE. Results will print on screen only.

setMethod("summary.short", signature="simMatrix", definition = function(object) {
		Data <- object@Data
		Labels <- object@Labels
		for(i in 1:nrow(Data)) {
			for(j in 1:ncol(Data)) {
				if(is.na(Labels[i,j])) Labels[i,j] <- as.character(Data[i,j])
			}
		}
		print(Labels)
	}
)
#Arguments: 
#	object:	simMatrix.c that users wish to summarize shortly
#Description: This function will print all fixed values and free values into the same matrix.
#Return: 	NONE. Results will print on screen only.
#Examples:
#u89 <- runif.object(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- matrix.object(loading, "u89")
#summary.short(LX)

setMethod("summary.short", signature="simVector", definition = function(object) {
		Data <- object@Data
		Labels <- object@Labels
		for(i in 1:length(Data)) {
			if(is.na(Labels[i])) Labels[i] <- as.character(Data[i])
		}
		print(Labels)
	}
)
#Arguments: 
#	object:	simVector.c that users wish to summarize shortly
#Description: This function will print all fixed values and free values into the same matrix.
#Return: 	NONE. Results will print on screen only.

setMethod("summary.short", signature="vector", definition=function(object) {
		print(object)
	}
)
#Arguments: 
#	object:	vector.c that users wish to summarize shortly
#Description: This function will print the object. (Different from summary function from main packages in R).
#Return: 	NONE. Results will print on screen only.

setMethod("summary.short", signature="matrix", definition=function(object) {
		print(object)
	}
)
#Arguments: 
#	object:	matrix.c that users wish to summarize shortly
#Description: This function will print the object. (Different from summary function from main packages in R).
#Return: 	NONE. Results will print on screen only.
