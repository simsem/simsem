combine.object <- function(object1, object2, ...) {}
setGeneric("combine.object")

divide.object <- function(object, constant, ...) {}
setGeneric("divide.object")

create.implied.CM <- function(object) {}
setGeneric("create.implied.CM")

adjust.object <- function(target, simDist, position) {}
setGeneric("adjust.object")

is.null.object <- function(target) {}
setGeneric("is.null.object")

summary.short <- function(object) {}
setGeneric("summary.short")

create.implied.MACS <- function(object) {}
setGeneric("create.implied.MACS")

starting.values <- function(object, trial, ...) {}
setGeneric("starting.values") 

count.random.object <- function(object) {}
setGeneric("count.random.object")

make.labels <- function(object, ...) {}
setGeneric("make.labels")

constrain.matrices <- function(object, simConstraint, ...) {}
setGeneric("constrain.matrices")

find.OpenMx.values <- function(Parameters, Starting.Values) {}
setGeneric("find.OpenMx.values")

model.object <- function(object, ...) {}
setGeneric("model.object")

tag.headers <- function(object, ...) {}
setGeneric("tag.headers")

setClass("simMatrix", 
	representation(
		Data="matrix",
		Labels="matrix"
	)
)

matrix.object <- function(Matrix, name.dist.object = NULL) {
	Nrow <- nrow(Matrix)
	Ncol <- ncol(Matrix)
	Labels <- matrix("", Nrow, Ncol)
	if(is.null(name.dist.object)) {
		return(new("simMatrix", Data=Matrix, Labels=Labels))
	} else {
		if(is.matrix(name.dist.object)) {
			if(nrow(name.dist.object) == Nrow & ncol(name.dist.object) == Ncol) {
				for(i in 1:Nrow) {
					for(j in 1:Ncol) {
						if(is.na(Matrix[i, j])) Labels[i, j] <- name.dist.object[i, j] #first, second)
					}
				}
			} else {
				stop("Desired matrix and labels do not have the same dimensions")
			}
		} else {
			for(i in 1:Nrow) {
				for(j in 1:Ncol) {
					if(is.na(Matrix[i, j])) Labels[i, j] <- name.dist.object #first, second)
				}
			}
		}
		return(new("simMatrix", Data=Matrix, Labels=Labels))
	}
}


setMethod("run", signature="simMatrix", definition= function(object) {
		if(is.null.object(object)) return(.NULL.matrix)
		Matrix <- object@Data
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:Ncol) {
				if(is.na(Matrix[i, j])) {
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

setMethod("summary", signature="simMatrix", definition = function(object) {
		print("Random Full Matrix Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)

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

setMethod("adjust.object", signature(target="simMatrix"), definition=function(target, simDist, position) {
		if(is.vector(position) && (length(position) == 2)) position <- matrix(position, ncol=2)
		for(i in 1:nrow(position)) {
			if(is.character(simDist)) {
				target@Labels[position[i,1], position[i,2]] <- simDist
				target@Data[position[i,1], position[i,2]] <- NA
				if(is(target, "symMatrix")) {
					target@Labels[position[i,2], position[i,1]] <- simDist
					target@Data[position[i,2], position[i,1]] <- NA
				}
			} else if(is.numeric(simDist)) {
				target@Labels[position[i,1], position[i,2]] <- NA
				target@Data[position[i,1], position[i,2]] <- simDist
				if(is(target, "symMatrix")) {
					target@Labels[position[i,2], position[i,1]] <- NA
					target@Data[position[i,2], position[i,1]] <- simDist
				}
			} else {
				stop("Please put a number or a name of random distribution object to the simDist attribute")
			}
		}
		return(target) #new("simMatrix", Data=target@Data, Labels=target@Labels))
	}
)

setMethod("starting.values", signature(object="simMatrix"), definition=function(object, trial, ...) {
		if(is.null.object(object)) return(.NULL.matrix)
		Nrow <- nrow(run(object))
		Ncol <- ncol(run(object))
		Result <- matrix(0, Nrow, Ncol)
		for(i in 1:trial) {
			temp <- run(object)
			Result <- Result + temp
		}
		return(Result / trial)
	}
)

setClass("symMatrix",
	contains = "simMatrix"
)

sym.matrix.object <- function(Matrix, name.dist.object = NULL) {
	if(!isSymmetric(Matrix)) {
		stop("The input matrix is not symmetric.")
	}
	Nrow <- nrow(Matrix)
	Result <- matrix.object(Matrix, name.dist.object)
	if(Nrow > 1) {
	for(i in 2:Nrow) {
		for(j in 1:(i-1)) {
			Result@Data[j,i] <- Result@Data[i, j]
			Result@Labels[j, i] <- Result@Labels[i, j]
		}
	}
	}
	return(new("symMatrix", Data=Result@Data, Labels=Result@Labels))
}

setMethod("run", signature="symMatrix", definition= function(object) {
		if(is.null.object(object)) return(.NULL.matrix)
		Matrix <- object@Data
		Nrow <- nrow(Matrix)
		Ncol <- ncol(Matrix)
		for(i in 1:Nrow) {
			for(j in 1:i) {
				if(is.na(Matrix[i, j])) {
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

setMethod("summary", signature="symMatrix", definition = function(object) {
		print("Random Symmetric Matrix Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)

setClass("simVector", 
	representation(
		Data="vector",
		Labels="vector"
	)
)

vector.object <- function(Vector, name.dist.object = NULL) {
	Length <- length(Vector)
	Labels <- rep("", Length)
	if(is.null(name.dist.object)) {
		return(new("simVector", Data=Vector, Labels=Labels))
	} else {
		if(length(name.dist.object) > 1) {
			if(length(name.dist.object) == Length) {
				for(i in 1:Length) {
					if(is.na(Vector[i])) Labels[i] <- name.dist.object[i]
				}
			} else {
				stop("The length of desired vector and label are not equal")
			}
		} else {
			for(i in 1:Length) {
				if(is.na(Vector[i])) Labels[i] <- name.dist.object
			}
		}
		return(new("simVector", Data=Vector, Labels=Labels))
	}
}

setMethod("run", signature="simVector", definition= function(object) {
		if(is.null.object(object)) return(.NULL.vector)
		Vector <- object@Data
		Length <- length(Vector)
		for(i in 1:Length) {
			if(is.na(Vector[i])) { 
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

setMethod("summary", signature="simVector", definition = function(object) {
		print("Random Vector Object.")
		print("Parameters:")
		print(object@Data)
		print("Starting Values:")
		print(object@Labels)
	}	
)

setMethod("summary.short", signature="simVector", definition = function(object) {
		Data <- object@Data
		Labels <- object@Labels
		for(i in 1:length(Data)) {
			if(is.na(Labels[i])) Labels[i] <- as.character(Data[i])
		}
		print(Labels)
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

setMethod("adjust.object", signature(target="simVector"), definition=function(target, simDist, position) {
		for(i in 1:length(position)) {
			if(is.character(simDist)) {
				target@Labels[position[i]] <- simDist
				target@Data[position[i]] <- NA
			} else if(is.numeric(simDist)) {
				target@Labels[position[i]] <- NA
				target@Data[position[i]] <- simDist
			} else {
				stop("Please put a number or a name of random distribution object to the simDist attribute")
			}
		}
		return(target) #new("simMatrix", Data=target@Data, Labels=target@Labels))
	}
)

setMethod("starting.values", signature(object="simVector"), definition=function(object, trial, ...) {
		if(is.null.object(object)) return(.NULL.vector)
		Length <- length(run(object))
		Result <- rep(0, Length)
		for(i in 1:trial) {
			temp <- run(object)
			Result <- Result + temp
		}
		return(Result / trial)
	}
)

.NULL.vector <- as.vector(NaN)
.NULL.matrix <- as.matrix(NaN)
comment(.NULL.vector) <- "Double NaN"
comment(.NULL.matrix) <- "Double NaN"
.NULL.simMatrix <- new("simMatrix", Data=.NULL.matrix, Labels=.NULL.matrix)
.NULL.symMatrix <- new("symMatrix", Data=.NULL.matrix, Labels=.NULL.matrix)
.NULL.simVector <- new("simVector", Data=.NULL.vector, Labels=.NULL.vector)

setMethod("is.null.object", signature(target="vector"), definition=function(target) {
		if(length(target) == 1) {
			return(is.nan(target))
		} else {
			return(FALSE)
		}
	}
)

setMethod("is.null.object", signature(target="simMatrix"), definition=function(target) {
		if((length(target@Data) == 1) && length(target@Labels == 1)) {
			if(as.vector(is.nan(target@Data)) && as.vector(is.nan(target@Labels))) {
				return(TRUE)
			} else {
				return(FALSE)
			}
		} else {
			return(FALSE)
		}
	}
)

setMethod("is.null.object", signature(target="simVector"), definition=function(target) {
		if((length(target@Data) == 1) && length(target@Labels == 1)) {
			if(is.na(target@Data) && is.na(target@Labels)) {
				return(TRUE)
			} else {
				return(FALSE)
			}
		} else {
			return(FALSE)
		}
	}
)

constant.vector <- function(constant, ni) {
	return(new("simVector", Data=rep(constant, ni), Labels=rep(NA, ni)))
}

setMethod("count.random.object", signature="simMatrix", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Labels <- object@Labels
			return(sum(!is.na(Labels)))
		}
	}
)

setMethod("count.random.object", signature="symMatrix", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Labels <- object@Labels
			return(sum(!is.na(Labels[upper.tri(Labels, diag=TRUE)])))
		}
	}
)

setMethod("count.random.object", signature="simVector", definition=function(object) {
		if(is.null.object(object)) {
			return(0)
		} else {
			Labels <- object@Labels
			return(sum(!is.na(Labels)))
		}
	}
)
