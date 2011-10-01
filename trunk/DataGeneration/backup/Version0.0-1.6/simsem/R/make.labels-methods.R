setMethod("make.labels", signature="vector", definition=function(object, name, package) {
	if(is.null.object(object)) {
		return(new("nullVector"))
	} else {
		Length <- length(object)
		if(package == "OpenMx") {
			for(i in 1:Length) {
				ifelse(is.na(object[i]), object[i] <- paste(name, i, sep = ""), object[i] <- NA)
			}
			return(object)
		} else if(package == "lavaan") {
			object[] <- ""
			return(object)
		}
	}
})

setMethod("make.labels", signature="matrix", definition=function(object, name, package, symmetric=FALSE) {
	if(is.null.object(object)) {
		return(new("nullMatrix"))
	} else {
		np <- nrow(object)
		nq <- ncol(object)
		if(package == "OpenMx") {
			if(symmetric) {
				for(i in 1:np) {
					for(j in 1:i) {
						if(is.na(object[i,j])) {
							object[i, j] <- paste(name, i, "_", j, sep = "")
						} else {
							object[i, j] <- NA
						}
						if(i != j) object[j, i] <- object[i,j]
					}
				}
			} else {
				for(i in 1:np) {
					for(j in 1:nq) {
						if(is.na(object[i,j])) {
							object[i, j] <- paste(name, i, "_", j, sep ="")
						} else {
							object[i, j] <- NA
						}
					}
				}
			}
		} else if(package == "lavaan") {
			object[,] <- ""
		}
		return(object)
	}
})
