setMethod("create.implied.MACS", signature="matrixSet", definition=function(object) {
		new.object <- reduce.matrices(object)
		result <- create.implied.MACS(new.object)
		return(result)
	}
)

setMethod("create.implied.MACS", signature="reducedMatrixSet", definition=function(object) {
		implied.mean <- NULL
		implied.covariance <- NULL
		ID <- matrix(0, nrow(object@PS), nrow(object@PS))
		diag(ID) <- 1
		if(object@Tag == "CFA") {
			implied.mean <- object@LY %*% object@AL + object@TY
			implied.covariance <- (object@LY %*% object@PS %*% t(object@LY)) + object@TE
		} else if(object@Tag == "Path" | object@Tag == "SEM") {
			implied.mean <- solve(ID - object@BE) %*% object@AL
			implied.covariance <- solve(ID - object@BE) %*% object@PS %*% t(solve(ID - object@BE))
			if(object@Tag == "SEM") {
				implied.mean <- object@TY + (object@LY %*% implied.mean)
				implied.covariance <- (object@LY %*% implied.covariance %*% t(object@LY)) + object@TE
			}
		} else if(object@Tag == "Path.exo" | object@Tag == "SEM.exo") {
			implied.mean.1 <- as.matrix(object@KA)
			implied.mean.2 <- solve(ID - object@BE) %*% (object@AL + (object@GA %*% object@KA))
			implied.covariance.1.1 <- object@PH
			implied.covariance.1.2 <- object@PH %*% t(object@GA) %*% t(solve(ID - object@BE))
			implied.covariance.2.2 <- solve(ID - object@BE) %*% ((object@GA %*% object@PH %*% t(object@GA)) + object@PS) %*% t(solve(ID - object@BE))
			if(object@Tag == "SEM.exo") {
				implied.mean.1 <- object@TX + (object@LX %*% implied.mean.1)
				implied.mean.2 <- object@TY + (object@LY %*% implied.mean.2)
				implied.covariance.1.1 <- (object@LX %*% implied.covariance.1.1 %*% t(object@LX)) + object@TD
				implied.covariance.1.2 <- (object@LX %*% implied.covariance.1.2 %*% t(object@LY)) + object@TH
				implied.covariance.2.2 <- (object@LY %*% implied.covariance.2.2 %*% t(object@LY)) + object@TE
			}
			implied.mean <- rbind(implied.mean.1, implied.mean.2)
			implied.covariance.2.1 <- t(implied.covariance.1.2)
			implied.covariance.1 <- cbind(implied.covariance.1.1, implied.covariance.1.2)
			implied.covariance.2 <- cbind(implied.covariance.2.1, implied.covariance.2.2)
			implied.covariance <- rbind(implied.covariance.1, implied.covariance.2)
		}
		return(list(M = as.vector(implied.mean), CM = implied.covariance))
	}
)
