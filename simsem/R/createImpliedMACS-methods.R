## # createImpliedMACS: Create model implied means and Covariance Matrix (MACS)

## setMethod("createImpliedMACS", signature = "MatrixSet", definition = function(object) {
##     new.object <- reduceMatrices(object)
##     result <- createImpliedMACS(new.object)
##     return(result)
## })

## setMethod("createImpliedMACS", signature = "SimRSet", definition = function(object) {
##     implied.mean <- NULL
##     implied.covariance <- NULL
##     ID <- matrix(0, nrow(object@PS), nrow(object@PS))
##     diag(ID) <- 1
##     if (object@modelType == "CFA") {
##         implied.mean <- object@LY %*% object@AL + object@TY
##         implied.covariance <- (object@LY %*% object@PS %*% t(object@LY)) + object@TE
##     } else if (object@modelType == "Path" | object@modelType == "SEM") {
##         implied.mean <- solve(ID - object@BE) %*% object@AL
##         implied.covariance <- solve(ID - object@BE) %*% object@PS %*% t(solve(ID - object@BE))
##         if (object@modelType == "SEM") {
##             implied.mean <- object@TY + (object@LY %*% implied.mean)
##             implied.covariance <- (object@LY %*% implied.covariance %*% t(object@LY)) + object@TE
##         }
##     } else if (object@modelType == "Path.exo" | object@modelType == "SEM.exo") {
##         implied.mean.1 <- as.matrix(object@KA)
##         implied.mean.2 <- solve(ID - object@BE) %*% (object@AL + (object@GA %*% object@KA))
##         implied.covariance.1.1 <- object@PH
##         implied.covariance.1.2 <- object@PH %*% t(object@GA) %*% t(solve(ID - object@BE))
##         implied.covariance.2.2 <- solve(ID - object@BE) %*% ((object@GA %*% object@PH %*% t(object@GA)) + object@PS) %*% t(solve(ID - 
##             object@BE))
##         if (object@modelType == "SEM.exo") {
##             implied.mean.1 <- object@TX + (object@LX %*% implied.mean.1)
##             implied.mean.2 <- object@TY + (object@LY %*% implied.mean.2)
##             implied.covariance.1.1 <- (object@LX %*% implied.covariance.1.1 %*% t(object@LX)) + object@TD
##             implied.covariance.1.2 <- (object@LX %*% implied.covariance.1.2 %*% t(object@LY)) + object@TH
##             implied.covariance.2.2 <- (object@LY %*% implied.covariance.2.2 %*% t(object@LY)) + object@TE
##         }
##         implied.mean <- rbind(implied.mean.1, implied.mean.2)
##         implied.covariance.2.1 <- t(implied.covariance.1.2)
##         implied.covariance.1 <- cbind(implied.covariance.1.1, implied.covariance.1.2)
##         implied.covariance.2 <- cbind(implied.covariance.2.1, implied.covariance.2.2)
##         implied.covariance <- rbind(implied.covariance.1, implied.covariance.2)
##     }
##     return(list(M = as.vector(implied.mean), CM = implied.covariance))
## })

## setMethod("createImpliedMACS", signature = "SimModelOut", definition = function(object) {
##     result <- createImpliedMACS(object@coef)
##     return(result)
## })

## setMethod("createImpliedMACS", signature = "SimDataOut", definition = function(object, misspec = FALSE) {
##     result <- NULL
##     if (misspec) {
##         if (isNullObject(object@misspecOut)) {
##             stop("The data output object does not have model misspecification.")
##         } else {
##             result <- createImpliedMACS(object@misspecOut)
##         }
##     } else {
##         result <- createImpliedMACS(object@paramOut)
##     }
##     return(result)
## }) 
