### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Determine design conditions (e.g., N, missingness) required for target power or coverage

#' Find required values for target coverage
#'
#' Computes the value of a varying parameter that yields a specified
#' coverage level. Coverage is computed as \eqn{1 - power}, and the
#' function internally calls \code{\link{findPower}} to determine the
#' corresponding parameter value.
#'
#' @param coverTable A data frame containing coverage results from a
#' simulation study.
#' @param iv The independent variable whose value should be determined.
#' This can be specified either by column name or index.
#' @param target The desired coverage level.
#'
#' @return
#' A value (or set of values) of the varying parameter that achieves the
#' desired coverage level.
#'
#' @seealso
#' \code{\link{findPower}}
#'
#' @export
findCoverage <- function(coverTable, iv, target) {
	ivCol <- grep("iv", colnames(coverTable))
	coverTable[, -ivCol] <- 1 - coverTable[, -ivCol]
	findPower(coverTable, iv, 1 - target)
}

#' Find required values for target power
#'
#' Determines the value of a varying parameter that achieves a specified
#' level of statistical power based on a power table from a simulation
#' study.
#'
#' The function can handle multiple independent variables by splitting
#' the data according to the remaining variables and applying the power
#' search within each subset.
#'
#' @param powerTable A data frame containing power results from a
#' simulation study.
#' @param iv The independent variable whose value should be determined.
#' This can be specified either by column name or index.
#' @param power The desired level of statistical power.
#'
#' @return
#' A value (or matrix of values) of the varying parameter that achieves
#' the specified power level.
#'
#' @seealso
#' \code{\link{findCoverage}}, \code{\link{findTargetPower}}
#'
#' @export
findPower <- function(powerTable, iv, power) {
    ivCol <- grep("iv", colnames(powerTable))
    ivTable <- as.matrix(powerTable[, ivCol])
    dvTable <- as.matrix(powerTable[, -ivCol])
    colnames(ivTable) <- colnames(powerTable)[ivCol]
    ivName <- colnames(powerTable)[ivCol]
    ivName <- gsub("iv.", "", ivName)
    ivName <- gsub("pmMCAR", "MCAR", ivName)
    ivName <- gsub("pmMAR", "MAR", ivName)
    if (is.numeric(iv)) {
        iv <- colnames(powerTable)[iv]
    }
    if (length(grep("iv.", iv)) != 0) 
        iv <- gsub("iv.", "", iv)
    temp <- grep(iv, ivName)
    if (length(temp) == 0) 
        stop("Cannot find the specified target column")
    iv <- temp
    if (length(ivName) == 1) {
        pow <- findTargetPower(ivTable, dvTable, power)
    } else {
        powList <- split(as.data.frame(dvTable), as.data.frame(ivTable[, -iv]))
        ivList <- split(as.data.frame(ivTable[, iv]), as.data.frame(ivTable[, -iv]))
        pow <- mapply(findTargetPower, iv = ivList, dv = powList, power = power)
        temp2 <- as.matrix(unique(ivTable[, -iv]))
        colnames(temp2) <- colnames(ivTable)[-iv]
        pow <- cbind(temp2, t(pow))
        rownames(pow) <- NULL
    }
    return(pow)
}

#' Find parameter value achieving target power
#'
#' Computes the value of a varying parameter that achieves a specified
#' level of statistical power for a single independent variable.
#'
#' This function is used internally by \code{\link{findPower}} when only
#' one varying parameter is present.
#'
#' @param iv A vector of the varying parameter values.
#' @param dv A data frame containing power values for the parameters of
#' interest.
#' @param power The desired level of statistical power.
#'
#' @return
#' The value of the varying parameter that provides the desired power.
#' If \code{NA}, no value in the domain of the varying parameter provides
#' the desired power. If the minimum value of the varying parameter is
#' returned, the minimum value already achieves the target power.
#'
#' @keywords internal
findTargetPower <- function(iv, dv, power) {
    FUN <- function(dv, iv, power) {
        x <- dv > power
        target <- which(x)
        if (length(target) > 0) {
            minIndex <- min(target)
            maxIndex <- max(target)
            if (dv[minIndex] > dv[maxIndex]) {
                return(iv[maxIndex, ])
            } else if (dv[minIndex] < dv[maxIndex]) {
                return(iv[minIndex, ])
            } else {
                return(Inf)
            }
        } else {
            return(NA)
        }
    }
    apply(dv, 2, FUN, iv = iv, power = power)
}
 
