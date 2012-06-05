# findPower: Find a value of a given independent variable that provides a given
# value of power. This function can handle multiple ivs by split data.

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

# findTargetPower: Find a value of a given independent variable that provides a
# given value of power. This function can handle only one independent variable.

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
                return(NaN)
            }
        } else {
            return(NA)
        }
    }
    apply(dv, 2, FUN, iv = iv, power = power)
}
 
