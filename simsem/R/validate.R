
validateObject <- function(paramSet) {
    if (validateCovariance(paramSet$VPS, paramSet$RPS, paramSet$VE) == FALSE) 
        return(FALSE)
    if (!is.null(paramSet$BE)) {
        # Path or SEM
        if (validatePath(paramSet$BE, paramSet$VE, paramSet$VE) == FALSE) 
            return(FALSE)
    }
    if (!is.null(paramSet$LY)) {
        # SEM or CFA
        if (validateCovariance(paramSet$VTE, paramSet$RTE, paramSet$VY) == FALSE) 
            return(FALSE)
        if (validatePath(paramSet$LY, paramSet$VE, paramSet$VY) == FALSE) 
            return(FALSE)
    }
    return(TRUE)
}

# validatePath: Validate whether the regression coefficient (or loading) matrix
# is good

validatePath <- function(path, var.iv, var.dv) {
    # Need to account for multiple independent variables
    if (isTRUE(all.equal(var.iv, round(var.iv)))) 
        var.iv <- round(var.iv)
    if (isTRUE(all.equal(var.dv, round(var.dv)))) 
        var.dv <- round(var.dv)
    singleIV <- apply(path, 1, function(object) {
        x <- object[!is.na(object)]
        sum(x != 0) == 1
    })
    if (all(singleIV == 0)) {
        return(TRUE)
    } else {
        path <- extract(path, row = which(singleIV))
        var.dv <- var.dv[singleIV]
        inv.var.iv <- 1/var.iv
        max.path <- sqrt(var.dv) %o% sqrt(inv.var.iv)
        abs.path <- abs(path)
        max.path[var.dv == 0, ] <- abs.path[var.dv == 0, ]
        ifelse(sum(abs.path > max.path) > 0, return(FALSE), return(TRUE))
    }
}

# validateCovariance: Validate whether all elements provides a good covariance
# matrix

validateCovariance <- function(resVar, correlation, totalVar = NULL) {
    if (!isSymmetric(correlation)) 
        return(FALSE)
    if (sum(resVar < 0) > 0) 
        return(FALSE)
    zero.row <- resVar == 0
    if (sum(zero.row) > 0) {
        target.rows <- extract(correlation, row = which(zero.row), col = NULL)
        for (i in 1:nrow(target.rows)) {
            temp <- target.rows[i, -zero.row[i]]
            if (sum(temp != 0) > 0) 
                return(FALSE)
        }
        if (det(correlation < 0)) 
            return(FALSE)
    } else {
        if (det(correlation) <= 0) 
            return(FALSE)
    }
    if (!is.null(totalVar)) {
        if (sum(totalVar < 0) > 0) 
            return(FALSE)
    }
    return(TRUE)
}

extract <- function(object, row = NULL, col = NULL) {
    if (class(object) == "matrix") {
        if (is.null(row)) 
            row <- 1:nrow(object)
        if (is.null(col)) 
            col <- 1:ncol(object)
        if (length(row) > 1 & length(col) > 1) {
            return(object[row, col])
        } else {
            if (length(col) == 1) 
                return(as.matrix(object[row, col]))
            if (length(row) == 1) 
                return(t(as.matrix(object[row, col])))
        }
    } else {
        stop("This function can be used for a matrix only.")
    }
} 
