# validateObject: Validate whether the drawn parameters are good (providing an identified model).

# arguments: paramSet A target set of parameters

# value: Return \code{TRUE} if the target parameters are good.

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

# arguments: a) path A regression coefficient or loading matrix
# 			 b) var.iv The variances of variables corresponding to the columns
# 			 c) var.dv The variances of variables corresponding to the rows

# value: Return \code{TRUE} if the target regression coefficient matrix is good.

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
        path <- path[which(singleIV), , drop=FALSE]
        var.dv <- var.dv[singleIV]
        inv.var.iv <- 1/var.iv
        max.path <- sqrt(var.dv) %o% sqrt(inv.var.iv)
        abs.path <- abs(path)
        max.path[var.dv == 0, ] <- abs.path[var.dv == 0, ]
        ifelse(sum(round(abs.path, 6) > round(max.path, 6)) > 0, return(FALSE), return(TRUE))
    }
}

# validateCovariance: Validate whether all elements provides a good covariance matrix

# arguments: a) resVar A vector of residual variances
# 			b) correlation A correlation matrix
# 			c) totalVar A vector of total variances

# value: Return \code{TRUE} if the covariance matrix is good

validateCovariance <- function(resVar, correlation, totalVar = NULL) {
    if (!isSymmetric(correlation)) 
        return(FALSE)
    if (any(is.na(resVar)) || sum(resVar < 0) > 0) {
        return(FALSE)
	}
    zero.row <- resVar == 0
    if (sum(zero.row) > 0) {
        target.rows <- correlation[which(zero.row), , drop = FALSE]
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
