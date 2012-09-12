# validatePath: Validate whether the regression coefficient (or loading) matrix is good

validatePath <- function(path, var.iv, var.dv) {
    # Need to account for multiple independent variables
	if(isTRUE(all.equal(var.iv, round(var.iv)))) var.iv <- round(var.iv)
	if(isTRUE(all.equal(var.dv, round(var.dv)))) var.dv <- round(var.dv)
    singleIV <- apply(path, 1, function(object) {x <- object[!is.na(object)]; sum(x != 0) == 1})
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
