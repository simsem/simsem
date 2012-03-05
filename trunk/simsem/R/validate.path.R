validate.path <- function(path, var.iv, var.dv) {
	#browser()
	inv.var.iv <- 1/var.iv
	max.path <- sqrt(var.dv) %o% sqrt(inv.var.iv)
	abs.path <- abs(path)
	max.path[var.dv == 0, ] <- abs.path[var.dv == 0, ]
	ifelse(sum(abs.path > max.path) > 0, return(FALSE), return(TRUE))
}
