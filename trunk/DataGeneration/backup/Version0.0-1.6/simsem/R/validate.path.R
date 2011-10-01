validate.path <- function(path, var.iv, var.dv) {
	inv.var.iv <- 1/var.iv
	max.path <- sqrt(var.dv) %o% sqrt(inv.var.iv)
	abs.path <- abs(path)
	ifelse(sum(abs.path > max.path) > 0, return(FALSE), return(TRUE))
}
