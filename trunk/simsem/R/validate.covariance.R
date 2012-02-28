validate.covariance <- function(variance, covariance, variance2 = NULL) {
	#browser()
	if(!isSymmetric(covariance)) return(FALSE)
	if(sum(variance < 0) > 0) return(FALSE)
	zero.row <- variance == 0
	if(sum(zero.row) > 0) {
		target.rows <- extract(covariance, row=which(zero.row), col=NULL)
		for(i in 1:nrow(target.rows)) {
			temp <- target.rows[i, -zero.row[i]]
			if(sum(temp != 0) > 0) return(FALSE)
		}
		if(det(covariance < 0)) return(FALSE)
	} else {
		if(det(covariance) <= 0) return(FALSE)
	}
	if(!is.null(variance2)) {
		if(sum(variance2 < 0) > 0) return(FALSE)
	}
	return(TRUE)
}
