find.latent.error.var <- function(path.matrix, latent.cor.matrix, factor.var = NULL) {
	#browser()
	if(sum(diag(latent.cor.matrix)) == 0) diag(latent.cor.matrix) <- 1
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	error.var <- rep(1, ni)
	if(is.null(factor.var)) factor.var <- rep(1, ni)
	error.var[set[[1]]] <- factor.var[set[[1]]]
	iv <- NULL
	iv.cor <- latent.cor.matrix[set[[1]], set[[1]]]
	start.var <- factor.var[set[[1]]]
	iv.cov <- cor2cov(as.matrix(iv.cor), sqrt(start.var))
	for(i in 1:(length(set) - 1)) {
		iv <- c(iv, set[[i]])
		dv <- set[[i + 1]]
		temp.path <- matrix(path.matrix[dv, iv], nrow = length(dv), ncol = length(iv))
		var.reg <- (temp.path %*% iv.cov %*% t(temp.path))
		psi <- latent.cor.matrix[dv, dv]
		psi.sd <- matrix(0, length(dv), length(dv))
		for(j in 1:length(dv)) {
			error.var[dv[j]]  <- factor.var[dv[j]] - var.reg[j, j]
			psi.sd[j, j] <- sqrt(error.var[dv[j]])
		}
		if(i < (length(set) - 1)) {
			psi <- cor2cov(psi, psi.sd)
			real.psi <- matrix(0, length(iv) + length(dv), length(iv) + length(dv))
			real.psi[1:length(iv), 1:length(iv)] <- iv.cov
			real.psi[(length(iv) + 1):(length(iv) + length(dv)), (length(iv) + 1):(length(iv) + length(dv))] <- psi
			agg <- c(iv, dv)
			blank.path <- matrix(0, nrow = length(iv), ncol = length(agg))
			temp.path2 <- path.matrix[dv, agg]
			temp.path2 <- rbind(blank.path, temp.path2)
			ID <- matrix(0, length(agg), length(agg))
			diag(ID) <- 1
			iv.cov <- solve(ID - temp.path2) %*% real.psi %*% t(solve(ID - temp.path2))
		}		
	}
	return(as.vector(error.var))
}
