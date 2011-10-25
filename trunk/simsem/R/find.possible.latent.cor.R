find.possible.latent.cor <- function(path.matrix) {
	ni <- nrow(path.matrix)
	set <- find.recursive.set(path.matrix)
	psi <- matrix(0, ni, ni)
	diag(psi) <- 1
	for(i in 1:length(set)) {
		temp.set <- set[[i]]
		if(length(temp.set) > 1) {
			for(j in 2:length(temp.set)) {
				for(k in 1:(j - 1)) {
					psi[temp.set[j], temp.set[k]] <- NA
					psi[temp.set[k], temp.set[j]] <- NA
				}
			}
		}
	}
	return(psi)
}
