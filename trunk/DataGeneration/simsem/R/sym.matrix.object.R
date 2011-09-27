sym.matrix.object <-
function(Matrix, name.dist.object = NULL) {
	if(!isSymmetric(Matrix)) {
		stop("The input matrix is not symmetric.")
	}
	Nrow <- nrow(Matrix)
	Result <- matrix.object(Matrix, name.dist.object)
	if(Nrow > 1) {
	for(i in 2:Nrow) {
		for(j in 1:(i-1)) {
			Result@Data[j,i] <- Result@Data[i, j]
			Result@Labels[j, i] <- Result@Labels[i, j]
		}
	}
	}
	return(new("symMatrix", Data=Result@Data, Labels=Result@Labels))
}

