shufflePosition <- function(square.matrix, var1, var2) {
	name <- colnames(square.matrix)
	name.var1 <- name[var1]
	name.var2 <- name[var2]
	name[var2] <- name.var1
	name[var1] <- name.var2
	temp.row1 <- square.matrix[var1,]
	temp.row2 <- square.matrix[var2,]
	square.matrix[var2,] <- temp.row1
	square.matrix[var1,] <- temp.row2
	temp.col1 <- square.matrix[,var1]
	temp.col2 <- square.matrix[,var2]
	square.matrix[,var2] <- temp.col1
	square.matrix[,var1] <- temp.col2
	colnames(square.matrix) <- name
	rownames(square.matrix) <- name
	return(square.matrix)
}
