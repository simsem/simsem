transform.constraint <- function(object, constraint) {
	object <- blank.parameters(object)
	if(!is.null(constraint)) {
		Equality <- constraint@Equality
		for(i in 1:length(Equality)) {
			current <- Equality[[i]]
			con.text <- type.constraint(rownames(current)[1], current[1,], slot(object, rownames(current)[1]))
			for(j in 2:nrow(current)) {
				Matrix <- rownames(current)[j]
				if(Matrix == "PS" | Matrix == "PH" | Matrix == "TE" | Matrix == "TD") {
					elements <- c(as.numeric(current[j, 2]), as.numeric(current[j, 3]))
					slot(object, Matrix)[max(elements), min(elements)] <- con.text
				} else {
					slot(object, Matrix)[as.numeric(current[j, 2]), as.numeric(current[j, 3])] <- con.text
				}
			#con.text <- Equality[[i]][1,]     get(rownames(current)[j])
		################################################ Right Here###############
			#equal("PA.domain2 ~ 1")*1 
			}
		}
	}
	return(object)
}
