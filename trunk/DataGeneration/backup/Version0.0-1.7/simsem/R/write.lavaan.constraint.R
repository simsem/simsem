write.lavaan.constraint <- function(object, temp.matrix) {
	output <- NULL
	if(!is.na(object[1])) output <- paste("g", object[1], ".", sep="")
	if(length(object) == 2) {
		if(temp.matrix == "AL") {
			output <- paste(output, "e", object[2], " ~ 1", sep = "")
		} else if(temp.matrix == "KA") {
			output <- paste(output, "k", object[2], " ~ 1", sep = "")
		} else if(temp.matrix == "TX") {
			output <- paste(output, "x", object[2], " ~ 1", sep = "")
		} else if(temp.matrix == "TY") {
			output <- paste(output, "y", object[2], " ~ 1", sep = "")
		}
	} else if(length(object) == 3) {
		if(temp.matrix == "LY") {
			output <- paste(output, "e", object[3], " =~ ", "y", object[2], sep = "")
		} else if(temp.matrix == "TE") {
			output <- paste(output, "y", object[3], " ~~ ", "y", object[2], sep = "")
		} else if(temp.matrix == "BE") {
			output <- paste(output, "e", object[2], " ~ ", "e", object[3], sep = "")
		} else if(temp.matrix == "PS") {
			output <- paste(output, "e", object[3], " ~~ ", "e", object[2], sep = "")
		} else if(temp.matrix == "LX") {
			output <- paste(output, "k", object[3], " =~ ", "x", object[2], sep = "")
		} else if(temp.matrix == "TD") {
			output <- paste(output, "x", object[3], " ~~ ", "x", object[2], sep = "")
		} else if(temp.matrix == "GA") {
			output <- paste(output, "e", object[2], " ~ ", "k", object[3], sep = "")
		} else if(temp.matrix == "PH") {
			output <- paste(output, "k", object[3], " ~~ ", "k", object[2], sep = "")
		} else if(temp.matrix == "TH") {
			output <- paste(output, "y", object[3], " ~~ ", "x", object[2], sep = "")
		}
	}
	the.front <- 'equal("'
	the.end <- '")*'
	output <- paste(the.front, output, the.end, sep="")
	return(output)
}
