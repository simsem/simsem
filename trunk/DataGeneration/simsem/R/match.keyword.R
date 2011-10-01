match.keyword <- function(Names, keywords) {
	Length <- length(Names)
	Result <- rep(NA, Length)
	for(i in 1:Length) {
		temp <- 0
		for(j in 1:length(keywords)) {
			temp.compare <- keywords[[j]]
			if(sum(temp.compare == Names[i]) != 0) temp <- j
		}
		Result[i] <- temp
	}
	return(Result)
}
