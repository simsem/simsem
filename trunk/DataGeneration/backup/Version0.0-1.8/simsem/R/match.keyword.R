# match.keyword
# Function -- simsem package
# Search for the keywords and check whether the specified text match one in the name vector
# Function: match.keyword(Names, keywords)
# Argument:
#	Names: 	Name of the searching object
# 	Vector: 	Name of the keywords vector that would like to matched
# Return: 	The position of keywords in the vector. 0 if the names does not match the specified vector.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

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

#Example:
#	match.keyword("LY", c("LY", "Ly", "ly", "LX", "Lx", "lx"))
