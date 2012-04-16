# extractVectorNames
# Function -- simsem package
# Extract a vector of parameter names based on specified elements
# Argument:
#	columnName: A column name that we wish to extract
#	keep:	element of the vector that we need to keep
# Return:	
#	columnName:	Original column name
#	newName:	Reordered column name
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 15, 2012

extractVectorNames <- function(columnName, keep=NULL) {
	name <- substr(columnName, 1, 2)
	position <- do.call("c", strsplit(substr(columnName, 3, nchar(columnName)), "_"))
	if(is.null(keep)) keep <- unique(position)
	select <- position %in% keep
	columnName <- columnName[select]
	for(i in 1:length(select)){
		for(j in 1:length(keep)){
			if(position[i] == keep[j]) position[i] <- j
		}
	}
	newName <- paste(name[select], position[select], sep="")
	return(list(columnName, newName))
}

# Example
#vec <- c("TY1", "TY2", "TY3", "TY4", "TY5", "TY6", "TY7")
#extractVectorNames(vec, 5:6)