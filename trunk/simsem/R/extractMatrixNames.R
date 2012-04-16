# extractMatricesNames
# Function -- simsem package
# Extract a vector of parameter names based on specified rows and columns
# Argument:
#	columnName: A column name that we wish to extract
#	keepRow:	Row of the matrix that we need to keep
#	keepCol:	Column of the matrix that we need to keep
# Return:	
#	columnName:	Original column name
#	newName:	Reordered column name
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 15, 2012

extractMatrixNames <- function(columnName, keepRow=NULL, keepCol=NULL) {
	name <- substr(columnName, 1, 2)
	position <- do.call("rbind", strsplit(substr(columnName, 3, nchar(columnName)), "_"))
	if(is.null(keepRow)) keepRow <- unique(position[,1])
	if(is.null(keepCol)) keepCol <- unique(position[,2])
	select <- (position[,1] %in% keepRow) & (position[,2] %in% keepCol)
	columnName <- columnName[select]
	for(i in 1:length(select)){
		for(j in 1:length(keepRow)){
			if(position[i, 1] == keepRow[j]) position[i, 1] <- j
		}
	}
	for(i in 1:length(select)){
		for(j in 1:length(keepCol)){
			if(position[i, 2] == keepCol[j]) position[i, 2] <- j
		}
	}
	newName <- paste(name[select], position[select, 1], "_", position[select, 2], sep="")
	return(list(columnName, newName))
}

# Example
#vec <- c("LY1_1", "LY2_1", "LY3_1", "LY4_2", "LY5_2", "LY6_2", "LY7_3")
#extractMatrixNames(vec, 5:6, 2)