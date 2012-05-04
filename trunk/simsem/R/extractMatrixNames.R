# extractMatricesNames: Extract a vector of parameter names based on specified
# rows and columns

extractMatrixNames <- function(columnName, keepRow = NULL, keepCol = NULL) {
    name <- substr(columnName, 1, 2)
    position <- do.call("rbind", strsplit(substr(columnName, 3, nchar(columnName)), 
        "_"))
    if (is.null(keepRow)) 
        keepRow <- unique(position[, 1])
    if (is.null(keepCol)) 
        keepCol <- unique(position[, 2])
    select <- (position[, 1] %in% keepRow) & (position[, 2] %in% keepCol)
    columnName <- columnName[select]
    for (i in 1:length(select)) {
        for (j in 1:length(keepRow)) {
            if (position[i, 1] == keepRow[j]) 
                position[i, 1] <- j
        }
    }
    for (i in 1:length(select)) {
        for (j in 1:length(keepCol)) {
            if (position[i, 2] == keepCol[j]) 
                position[i, 2] <- j
        }
    }
    newName <- paste(name[select], position[select, 1], "_", position[select, 2], 
        sep = "")
    return(list(columnName, newName))
} 
