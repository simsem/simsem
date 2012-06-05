# extractVectorNames: Extract a vector of parameter names based on specified elements

extractVectorNames <- function(columnName, keep = NULL) {
    name <- substr(columnName, 1, 2)
    position <- do.call("c", strsplit(substr(columnName, 3, nchar(columnName)), "_"))
    if (is.null(keep)) 
        keep <- unique(position)
    select <- position %in% keep
    columnName <- columnName[select]
    for (i in 1:length(select)) {
        for (j in 1:length(keep)) {
            if (position[i] == keep[j]) 
                position[i] <- j
        }
    }
    newName <- paste(name[select], position[select], sep = "")
    return(list(columnName, newName))
} 
