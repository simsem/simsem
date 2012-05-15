# isDefault: Check whether a specified SimVector.c was a default SimVector.c such that users did not specify anything. For example, check whether means of indicators are specified as 1.

isDefault <- function(object) {
    if (isNullObject(object)) 
        return(FALSE)
    if (is.null(comment(object))) 
        return(FALSE)
    ifelse(comment(object) == "default", return(TRUE), return(FALSE))
} 
