# isMeanConstraint: Check whether all rownames in a constraint matrix
# containing symbols of means vectors

isMeanConstraint <- function(Name) {
    W <- getKeywords()
    keywords <- c(W$TX, W$TY, W$KA, W$AL, W$MX, W$MY, W$ME)
    result <- Name %in% keywords
    if (sum(result) == length(Name)) {
        return(TRUE)
    } else if (sum(result) == 0) {
        return(FALSE)
    } else {
        stop("A constraint matrix was mixed between mean and other types of elements.")
    }
} 
