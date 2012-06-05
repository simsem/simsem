# isVarianceConstraint: Check whether all rownames in a constraint matrix
# containing symbols of variance vectors

isVarianceConstraint <- function(Name) {
    W <- getKeywords()
    keywords <- c(W$VTE, W$VTD, W$VPH, W$VPS, W$VX, W$VY, W$VE)
    result <- Name %in% keywords
    if (sum(result) == length(Name)) {
        return(TRUE)
    } else if (sum(result) == 0) {
        return(FALSE)
    } else {
        stop("A constraint matrix was mixed between variance and other types of elements.")
    }
} 
