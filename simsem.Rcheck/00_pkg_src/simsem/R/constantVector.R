# constantVector: Create a constant SimVector class

constantVector <- function(constant, ni) {
    return(new("SimVector", free = rep(constant, ni), value = rep(NA, ni)))
} 
