# freeVector: Create a free parameters vector with a starting values in
# SimVector.c

freeVector <- function(start, ni) {
    return(new("SimVector", free = rep(NA, ni), value = rep(start, ni)))
} 
