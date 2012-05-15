# cov2corMod: The cov2cor function that takes care of the zero-variance variables

cov2corMod <- function(V) {
    targetCol <- which(diag(V) != 0)
    V[targetCol, targetCol] <- cov2cor(V[targetCol, targetCol])
    return(V)
} 
