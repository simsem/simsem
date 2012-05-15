# combineLatentCorExoEndof: Combine exogenous factor correlation, PH, and endogenous factor correlation, PS, into a single matrix

combineLatentCorExoEndo <- function(PH, PS, value = 0) {
    nk <- ncol(PH)
    ne <- ncol(PS)
    part1.2 <- matrix(value, nk, ne)
    part2.1 <- matrix(value, ne, nk)
    part1 <- cbind(PH, part1.2)
    part2 <- cbind(part2.1, PS)
    Result <- rbind(part1, part2)
    return(Result)
} 
