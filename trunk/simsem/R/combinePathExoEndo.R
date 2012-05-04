# combinePathExoEndo: Combine the regression coefficient matrices (exogenous
# --> endogenous and endogenous --> endogenous)

combinePathExoEndo <- function(GA, BE, value = 0) {
    nk <- ncol(GA)
    ne <- nrow(GA)
    part1 <- matrix(value, nk, nk + ne)
    part2 <- cbind(GA, BE)
    Result <- rbind(part1, part2)
    return(Result)
} 
