# combineMeasurementErrorExoEndo: Combine measurement error correlation from X
# and Y sides into a single matrix

combineMeasurementErrorExoEndo <- function(TD, TE, TH) {
    part1 <- cbind(TD, TH)
    part2 <- cbind(t(TH), TE)
    Result <- rbind(part1, part2)
    return(Result)
} 
