combine.measurement.error.exo.endo <- function(TD, TE, TH) {
	part1 <- cbind(TD, TH)
	part2 <- cbind(t(TH), TE)
	Result <- rbind(part1, part2)
	return(Result)
}
