# summaryMisspec: This function will summarize the obtained fit indices and generate a data frame.

# setMethod("summaryMisspec", signature(object = "SimResultParam"), definition = function(object) {
	# if(isNullObject(object@misspec)) {
		# stop("This object does not have any model misspecification.")
	# } else {
		# misspecAverage <- colMeans(object@misspec, na.rm = TRUE)
		# misspecSE <- sapply(object@misspec, sd, na.rm = TRUE)
		# mis <- data.frame(mean = misspecAverage, sd = misspecSE)
		# return(mis)
	# }
# })
