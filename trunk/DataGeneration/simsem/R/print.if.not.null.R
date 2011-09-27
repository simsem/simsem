print.if.not.null <-
function(object, name) {
	if(!is.null.object(object)) {
		cat(name, "\n")
		summary.short(object)	
	}
}

