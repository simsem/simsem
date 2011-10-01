print.if.not.null <- function(object, name) {
	if(!is.null.object(object)) {
		cat(name, "\n")
		#print(name, quote=FALSE)
		summary.short(object)	
	}
}
