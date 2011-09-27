is.default <-
function(object) {
	if(is.null.object(object)) return(FALSE)
	if(is.null(comment(object))) return(FALSE)
	ifelse(comment(object) == "default", return(TRUE), return(FALSE))
}

