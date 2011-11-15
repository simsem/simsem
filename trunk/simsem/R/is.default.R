# is.default
# Function -- simsem package
# Description: Check whether a specified SimVector.c was a default SimVector.c such that users did not specify anything. 
# 		For example, check whether means of indicators are specified as 1.
# Function: is.default(object)
# Argument:
#	object: 	The target object
# Return: 	TRUE if the element is the default SimVector.c
#			FALSE if the element is not the default SimVector.c
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

is.default <- function(object) {
	if(is.null.object(object)) return(FALSE)
	if(is.null(comment(object))) return(FALSE)
	ifelse(comment(object) == "default", return(TRUE), return(FALSE))
}
