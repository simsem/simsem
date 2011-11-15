# contain
# Function -- simsem package
# Check whether a desired element is in a vector
# Function: contain(element, Vector)
# Argument:
#	element: 	Desired element that would like to be searched
# 	Vector: 	Searching vector
# Return: 	TRUE if the element is in the vector
#			FALSE if the element is not in the vector
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

contain <- function(element, Vector) {
	ifelse(sum(Vector == element) > 0, return(TRUE), return(FALSE))
}

# Example
# contain(0, 1:3)
# contain(1, 1:3)
