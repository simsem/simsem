# printIfNotNull
# Function -- simsem package
# Provide basic summary of each object if that object is not NULL. Mainly call from summary function from SimSet.c object.
# Function: printIfNotNull(object, name)
# Argument:
#	object: 	Printed object (SimMatrix.c, SymMatrix.c, or SimVector.c)
# 	name: 	Name of this object
# Return: 	NONE. It will print the object instead.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

printIfNotNull <- function(object, name=NULL) {
	if(!isNullObject(object)) {
		if(!is.null(name)) cat(name, "\n")
		summaryShort(object)	
	}
}

#Example:
#AL <- simVector(rep(NA, 5), "0")
#printIfNotNull(AL, "Factor mean")
