setClass("simVector", 
	representation(
		Data="vector",
		Labels="vector"
	), 
	prototype(Data=as.vector(NaN), Labels=as.vector(NaN))
)
