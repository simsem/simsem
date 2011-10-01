setClass("Runif",
	representation(
		Lower="numeric",
		Upper="numeric"
	)
)

setClass("Rnorm",
	representation(
		Mean="numeric",
		SD="numeric"
	)
)

setClassUnion("simDist", c("Runif", "Rnorm"))

setClass("simMatrix", 
	representation(
		Data="matrix",
		Labels="matrix"
	), 
	prototype(Data=as.matrix(NaN), Labels=as.matrix(NaN))
)

setClass("symMatrix",
	contains = "simMatrix"
)

setClass("simVector", 
	representation(
		Data="vector",
		Labels="vector"
	), 
	prototype(Data=as.vector(NaN), Labels=as.vector(NaN))
)

setClass("nullVector", contains = "vector")
setClass("nullMatrix", contains = "matrix")
setClass("nullSimMatrix", contains="simMatrix")
setClass("nullSymMatrix", contains="symMatrix")
setClass("nullSimVector", contains="simVector")

