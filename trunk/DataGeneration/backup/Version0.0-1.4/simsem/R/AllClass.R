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

setClass("simMatrixSet", 
	representation(
		Tag="character", #Path, Path.exo, CFA, SEM, SEM.exo
		LY="simMatrix",
		TE="symMatrix",
		VTE="simVector",
		PS="symMatrix",
		VPS="simVector",
		BE="simMatrix",
		TY="simVector",
		AL="simVector",
		ME="simVector",
		MY="simVector",
		VE="simVector",
		VY="simVector",
		LX="simMatrix",
		TD="symMatrix",
		VTD="simVector",
		PH="symMatrix",
		GA="simMatrix",
		TX="simVector",
		KA="simVector",
		MX="simVector",
		VPH="simVector",
		VX="simVector",
		TH="simMatrix"), #Delta on rows, epsilon on columns
	prototype(
		LY=new("nullSimMatrix"),
		TE=new("nullSymMatrix"),
		VTE=new("nullSimVector"),
		PS=new("nullSymMatrix"),
		VPS=new("nullSimVector"),
		BE=new("nullSimMatrix"),
		TY=new("nullSimVector"),
		AL=new("nullSimVector"),
		ME=new("nullSimVector"),
		MY=new("nullSimVector"),
		VE=new("nullSimVector"),
		VY=new("nullSimVector"), 
		LX=new("nullSimMatrix"),
		TD=new("nullSymMatrix"),
		VTD=new("nullSimVector"),
		PH=new("nullSymMatrix"),
		GA=new("nullSimMatrix"),
		TX=new("nullSimVector"),
		KA=new("nullSimVector"),
		MX=new("nullSimVector"),
		VPH=new("nullSimVector"),
		VX=new("nullSimVector"),
		TH=new("nullSimMatrix"))
)

setClass("nullSimMatrixSet", contains="simMatrixSet")
