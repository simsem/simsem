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

setClass("blankReducedMatrixSet", 
	representation(
		Tag="character",
		LY="matrix",
		TE="matrix",
		PS="matrix",
		BE="matrix",
		TY="vector",
		AL="vector",
		LX="matrix",
		TD="matrix",
		PH="matrix",
		GA="matrix",
		TX="vector",
		KA="vector",
		TH="matrix"),
	prototype(
		LY=new("nullMatrix"),
		TE=new("nullMatrix"),
		PS=new("nullMatrix"),
		BE=new("nullMatrix"),
		TY=new("nullVector"),
		AL=new("nullVector"),
		LX=new("nullMatrix"),
		TD=new("nullMatrix"),
		PH=new("nullMatrix"),
		GA=new("nullMatrix"),
		TX=new("nullVector"),
		KA=new("nullVector"),
		TH=new("nullMatrix"))
)

setClass("simConstraint", 
	representation(
		Equality="list",
		Tag="character")
)

setClass("nullSimConstraint", contains="simConstraint", 
	representation(
		Equality="list",
		Tag="character"), 
	prototype(Equality=list(NA), Tag="NA")
)

setClass("simReducedConstraint", 
	representation(
		Equality="list",
		Tag="character")
)

setClass("nullSimReducedConstraint", contains="simReducedConstraint", 
	representation(
		Equality="list",
		Tag="character"), 
	prototype(Equality=list(NA), Tag="NA")
)

setClass("matrixSet", 
	representation(
		Tag="character",
		LY="matrix",
		TE="matrix",
		VTE="vector",
		PS="matrix",
		VPS="vector",
		BE="matrix",
		TY="vector",
		AL="vector",
		ME="vector",
		MY="vector",
		VE="vector",
		VY="vector",
		LX="matrix",
		TD="matrix",
		VTD="vector",
		PH="matrix",
		GA="matrix",
		TX="vector",
		KA="vector",
		MX="vector",
		VPH="vector",
		VX="vector",
		TH="matrix"),
	prototype(
		LY=new("nullMatrix"),
		TE=new("nullMatrix"),
		VTE=new("nullVector"),
		PS=new("nullMatrix"),
		VPS=new("nullVector"),
		BE=new("nullMatrix"),
		TY=new("nullVector"),
		AL=new("nullVector"),
		ME=new("nullVector"),
		MY=new("nullVector"),
		VE=new("nullVector"),
		VY=new("nullVector"),
		LX=new("nullMatrix"),
		TD=new("nullMatrix"),
		VTD=new("nullVector"),
		PH=new("nullMatrix"),
		GA=new("nullMatrix"),
		TX=new("nullVector"),
		KA=new("nullVector"),
		MX=new("nullVector"),
		VPH=new("nullVector"),
		VX=new("nullVector"),
		TH=new("nullMatrix"))
)

setClass("freeParamSet", 
	contains="blankReducedMatrixSet"
)

setClass("labelsSet", 
	contains="blankReducedMatrixSet"
)

setClass("reducedMatrixSet", 
	contains="blankReducedMatrixSet"
)

setClass("simMisspecifiedSet", 
	contains = "simMatrixSet"
)

setClass("nullSimMisspecifiedSet", contains = "simMisspecifiedSet")

setClass("misspecifiedSet", 
	contains = "matrixSet"
)

setClass("simData", 
	representation(
		Tag="character",
		N="numeric",
		Parameters="simMatrixSet",
		Misspecified="simMisspecifiedSet",
		Constraint="simConstraint",
		Constrain.Parameters.Only="logical",
		Misfit.bound="vector",
		Maximum.random="numeric"),
	prototype(
		Misspecified=new("nullSimMisspecifiedSet"),
		Constraint=new("nullSimConstraint"),
		Constrain.Parameters.Only=TRUE,
		Misfit.bound=new("nullVector"),
		Maximum.random=100)
)

setClass("simModel", 
	representation(
		Tag="character",
		Parameters="freeParamSet",
		Starting.Values="reducedMatrixSet",
		Constraint="simConstraint",
		Program="character"), #OpenMx, lavaan
	prototype(
		Constraint=new("nullSimConstraint"),
		Program="OpenMx")
)

setClass("simResult", 
	representation(
		Tag="character",
		Data="simData",
		Model="simModel",
		Replication="numeric",
		Output="data.frame",
		Convergence="numeric",
		Seed="numeric")
)

setClass("subMatrixSet", 
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
