library(simsem)
library(OpenMx)

####################### Fitting amodFit

set.seed(159)
xdat <- data.frame(a=rnorm(10, mean=4.2), b=1:10) # Make data set

amod <- mxModel(	
	name='Row Model',
	mxData(observed=xdat, type='raw'),
	mxAlgebra(sum(filteredDataRow), name = 'rowAlgebra'),
	mxAlgebra(sum(rowResults), name = 'reduceAlgebra'),
	mxRowObjective(
		rowAlgebra='rowAlgebra',
		reduceAlgebra='reduceAlgebra',
		dimnames=c('a','b'))
)

amodFit <- mxRun(amod)
amodFitSim <- sim(10, amodFit, n = 200)
summary(amodFitSim)

###################### Fitting bmodFit

bmod <- mxModel(
	name='Estimation Row Model',
	mxData(observed=xdat, type='raw'),
	mxMatrix(values=.75, ncol=2, nrow=1, free=TRUE, name='M'),
	mxAlgebra((filteredDataRow-M)%^%2, name='rowAlgebra'),
	mxAlgebra(sum(rowResults), name='reduceAlgebra'),
	mxRowObjective(
		rowAlgebra='rowAlgebra',
		reduceAlgebra='reduceAlgebra',
		dimnames=c('a', 'b'))
)

bmodFit <- mxRun(bmod)
bmodFitSim <- sim(10, bmodFit, n = 200)
summary(bmodFitSim)

################ Fitting cmodFit

xdat$a[3] <- NA
xdat$b[5] <- NA

cmod <- mxModel(
	name='Estimation Row Model with Missingness',
	mxData(observed=xdat, type='raw'),
	mxMatrix(values=.75, ncol=2, nrow=1, free=TRUE, name='M'),
	mxAlgebra(omxSelectCols(M, existenceVector), name='fM'),
	mxAlgebra((filteredDataRow-fM)%^%2, name='rowAlgebra'),
	mxAlgebra(sum(rowResults), name='reduceAlgebra'),
	mxRowObjective(
		rowAlgebra='rowAlgebra',
		reduceAlgebra='reduceAlgebra',
		dimnames=c('a', 'b'))
)

cmodFit <- mxRun(cmod)
cmodFitSim <- sim(10, cmodFit, n = 200)
summary(cmodFitSim)

###################### Fitting dmodRun

set.seed(135)
nobs <- 13
adat <- data.frame(x=rnorm(nobs))

dmod <- mxModel(
	name='I will run fast on OpenMx',
	mxMatrix(name='A', nrow=nobs, ncol=1, free=T, values=0.1),
	mxMatrix(name='X', nrow=nobs, ncol=1, free=F, values=as.matrix(adat)),
	mxAlgebra((X-A) %^% 2, name='Row'),
	mxAlgebra(sum(Row), name='Red'),
	mxAlgebraObjective('Red')
)

dmodRun <- mxRun(dmod) # runs super fast := 0.07 sec
dmodRunSim <- sim(10, dmodRun, n = 200)
summary(dmodRunSim)

################## Fitting emodRun

robj1 <- function(model, state) {
	a <- model$A@values
	x <- model$X@values
	return(sum((x - a) ^ 2))
}

emod <- mxModel(
	name='I will run slow on OpenMx',
	mxMatrix(name='A', nrow=nobs, ncol=1, free=T, values=0.1),
	mxMatrix(name='X', nrow=nobs, ncol=1, free=F, values=as.matrix(adat)),
	mxRObjective(robj1)
)

emodRun <- mxRun(emod) # runs super slow := 10.5 sec
emodRunSim <- sim(10, emodRun, n = 200)
summary(emodRunSim)


