library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

###################### Fitting oneFactorThresholdFit01

nVariables<-5
nFactors<-1
nThresholds<-3
nSubjects<-500
isIdentified<-function(nVariables,nFactors) as.logical(1+sign((nVariables*(nVariables-1)/2) - nVariables*nFactors + nFactors*(nFactors-1)/2))

isIdentified(nVariables,nFactors) # if this function returns FALSE then model is not identified, otherwise it is.

loadings <- matrix(.7,nrow=nVariables,ncol=nFactors)
residuals <- 1 - (loadings * loadings)
sigma <- loadings %*% t(loadings) + vec2diag(residuals)
mu <- matrix(0,nrow=nVariables,ncol=1)

set.seed(1234)
continuousData <- mvrnorm(n=nSubjects,mu,sigma)

quants<-quantile(continuousData[,1],  probs = c((1:nThresholds)/(nThresholds+1)))
ordinalData<-matrix(0,nrow=nSubjects,ncol=nVariables)
for(i in 1:nVariables)
{
ordinalData[,i] <- cut(as.vector(continuousData[,i]),c(-Inf,quants,Inf))
}

ordinalData <- mxFactor(as.data.frame(ordinalData),levels=c(1:(nThresholds+1)))

fruitynames<-paste("banana",1:nVariables,sep="")
names(ordinalData)<-fruitynames

oneFactorThresholdModel01 <- mxModel("oneFactorThresholdModel01",
    mxMatrix(
        type="Full", 
        nrow=nVariables, 
        ncol=nFactors, 
        free=TRUE, 
        values=0.2, 
        lbound=-.99, 
        ubound=2, 
        name="facLoadings"
    ),
    mxMatrix(
        type="Diag", 
        nrow=nVariables, 
        ncol=nVariables,
        free=TRUE,
        values=0.9,
        name="resVariances"
    ),
    mxAlgebra(
        expression=facLoadings %*% t(facLoadings) + resVariances, 
        name="expCovariances"
    ),
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=nVariables,
        free=T,
        name="expMeans"
    ),
    mxMatrix(
        type="Full", 
        nrow=nThresholds, 
        ncol=nVariables,
        free=rep( c(F,F,rep(T,(nThresholds-2))), nVariables), 
        values=rep( c(0,1,rep(.2,(nThresholds-2))), nVariables),
        lbound=rep( c(-Inf,rep(.01,(nThresholds-1))), nVariables),
        dimnames=list(c(), fruitynames),
        name="thresholdDeviations"
    ),
    mxMatrix(
        type="Lower",
        nrow=nThresholds,
        ncol=nThresholds,
        free=FALSE,
        values=1,
        name="unitLower"
    ),
    mxAlgebra(
        expression=unitLower %*% thresholdDeviations, 
        name="expThresholds"
    ),
        mxMatrix(
    	 type="Unit",
    	 nrow=nThresholds,
    	 ncol=1,
    	 name="columnofOnes"
    ),
    mxAlgebra(
    	 expression=expMeans %x% columnofOnes,
    	 name="meansMatrix"
    ),
    mxAlgebra(
    	 expression=sqrt(t(diag2vec(expCovariances))) %x% columnofOnes,
    	 name="variancesMatrix"
    ),
    mxAlgebra(
    	 expression=(expThresholds - meansMatrix) / variancesMatrix,
    	 name="thresholdMatrix"
    ),
    mxMatrix( 
    	type="Iden", 
    	nrow=nVariables, 
    	ncol=nVariables, 
    	name="Identity"
    ),
    mxAlgebra(
    	 expression=solve(sqrt(Identity * expCovariances)) %*% facLoadings,
    	 name="facLoadingsMatrix"
    ),
    mxData(
        observed=ordinalData, 
        type='raw'
    ),
    mxExpectationNormal(
        covariance="expCovariances", 
        means="expMeans", 
        dimnames=fruitynames, 
        thresholds="expThresholds"
    )
)

oneFactorThresholdFit01 <- mxRun(oneFactorThresholdModel01, suppressWarnings=TRUE)
fitMeasuresMx(oneFactorThresholdFit01)
oneFactorThresholdFit01Sim <- sim(10, oneFactorThresholdFit01, n = 500)
summary(oneFactorThresholdFit01Sim)
summaryParam(oneFactorThresholdFit01Sim, improper = TRUE)
summaryFit(oneFactorThresholdFit01Sim, improper = TRUE)
summaryConverge(oneFactorThresholdFit01Sim, improper = TRUE)
