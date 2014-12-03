library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

############################ Fitting oneFactorThresholdFit

nVariables<-5
nFactors<-1
nThresholds<-3
nSubjects<-500
isIdentified<-function(nVariables,nFactors) as.logical(1+sign((nVariables*(nVariables-1)/2) -  nVariables*nFactors + nFactors*(nFactors-1)/2))

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

oneFactorThresholdModel <- mxModel("oneFactorThresholdModel",
    mxMatrix(
        type="Full", 
        nrow=nVariables, 
        ncol=nFactors, 
        free=TRUE, 
        values=0.2, 
        lbound=-.99, 
        ubound=.99, 
        name="facLoadings"
    ),
    mxMatrix(
        type="Unit", 
        nrow=nVariables, 
        ncol=1, 
        name="vectorofOnes"
    ),
    mxAlgebra(
        expression=vectorofOnes - (diag2vec(facLoadings %*% t(facLoadings))) , 
        name="resVariances"
    ),
    mxAlgebra(
        expression=facLoadings %*% t(facLoadings) + vec2diag(resVariances), 
        name="expCovariances"
    ),
    mxMatrix(
        type="Zero", 
        nrow=1, 
        ncol=nVariables, 
        name="expMeans"
    ),
    mxMatrix(
        type="Full", 
        nrow=nThresholds, 
        ncol=nVariables,
        free=TRUE, 
        values=.2,
        lbound=rep( c(-Inf,rep(.01,(nThresholds-1))) , nVariables),
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
    mxData(
        observed=ordinalData, 
        type='raw'
    ),
    mxExpectationNormal(
        covariance="expCovariances", 
        means="expMeans", 
        dimnames=fruitynames, 
        thresholds="expThresholds"
    ),
	mxFitFunctionML()
)

oneFactorThresholdFit <- mxRun(oneFactorThresholdModel, suppressWarnings=TRUE)
fitMeasuresMx(oneFactorThresholdFit)
oneFactorThresholdFitSim <- sim(10, oneFactorThresholdFit, n = 500)
summary(oneFactorThresholdFitSim)
