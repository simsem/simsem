library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

####################### Fitting defMeansFit

set.seed(200)
N=500
Sigma <- matrix(c(1,.5,.5,1),2,2)
group1<-mvrnorm(N, c(1,2), Sigma) # use mvrnorm from MASS package
group2<-mvrnorm(N, c(0,0), Sigma)

y<-rbind(group1,group2)           # Bind both groups together by rows
dimnames(y)[2]<-list(c("x","y")); # Add names
def    <-rep(c(1,0),each=N);      # Add a definition variable 2n in 
								  # length for group status
selVars<-c("x","y")               # Make a selection variables object

defMeansModel <- mxModel("Definition  Means Matrix Specification", 
    mxMatrix(
    	type="Symm", 
    	nrow=2, 
    	ncol=2, 
    	free=TRUE, 
    	values=c(1, 0, 1), 
    	name="Sigma"
    ), 
    mxMatrix(
    	type="Full", 
    	nrow=1, 
    	ncol=2, 
    	free=TRUE, 
    	name = "M"
    ),
    mxMatrix(
	    type="Full", 
	    nrow=1, 
	    ncol=2, 
	    free=TRUE, 
	    values=c(0, 0), 
	    name = "beta"
	), 
    mxMatrix(
    	type="Full", 
    	nrow=1, 
    	ncol=2, 
    	free=FALSE, 
    	labels=c("data.def"), 
    	name = "def"
    ),
	mxAlgebra(
		expression=M+beta*def, 
		name = "Mu"
	),
    mxData(
    	observed=data.frame(y,def), 
    	type="raw"
    ), 
    mxExpectationNormal(
    	covariance="Sigma", 
    	means="Mu", 
    	dimnames=selVars
    ),
	mxFitFunctionML()
)

defMeansFit <- mxRun(defMeansModel)
fitMeasuresMx(defMeansFit)
defMeansFitSim <- sim(10, defMeansFit, n = 500, covData = data.frame(def=def))
summary(defMeansFitSim)
