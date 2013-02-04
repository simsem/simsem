library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

######################### Fitting defMeansFit

set.seed(200)
N <- 500
Sigma <- matrix(c(1,.5,.5,1),2,2)
group1 <- mvrnorm(N, c(1,2), Sigma) # use mvrnorm from MASS package
group2 <- mvrnorm(N, c(0,0), Sigma)

y <- rbind(group1,group2)           # Bind both groups together by rows
dimnames(y)[2] <- list(c("x","y")); # Add names
def    <- rep(c(1,0),each=N);      # Add a definition variable 2n 
								  # in length for group status
selVars <- c("x","y")               # Make a selection variables object

defMeansModel <- mxModel("Definition Means Path Specification", 
	type="RAM",
	manifestVars=selVars,
	latentVars  ="DefDummy",
    mxPath(
    	from=c("x","y"), 
    	arrows=2, 
    	free= TRUE, 
    	values=1,  
    	labels=c("Varx","Vary")
    ),
    mxPath(
    	from="x", 
    	to="y", 
    	arrows=2, 
    	free= TRUE, 
    	values=.1, 
    	labels=c("Covxy")
    ),   
    mxPath(
    	from="one", 
    	to=c("x","y"), 
    	arrows=1, 
    	free= TRUE, 
    	values=1, 
    	labels=c("meanx","meany")
    ),
    mxPath(
    	from="one", 
    	to="DefDummy", 
    	arrows=1, 
    	free= FALSE, 
    	values=1, 
    	labels="data.def"
    ),
    mxPath(
    	from="DefDummy", 
    	to=c("x","y"), 
    	arrows=1, 
    	free= TRUE, 
    	values=1, 
    	labels=c("beta_1","beta_2")
    ), 
    mxData(
    	observed=data.frame(y,def), 
    	type="raw"
    )
)

defMeansFit <- mxRun(defMeansModel)
fitMeasuresMx(defMeansFit)
defMeansFitSim <- sim(10, defMeansFit, n = 500, covData = data.frame(def=def))
summary(defMeansFitSim)
