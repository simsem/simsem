library(simsem)
library(semTools)
library(OpenMx)

############################ Fitting uniRegFit

myRegDataCov <- matrix(
    c(0.808,-0.110, 0.089, 0.361,
     -0.110, 1.116, 0.539, 0.289,
      0.089, 0.539, 0.933, 0.312,
      0.361, 0.289, 0.312, 0.836),
    nrow=4,
    dimnames=list(
      c("w","x","y","z"),
      c("w","x","y","z"))
)
 
myRegDataMeans <- c(2.582, 0.054, 2.574, 4.061)
names(myRegDataMeans) <- c("w","x","y","z") 

SimpleDataCov <- myRegDataCov[c("x","y"),c("x","y")]	
SimpleDataMeans <- myRegDataMeans[c(2,3)]
	
myRegDataMeans<-c(0.05416, 2.57393)

 uniRegModel <- mxModel("Simple Regression Path Specification", 
    type="RAM",
    mxData(
        observed=SimpleDataCov, 
        type="cov", 
        numObs=100,
        means=SimpleDataMeans 
    ),
    manifestVars=c("x", "y"),
    mxPath(
        from=c("x", "y"), 
        arrows=2,
        free=TRUE, 
        values = c(1, 1),
        labels=c("varx", "residual")
    ),
    # variance paths
    # -------------------------------------
    mxPath(
        from="x",
        to="y",
        arrows=1,
        free=TRUE,
        values=1,
        labels="beta1"
    ), 
    # regression weights
    # -------------------------------------
    mxPath(
        from="one",
        to=c("x", "y"),
        arrows=1,
        free=TRUE,
        values=c(1, 1),
        labels=c("meanx", "beta0")
    )
    # means and intercepts
    # -------------------------------------
) 
    
uniRegFit <- mxRun(uniRegModel)
fitMeasuresMx(uniRegFit)
uniRegFitSim <- sim(10, uniRegFit, n = 200)
summary(uniRegFitSim)
