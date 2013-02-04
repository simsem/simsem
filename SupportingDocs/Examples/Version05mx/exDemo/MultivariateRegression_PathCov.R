library(simsem)
library(semTools)
library(OpenMx)

###################### Fitting multivariateRegFit

myRegDataCov<-matrix(
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

multivariateRegModel <- mxModel("MultiVariate Regression Path Specification", 
    type="RAM",
    mxData(
        observed=myRegDataCov, 
        type="cov",
        numObs=100,
        means=myRegDataMeans
    ),
    manifestVars=c("w", "x", "y", "z"),
    mxPath(
        from=c("w", "x", "y", "z"), 
        arrows=2,
        free=TRUE, 
        values = c(1, 1, 1, 1),
        labels=c("residualw", "varx", "residualy", "varz")
    ),
    # variance paths
    # -------------------------------------
    mxPath(
        from="x",
        to="z",
        arrows=2,
        free=TRUE,
        values=0.5,
        labels="covxz"
    ), 
    # covariance of x and z
    # -------------------------------------
    mxPath(
        from=c("x","z"),
        to="y",
        arrows=1,
        free=TRUE,
        values=1,
        labels=c("betayx","betayz")
    ), 
    # regression weights for y
    # -------------------------------------
    mxPath(
        from=c("x","z"),
        to="w",
        arrows=1,
        free=TRUE,
        values=1,
        labels=c("betawx","betawz")
    ), 
    # regression weights for w
    # -------------------------------------
    mxPath(
        from="one",
        to=c("w", "x", "y", "z"),
        arrows=1,
        free=TRUE,
        values=c(1, 1),
        labels=c("betaw", "meanx", "betay", "meanz")
    )
    # means and intercepts
    # -------------------------------------
) 
 
multivariateRegFit <- mxRun(multivariateRegModel)
fitMeasuresMx(multivariateRegFit)
multivariateRegFitSim <- sim(10, multivariateRegFit, n = 200)
summary(multivariateRegFitSim)
