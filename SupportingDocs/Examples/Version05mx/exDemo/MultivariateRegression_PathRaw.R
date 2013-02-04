library(simsem)
library(semTools)
library(OpenMx)

######################### Fitting multivariateRegFit

data(myRegDataRaw)

multivariateRegModel <- mxModel("MultiVariate Regression Path Specification", 
    type="RAM",
    mxData(
        observed=myRegDataRaw, 
        type="raw"
    ),
    manifestVars=c("w", "x", "y", "z"),
    mxPath(
        from=c("w", "x", "y", "z"), 
        arrows=2,
        free=TRUE, 
        values = c(1, 1, 1),
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
