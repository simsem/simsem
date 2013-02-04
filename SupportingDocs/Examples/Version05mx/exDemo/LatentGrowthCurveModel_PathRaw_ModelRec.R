library(simsem)
library(semTools)
library(OpenMx)

####################### Fitting growthCurveFit

data(myLongitudinalData)

growthCurveModel <- mxModel(
    name="Linear Growth Curve Model Path Specification", 
    type="RAM",
    mxData(
    	observed=myLongitudinalData,
        type="raw"
    ),
    manifestVars=c("x1","x2","x3","x4","x5"),
    latentVars=c("intercept","slope")
)

growthCurveModel <- mxModel(
    model = growthCurveModel,
    # residual variances
    mxPath(
    	from=c("x1","x2","x3","x4","x5"), 
        arrows=2,
        free=TRUE, 
        values = c(1, 1, 1, 1, 1),
        labels=c("residual","residual","residual","residual","residual")
    )
)

growthCurveModel <- mxModel(
    model = growthCurveModel,
    # latent variances and covariance
    mxPath(
    	from=c("intercept","slope"), 
        arrows=2,
		connect="unique.pairs",
        free=TRUE, 
        values=c(1, 1, 1),
        labels=c("vari", "cov", "vars")
    )
)

growthCurveModel <- mxModel(
    model = growthCurveModel,
    # intercept loadings
    mxPath(
    	from="intercept",
        to=c("x1","x2","x3","x4","x5"),
        arrows=1,
        free=FALSE,
        values=c(1, 1, 1, 1, 1)
    )
)

growthCurveModel <- mxModel(
    model= growthCurveModel,
    # slope loadings
    mxPath(
    	from="slope",
        to=c("x1","x2","x3","x4","x5"),
        arrows=1,
        free=FALSE,
        values=c(0, 1, 2, 3, 4)
    )
)

growthCurveModel <- mxModel(
    model = growthCurveModel,
    # manifest means
    mxPath(from="one",
        to=c("x1", "x2", "x3", "x4", "x5"),
        arrows=1,
        free=FALSE,
        values=c(0, 0, 0, 0, 0)
    )
)

growthCurveModel <- mxModel(
    model = growthCurveModel,
    # latent means
    mxPath(from="one",
        to=c("intercept", "slope"),
        arrows=1,
        free=TRUE,
        values=c(1, 1),
        labels=c("meani", "means")
    )
)
   
growthCurveFit <- mxRun(growthCurveModel, suppressWarnings=TRUE)
fitMeasuresMx(growthCurveFit)
growthCurveFitSim <- sim(10, growthCurveFit, n = 200)
summary(growthCurveFitSim)
