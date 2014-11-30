library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

################## Fitting bivHetFit

set.seed(200)
rs=.5
xy1 <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
set.seed(200)

rs=.4
xy2 <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))

selVars <- c("X","Y")
summary(xy1)
cov(xy1)
dimnames(xy1) <- list(NULL, selVars)
summary(xy2)
cov(xy2)
dimnames(xy2) <- list(NULL, selVars)

bivHetModel <- mxModel("bivariate Heterogeneity Path Specification",
    mxModel("group1",
        manifestVars= selVars,
        mxPath(
            from=selVars, 
            arrows=2, 
            free=T, 
            values=1, 
            lbound=.01, 
            labels=c("vX1","vY1")
        ),
        mxPath(
            from="X", 
            to="Y", 
            arrows=2, 
            free=T, 
            values=.2, 
            lbound=.01, 
            labels="cXY1"
        ),
        mxPath(
            from="one", 
            to=selVars, 
            arrows=1, 
            free=T, 
            values=c(0.1, -0.1),
            ubound=c(NA, 0.0),
            lbound=c(0.0, NA),
            labels=c("mX1", "mY1")
        ),
        mxData(
            observed=xy1, 
            type="raw", 
        ),
        type="RAM"
        ),
    mxModel("group2",
        manifestVars= selVars,
        mxPath(
            from=selVars, 
            arrows=2, 
            free=T, 
            values=1, 
            lbound=.01, 
            labels=c("vX2","vY2")
        ),
        mxPath(
            from="X", 
            to="Y", 
            arrows=2, 
            free=T, 
            values=.2, 
            lbound=.01, 
            labels="cXY2"
        ),
        mxPath(
            from="one", 
            to=selVars, 
            arrows=1, 
            free=T, 
            values=c(0.1, -0.1),
            ubound=c(NA, 0.0),
            lbound=c(0.0, NA),
            labels=c("mX2", "mY2")
        ),
        mxData(
            observed=xy2, 
            type="raw", 
        ),
        type="RAM"
        ),
    mxAlgebra(
        group1.objective + group2.objective, 
        name="h12"
    ),
    mxFitFunctionAlgebra("h12")
)

bivHetFit <- mxRun(bivHetModel)
fitMeasuresMx(bivHetFit)
bivHetFitSim <- sim(10, bivHetFit, n = list(200, 200))
summary(bivHetFitSim)

##################### Fitting bivHomModel

bivHomModel <- bivHetModel
    bivHomModel[['group2.S']]@labels <- bivHomModel[['group1.S']]@labels
    bivHomModel[['group2.M']]@labels <- bivHomModel[['group1.M']]@labels

bivHomFit <- mxRun(bivHomModel)
fitMeasuresMx(bivHomFit)
bivHomFitSim <- sim(10, bivHomFit, n = list(200, 200))
summary(bivHomFitSim)
