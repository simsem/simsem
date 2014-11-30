library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

###################### Fitting bivHetFit

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

bivHetModel <- mxModel("bivariate Heterogeneity Matrix Specification",
    mxModel("group1",
        mxMatrix(
            type="Lower", 
            nrow=2, 
            ncol=2, 
            free=T, 
            values=.5,
            name="Chol1"
        ), 
        mxAlgebra(
            expression=Chol1 %*% t(Chol1), 
            name="EC1"
        ), 
        mxMatrix(
            type="Full", 
            nrow=1, 
            ncol=2, 
            free=T, 
            values=c(0,0), 
            labels=c("mX1", "mY1"), 
            name="EM1"
        ), 
        mxData(
            xy1, 
            type="raw"
        ), 
        mxExpectationNormal(
            "EC1", 
            "EM1",
            selVars)
        ),
    mxModel("group2",
        mxMatrix(
            type="Lower", 
            nrow=2, 
            ncol=2, 
            free=T, 
            values=.5,
            labels=c("vX2", "cXY2", "vY2"),
            name="Chol2"
        ), 
        mxAlgebra(
            expression=Chol2 %*% t(Chol2), 
            name="EC2" 
        ), 
        mxMatrix(
            type="Full", 
            nrow=1, 
            ncol=2, 
            free=T, 
            values=c(0,0), 
            labels=c("mX2", "mY2"), 
            name="EM2"
        ), 
        mxData(
            xy2, 
            type="raw"
        ), 
        mxExpectationNormal(
            "EC2", 
            "EM2",
            selVars)
        ),
    mxAlgebra(
        group1.objective + group2.objective, 
        name="h12"
    ),
    mxFitFunctionAlgebra("h12")
)

bivHetFit <- mxRun(bivHetModel)
fitMeasuresMx(bivHetFit)
bivHetFitSim <- sim(10, bivHetFit, n = list(100, 100))
summary(bivHetFitSim)

###################### Fitting bivHomFit

bivHomModel <- bivHetModel
    bivHomModel[['group2.Chol2']]@labels <- bivHomModel[['group1.Chol1']]@labels
    bivHomModel[['group2.EM2']]@labels <- bivHomModel[['group1.EM1']]@labels
bivHomFit <- mxRun(bivHomModel)
fitMeasuresMx(bivHomFit)
bivHomFitSim <- sim(10, bivHomFit, n = list(100, 100))
summary(bivHomFitSim)
