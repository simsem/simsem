require(OpenMx)
require(MASS)

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c('X','Y')
dimnames(testData) <- list(NULL, selVars)
summary(testData)
cov(testData)

bivCorModel <- mxModel("bivCor",
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=2, 
        free=TRUE, 
        values=c(0,0), 
        name="expMean"
    ), 
    mxMatrix(
        type="Lower", 
        nrow=2, 
        ncol=2, 
        free=TRUE,
        values=.5, 
        name="Chol"
    ), 
    mxAlgebra(
        expression=Chol %*% t(Chol), 
        name="expCov", 
    ), 
    mxData(
        observed=testData, 
        type="raw"
    ), 
    mxFIMLObjective(
        covariance="expCov", 
        means="expMean",
        dimnames=selVars)
    )


bivCorFit <- mxRun(bivCorModel)
EM <- mxEval(expMean, bivCorFit)
EC <- mxEval(expCov, bivCorFit)
LL <- mxEval(objective, bivCorFit)

bivCorModelSub <-mxModel(bivCorModel,
    mxMatrix(
        type="Diag", 
        nrow=2, 
        ncol=2, 
        free=TRUE, 
		values=c(1,1),
        name="Chol"
    )
)

bivCorFitSub <- mxRun(bivCorModelSub)

dat1 <- generateOpenMx(bivCorModel, n=200)
dat2 <- generateOpenMx(bivCorFit, n=200)
dat3 <- generateOpenMx(bivCorModelSub, n=200)
dat4 <- generateOpenMx(bivCorFitSub, n=200)

out1 <- analyzeOpenMx(bivCorModel, dat1)
out2 <- analyzeOpenMx(bivCorModel, dat2)
out3 <- analyzeOpenMx(bivCorModelSub, dat3)
out4 <- analyzeOpenMx(bivCorModelSub, dat4)

outSat1 <- analyzeSaturateOpenMx(dat1)
outSat2 <- analyzeSaturateOpenMx(dat2)
outSat3 <- analyzeSaturateOpenMx(dat3)
outSat4 <- analyzeSaturateOpenMx(dat4)

outNull1 <- analyzeNullOpenMx(dat1)
outNull2 <- analyzeNullOpenMx(dat2)
outNull3 <- analyzeNullOpenMx(dat3)
outNull4 <- analyzeNullOpenMx(dat4)


