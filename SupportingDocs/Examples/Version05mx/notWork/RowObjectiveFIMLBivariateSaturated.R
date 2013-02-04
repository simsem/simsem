library(simsem)
library(OpenMx)
library(MASS)

############################### Fitting bivCorFit

set.seed(200)
rs <- .5
xy <- mvrnorm (1000, c(0,0), matrix(c(1, rs, rs, 1), nrow=2, ncol=2))
testData <- as.data.frame(xy)
testVars <- c('X','Y')
names(testData) <- testVars
summary(testData)
cov(testData)

bivCorModelSpec <- mxModel(
    name="FIML Saturated Bivariate",
    mxData(
        observed=testData, 
        type="raw",
    ),
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=2, 
        free=TRUE, 
        values=c(0,0), 
        name="expMean"
    ), 
    mxMatrix(
        type="Symm",
        nrow=2, 
        ncol=2,
        values=c(.21, .2, .2, .21),
        free=TRUE,
        name='expCov'
    )
)

bivCorFiltering <- mxModel(
    model=bivCorModelSpec,
    mxAlgebra(
        expression=omxSelectRowsAndCols(expCov, existenceVector),
        name="filteredExpCov",
    ),
    mxAlgebra(
        expression=omxSelectCols(expMean, existenceVector),
        name="filteredExpMean",
    ),
    mxAlgebra(
        expression=sum(existenceVector),
        name="numVar_i")
)

bivCorCalc <- mxModel(
    model=bivCorFiltering,
    mxAlgebra(
        expression = log(2*pi),
        name = "log2pi"
    ),
    mxAlgebra(
        expression=log2pi %*% numVar_i + log(det(filteredExpCov)),
        name ="firstHalfCalc",
    ),
    mxAlgebra(
        expression=(filteredDataRow - filteredExpMean) %&% solve(filteredExpCov),
        name = "secondHalfCalc",
    )
)

bivCorRowObj <- mxModel(
    model=bivCorCalc,
    mxAlgebra(
        expression=(firstHalfCalc + secondHalfCalc),
        name="rowAlgebra",
    ),
    mxAlgebra(
        expression=sum(rowResults),
        name = "reduceAlgebra",
    ),
    mxRowObjective(
        rowAlgebra='rowAlgebra',
        reduceAlgebra='reduceAlgebra',
        dimnames=c('X','Y'),
    )
)

bivCorTotal <- bivCorRowObj

bivCorFit <- mxRun(bivCorTotal)
bivCorFitSim <- sim(10, bivCorFit, n = 200)
summary(bivCorFitSim)
