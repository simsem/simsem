library(simsem)
library(semTools)
library(OpenMx)

###################### Fitting univSatFit2

set.seed(100)
x <- rnorm (1000, 0, 1)
testData <- as.matrix(x)
selVars <- c("X")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
colMeans(testData)
var(testData)

univSatModel2 <- mxModel("univSat2",
    manifestVars= selVars,
    mxPath(
        from=c("X"), 
        arrows=2, 
        free=T, 
        values=1, 
        lbound=.01, 
        labels="vX"
    ),
    mxPath(
    	from=c("one"),
    	to=c("X"),
    	free=T,
    	values=0,
    	labels="mX"
    ),
    mxData(
        observed=testData, 
        type="raw", 
    ),
    type="RAM"
)

univSatFit2 <- mxRun(univSatModel2)
fitMeasuresMx(univSatFit2)
univSatFit2Sim <- sim(10, univSatFit2, n = 200)
summary(univSatFit2Sim)
