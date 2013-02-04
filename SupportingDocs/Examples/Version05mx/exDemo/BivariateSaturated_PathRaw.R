library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

################### Fitting bivSatFit2

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c("X","Y")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
cov(testData)
# Simulate Data
# -----------------------------------------------------------------------------

bivSatModel2 <- mxModel("bivSat2",
    manifestVars= selVars,
    mxPath(
        from=c("X", "Y"), 
        arrows=2, 
        free=T, 
        values=1, 
        lbound=.01, 
        labels=c("varX","varY")
    ),
    mxPath(
        from="X", 
        to="Y", 
        arrows=2, 
        free=T, 
        values=.2, 
        lbound=.01, 
        labels="covXY"
    ),
    mxPath(
    	from="one",
        to=c("X", "Y"),
        arrows=1,
        free=T),
    mxData(
        observed=testData, 
        type="raw", 
    ),
    type="RAM"
)

bivSatFit2 <- mxRun(bivSatModel2)
fitMeasuresMx(bivSatFit2)
bivSatFit2Sim <- sim(10, bivSatFit2, n = 200)
summary(bivSatFit2Sim)
