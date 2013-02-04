library(simsem)
library(semTools)
library(OpenMx)

########################### Fitting uniRegFit

data(myRegDataRaw)
SimpleDataRaw <- myRegDataRaw[,c("x","y")]

uniRegModel <- mxModel("Simple Regression Matrix Specification", 
    mxData(
        observed=SimpleDataRaw,
        type="raw"
    ),
    mxMatrix(
        type="Full", 
        nrow=2, 
        ncol=2,
        free=c(F, F,
               T, F),
        values=c(0, 0,
                 1, 0),
        labels=c(NA,     NA,
                "beta1", NA),
        byrow=TRUE,
        name="A"
    ),
    mxMatrix(
        type="Symm", 
        nrow=2, 
        ncol=2, 
        values=c(1, 0,
                 0, 1),
        free=c(T, F,
               F, T),
        labels=c("varx", NA,
                  NA,    "residual"),
        byrow=TRUE,
        name="S"
    ),
    mxMatrix(
        type="Iden",  
        nrow=2, 
        ncol=2,
        name="F"
    ),
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=2,
        free=c(T, T),
        values=c(0, 0),
        labels=c("meanx", "beta0"),
        name="M"),
    mxRAMObjective("A", "S", "F", "M",
		dimnames=c("x","y"))
)
  
uniRegFit <- mxRun(uniRegModel)
fitMeasuresMx(uniRegFit)
uniRegFitSim <- sim(10, uniRegFit, n = 200)
summary(uniRegFitSim)
