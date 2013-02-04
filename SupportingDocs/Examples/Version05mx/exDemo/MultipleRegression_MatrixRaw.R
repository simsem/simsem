library(simsem)
library(semTools)
library(OpenMx)

########################## Fitting multiRegFit

data(myRegDataRaw)

MultipleDataRaw<-myRegDataRaw[,c("x","y","z")]

multiRegModel <- mxModel("Multiple Regression Matrix Specification", 
    mxData(
    	observed=MultipleDataRaw,
    	type="raw"
   	),
    mxMatrix(
    	type="Full", 
    	nrow=3, 
    	ncol=3,
        values=c(0,0,0,
                 1,0,1,
                 0,0,0),
        free=c(F, F, F,
               T, F, T,
               F, F, F),
        labels=c(NA,     NA, NA,
                "betax", NA,"betaz",
                 NA,     NA, NA),
        byrow=TRUE,
        name = "A"
    ),
    mxMatrix(
    	type="Symm", 
    	nrow=3, 
    	ncol=3, 
        values=c(1, 0, .5,
                 0, 1, 0,
                .5, 0, 1),
        free=c(T, F, T,
               F, T, F,
               T, F, T),
        labels=c("varx",  NA,         "covxz",
                  NA,    "residual",   NA,
                 "covxz", NA,         "varz"),
        byrow=TRUE,
        name="S"
    ),
    mxMatrix(
    	type="Iden",
    	nrow=3, 
    	ncol=3,
        name="F"
    ),
    mxMatrix(
    	type="Full", 
    	nrow=1, 
    	ncol=3,
        values=c(0,0,0),
        free=c(T,T,T),
        labels=c("meanx","beta0","meanz"),
        name="M"
    ),
    mxRAMObjective("A","S","F","M",
       dimnames=c("x","y","z"))
)

multiRegFit <- mxRun(multiRegModel)
fitMeasuresMx(multiRegFit)
multiRegFitSim <- sim(10, multiRegFit, n = 200)
summary(multiRegFitSim)
