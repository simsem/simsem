library(simsem)
library(semTools)
library(OpenMx)

######################## Fitting multivariateRegFit

data(myRegDataRaw)

multivariateRegModel <- mxModel("Multiple Regression Matrix Specification", 
    mxData(
    	observed=myRegDataRaw,
    	type="raw"
    ),
    mxMatrix(
    	type="Full", 
    	nrow=4, 
    	ncol=4,
        values=c(0,1,0,1,
                 0,0,0,0,
                 0,1,0,1,
                 0,0,0,0),
        free=c(F, T, F, T,
               F, F, F, F,
               F, T, F, T,
               F, F, F, F),
        labels=c(NA, "betawx", NA, "betawz",
                 NA,  NA,     NA,  NA, 
                 NA, "betayx", NA, "betayz",
                 NA,  NA,     NA,  NA),
        byrow=TRUE,
        name="A"
    ),
    mxMatrix(
    	type="Symm", 
    	nrow=4, 
    	ncol=4, 
        values=c(1,  0, 0,  0,
                 0,  1, 0, .5,
                 0,  0, 1,  0,
                 0, .5, 0,  1),
        free=c(T, F, F, F,
               F, T, F, T,
               F, F, T, F,
               F, T, F, T),
        labels=c("residualw",  NA,     NA,         NA,
                  NA,         "varx",  NA,        "covxz",
                  NA,          NA,    "residualy", NA,
                  NA,         "covxz", NA,        "varz"),
        byrow=TRUE,
        name="S"
    ),
    mxMatrix(
    	type="Iden",
    	nrow=4, 
    	ncol=4,
        name="F"
    ),
    mxMatrix(
    	type="Full", 
    	nrow=1, 
    	ncol=4,
        values=c(0,0,0,0),
        free=c(T,T,T,T),
        labels=c("betaw","meanx","betay","meanz"),
        name="M"
    ),
    mxRAMObjective("A","S","F","M",
		dimnames=c("w", "x", "y", "z"))
)
 
multivariateRegFit <- mxRun(multivariateRegModel)
fitMeasuresMx(multivariateRegFit)
multivariateRegFitSim <- sim(10, multivariateRegFit, n = 200)
summary(multivariateRegFitSim)
