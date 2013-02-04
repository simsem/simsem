library(simsem)
library(semTools)
library(OpenMx)

########################### Fitting gmmFit

data(myGrowthMixtureData)

class1 <- mxModel("Class1", 
    mxMatrix(
        type="Full",
        nrow=7, 
        ncol=7,
        free=F,
        values=c(0,0,0,0,0,1,0,
                 0,0,0,0,0,1,1,
                 0,0,0,0,0,1,2,
                 0,0,0,0,0,1,3,
                 0,0,0,0,0,1,4,
                 0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0),
        byrow=TRUE,
        name="A"
    ),
    mxMatrix(
        type="Symm",
        nrow=7,
        ncol=7,
        free=c(T, F, F, F, F, F, F,
               F, T, F, F, F, F, F,
               F, F, T, F, F, F, F,
               F, F, F, T, F, F, F,
               F, F, F, F, T, F, F,
               F, F, F, F, F, T, T,
               F, F, F, F, F, T, T),
        values=c(1,0,0,0,0,  0,  0,
                 0,1,0,0,0,  0,  0,
                 0,0,1,0,0,  0,  0,
                 0,0,0,1,0,  0,  0,
                 0,0,0,0,1,  0,  0,
                 0,0,0,0,0,  1,0.4,
                 0,0,0,0,0,0.4,  1),
        labels=c("residual", NA, NA, NA, NA, NA, NA,
                 NA, "residual", NA, NA, NA, NA, NA,
                 NA, NA, "residual", NA, NA, NA, NA,
                 NA, NA, NA, "residual", NA, NA, NA,
                 NA, NA, NA, NA, "residual", NA, NA,
                 NA, NA, NA, NA, NA, "vari1", "cov1",
                 NA, NA, NA, NA, NA, "cov1", "vars1"),
        byrow= TRUE,
        name="S"
    ),
    mxMatrix(
        type="Full",
        nrow=5,
        ncol=7,
        free=F,
        values=c(1,0,0,0,0,0,0,
                 0,1,0,0,0,0,0,
                 0,0,1,0,0,0,0,
                 0,0,0,1,0,0,0,
                 0,0,0,0,1,0,0),
        byrow=T,
        name="F"
    ),
    mxMatrix(
    	type="Full",
    	nrow=1, 
    	ncol=7,
        values=c(0,0,0,0,0,0,-1),
        free=c(F,F,F,F,F,T,T),
        labels=c(NA,NA,NA,NA,NA,"meani1","means1"),
        name="M"
    ),
    mxRAMObjective("A","S","F","M", vector=TRUE, 
		dimnames = c(names(myGrowthMixtureData), "intercept", "slope"))
) 

class2 <- mxModel(class1,
	mxMatrix(
        type="Symm",
        nrow=7,
        ncol=7,
        free=c(T, F, F, F, F, F, F,
               F, T, F, F, F, F, F,
               F, F, T, F, F, F, F,
               F, F, F, T, F, F, F,
               F, F, F, F, T, F, F,
               F, F, F, F, F, T, T,
               F, F, F, F, F, T, T),
        values=c(1,0,0,0,0,  0,  0,
                 0,1,0,0,0,  0,  0,
                 0,0,1,0,0,  0,  0,
                 0,0,0,1,0,  0,  0,
                 0,0,0,0,1,  0,  0,
                 0,0,0,0,0,  1,0.5,
                 0,0,0,0,0,0.5,  1),
        labels=c("residual", NA, NA, NA, NA, NA, NA,
                 NA, "residual", NA, NA, NA, NA, NA,
                 NA, NA, "residual", NA, NA, NA, NA,
                 NA, NA, NA, "residual", NA, NA, NA,
                 NA, NA, NA, NA, "residual", NA, NA,
                 NA, NA, NA, NA, NA, "vari2", "cov2",
                 NA, NA, NA, NA, NA, "cov2", "vars2"),
        byrow= TRUE,
        name="S"
    ),
    mxMatrix(
    	type="Full",
    	nrow=1, 
    	ncol=7,
        values=c(0,0,0,0,0,5,1),
        free=c(F,F,F,F,F,T,T),
        labels=c(NA,NA,NA,NA,NA,"meani2","means2"),
        name="M"
    ),
	name="Class2"
) 

classP <- mxMatrix("Full", 2, 1, free=c(TRUE, FALSE), 
          values=1, lbound=0.001, 
          labels = c("p1", "p2"), name="Props")

classS <- mxAlgebra(Props%x%(1/sum(Props)), name="classProbs")


algObj <- mxAlgebra(-2*sum(
          log(classProbs[1,1]%x%Class1.objective + classProbs[2,1]%x%Class2.objective)), 
          name="mixtureObj")

obj <- mxAlgebraObjective("mixtureObj")
      
gmm <- mxModel("Growth Mixture Model",
	mxData(
    	observed=myGrowthMixtureData,
        type="raw"
    ),
    class1, class2,
    classP, classS,
    algObj, obj
	)      

gmmFit <- mxRun(gmm, suppressWarnings=TRUE)
fitMeasuresMx(gmmFit)
gmmFitSim <- sim(10, gmmFit, n = list(0.4 * 500, 0.6 * 500), mxMixture = TRUE)
summary(gmmFitSim)
