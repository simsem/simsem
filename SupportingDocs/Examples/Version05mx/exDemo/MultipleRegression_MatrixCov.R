library(simsem)
library(semTools)
library(OpenMx)

########################## Fitting multiRegFit

myRegDataCov <- matrix(
    c(0.808,-0.110, 0.089, 0.361,
     -0.110, 1.116, 0.539, 0.289,
      0.089, 0.539, 0.933, 0.312,
      0.361, 0.289, 0.312, 0.836),
    nrow=4,
    dimnames=list(
      c("w","x","y","z"),
      c("w","x","y","z"))
)

 
myRegDataMeans <- c(2.582, 0.054, 2.574, 4.061)
names(myRegDataMeans) <- c("w","x","y","z") 

MultipleDataCov <- myRegDataCov[c("x","y","z"),c("x","y","z")]	
MultipleDataMeans <- myRegDataMeans[c(2,3,4)]

multiRegModel<-mxModel("Multiple Regression Matrix Specification", 
      mxData(
      		observed=MultipleDataCov, 
      		type="cov", 
      		numObs=100, 
      		mean=MultipleDataMeans
      ),
      mxMatrix("Full", nrow=3, ncol=3,
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
            name="A"
      ),
      mxMatrix("Symm", nrow=3, ncol=3, 
            values=c(1, 0, .5,
                     0, 1, 0,
                    .5, 0, 1),
            free=c(T, F, T,
                   F, T, F,
                   T, F, T),
            labels=c("varx", NA,         "covxz",
                     NA,     "residual",   NA,
                     "covxz",   NA,         "varz"),
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
      mxRAMObjective("A","S","F","M",dimnames=c('x','y','z'))
)
  
multiRegFit <- mxRun(multiRegModel)
fitMeasuresMx(multiRegFit)
multiRegFitSim <- sim(10, multiRegFit, n = 200)
summary(multiRegFitSim)
