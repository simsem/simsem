library(simsem)
library(semTools)
library(OpenMx)

############## Fitting multiRegFit

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

multiRegModel <- mxModel("Multiple Regression Path Specification", 
      type="RAM",
      mxData(
          observed=MultipleDataCov, 
          type="cov",
          numObs=100,
          means=MultipleDataMeans
      ),
      manifestVars=c("x", "y", "z"),
      mxPath(
          from=c("x", "y", "z"), 
          arrows=2,
          free=TRUE, 
          values = c(1, 1, 1),
          labels=c("varx", "residual", "varz")
      ),
      # variance paths
      # -------------------------------------
      mxPath(
          from="x",
          to="z",
          arrows=2,
          free=TRUE,
          values=0.5,
          labels="covxz"
      ), 
      # covariance of x and z
      # -------------------------------------
      mxPath(
          from=c("x","z"),
          to="y",
          arrows=1,
          free=TRUE,
          values=1,
          labels=c("betax","betaz")
      ), 
      # regression weights
      # -------------------------------------
      mxPath(
          from="one",
          to=c("x", "y", "z"),
          arrows=1,
          free=TRUE,
          values=c(1, 1),
          labels=c("meanx", "beta0", "meanz")
      )
      # means and intercepts
      # -------------------------------------
) 
 
multiRegFit <- mxRun(multiRegModel)
fitMeasuresMx(multiRegFit)
multiRegFitSim <- sim(10, multiRegFit, n = 200)
summary(multiRegFitSim)
