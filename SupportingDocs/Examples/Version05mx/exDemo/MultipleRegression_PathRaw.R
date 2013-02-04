library(simsem)
library(semTools)
library(OpenMx)

######################### Fitting multiRegFit

data(myRegDataRaw)

myRegDataRaw<-myRegDataRaw[,c("x","y","z")]

multiRegModel <- mxModel("Multiple Regression Path Specification", 
      type="RAM",
      mxData(
          observed=myRegDataRaw, 
          type="raw"
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
