library(simsem)
library(semTools)
library(OpenMx)

######################### Fitting uniRegFit

data(myRegDataRaw)

names(myRegDataRaw)

myRegDataRaw <- myRegDataRaw[,c("x","y")]

uniRegModel <- mxModel("Simple Regression Path Specification", 
      type="RAM",
      mxData(
      		observed=myRegDataRaw, 
      		type="raw"
      ),
      manifestVars=c("x","y"),
      mxPath(from=c("x","y"), 
            arrows=2,
            free=TRUE, 
            values = c(1,1),
            labels=c("varx","residual")
      ), 
	  # variances
      # -------------------------------------
      mxPath(from="x",
            to="y",
            arrows=1,
            free=TRUE,
            values=1,
            label="beta1"
      ), 
      # regression weight
      # -------------------------------------
      mxPath(from="one",
            to=c("x","y"),
            arrows=1,
            free=TRUE,
            values=c(1,1),
            labels=c("meanx","beta0")
      ) 
      # means
      # -------------------------------------
) 
   
uniRegFit <- mxRun(uniRegModel, suppressWarnings=TRUE)
fitMeasuresMx(uniRegFit)
uniRegFitSim <- sim(10, uniRegFit, n = 200)
summary(uniRegFitSim)
