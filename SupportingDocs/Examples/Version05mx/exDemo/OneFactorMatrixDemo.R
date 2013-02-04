library(simsem)
library(semTools)
library(OpenMx)

######################### Fitting factorFit

data(demoOneFactor)

manifestVars <- names(demoOneFactor)

factorModel <- mxModel("One Factor",
    mxMatrix(type="Full", nrow=5, ncol=1, values=0.7, free=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=1, ncol=1, values=1, free=FALSE, name="L"),
    mxMatrix(type="Diag", nrow=5, ncol=5, values=1, free=TRUE, name="U"),
    mxMatrix(type="Full", nrow=1, ncol=5, values=1, free=TRUE, name="M"),
    mxAlgebra(expression=A %*% L %*% t(A) + U, name="R"),
    mxMLObjective(covariance="R", means="M", dimnames=manifestVars),
    mxData(observed=cov(demoOneFactor), means=colMeans(demoOneFactor), type="cov", numObs=500)
)

factorFit <- mxRun(factorModel)
fitMeasuresMx(factorFit)
factorFitSim <- sim(10, factorFit, n = 200)
summary(factorFitSim)
