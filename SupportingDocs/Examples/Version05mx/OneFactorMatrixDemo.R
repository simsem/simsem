#
#   Copyright 2007-2012 The OpenMx Project
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        http://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


# -----------------------------------------------------------------------------
# Program: OneFactorMatrixDemo.R  
# Author: Steve Boker
# Date: 2009.07.30 
#
# ModelType: Factor
# DataType: Continuous
# Field: None
#
# Purpose:
#      OpenMx one factor matrix model demo for front page of website
# 
# RevisionHistory:
#      Hermine Maes -- 2010.02.22 updated & reformatted
#      Ross Gore -- 2011.06.06 added Model, Data & Field metadata
# -----------------------------------------------------------------------------

require(OpenMx)
# Load Library
# -----------------------------------------------------------------------------

data(demoOneFactor)
# Prepare Data
# -----------------------------------------------------------------------------

manifestVars <- names(demoOneFactor)
# Prepare Manifests Data
# -----------------------------------------------------------------------------

factorModel <- mxModel("One Factor",
    mxMatrix(type="Full", nrow=5, ncol=1, values=0.7, free=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=1, ncol=1, values=1, free=FALSE, name="L"),
    mxMatrix(type="Diag", nrow=5, ncol=5, values=1, free=TRUE, name="U"),
    mxMatrix(type="Full", nrow=1, ncol=5, values=1, free=TRUE, name="M"),
    mxAlgebra(expression=A %*% L %*% t(A) + U, name="R"),
    mxMLObjective(covariance="R", means="M", dimnames=manifestVars),
    mxData(observed=cov(demoOneFactor), means=colMeans(demoOneFactor), type="cov", numObs=500)
)

# Create an MxModel object
# -----------------------------------------------------------------------------

factorFit <- mxRun(factorModel)
# Fit the model to the observed covariances with mxRun
# -----------------------------------------------------------------------------

summary(factorFit)
# Print a summary of the results
# -----------------------------------------------------------------------------

dat1 <- generateOpenMx(factorModel, n=200)
dat2 <- generateOpenMx(factorFit, n=200)

out1 <- analyzeOpenMx(factorModel, dat1)
out2 <- analyzeOpenMx(factorFit, dat2)

outSat1 <- analyzeSaturateOpenMx(dat1)
outSat2 <- analyzeSaturateOpenMx(dat2)

outNull1 <- analyzeNullOpenMx(dat1)
outNull2 <- analyzeNullOpenMx(dat2)

round(fitMeasuresOpenMx(out2), digits=3)

library(lavaan)
script <- "
f1 =~ x1 + x2 + x3 + x4 +x5
"
fitMeasures(cfa(script, data=dat2, std.lv=TRUE, meanstructure=TRUE))
