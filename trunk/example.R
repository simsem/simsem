source("simDist.R")
source("simMatrix.R")
source("simMatrixSet.R")
source("simConstraint.R")
source("matrixSet.R")
source("freeParamSet.R")
source("reducedMatrixSet.R")
source("misspecifiedSet.R")
source("simData.R")
source("simModel.R")
source("simResult.R")
source("subMatrixSet.R")
source("simConstraint.R")

factor.loading <- matrix(NA, 4, 2)
factor.loading[,1] <- 1
factor.loading[,2] <- 0:3
LY <- matrix.object(factor.loading)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5,2)
AL <- vector.object(factor.mean, factor.mean.starting)

factor.var <- rep(NA,2)
factor.var.starting <- c(1, 0.25)
VPS <- vector.object(factor.var, factor.var.starting)

factor.cor <- matrix(NA,2,2)
diag(factor.cor) <- 1
PS <- sym.matrix.object(factor.cor, 0.5)

VTE <- vector.object(rep(NA,4),1.2)

TE <- sym.matrix.object(diag(4))

TY <- vector.object(rep(0,4))

LCA.Model <- matrix.CFA.object(LY=LY, PS=PS, VPS=VPS, AL=AL,
                               VTE=VTE, TE=TE, TY=TY)

Data.True <- data.object(300, LCA.Model)
SimModel <- model.object(LCA.Model)
Output <- result.object(Data.True,SimModel, 1000)

find.cutoff(Output, 0.95)
visualize(Output, 0.95)
