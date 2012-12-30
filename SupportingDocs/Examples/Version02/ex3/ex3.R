library(simsem)

factor.loading <- matrix(NA, 4, 2)
factor.loading[,1] <- 1
factor.loading[,2] <- 0:3
LY <- simMatrix(factor.loading)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 1)
AL <- simVector(factor.mean, factor.mean.starting)

factor.var <- rep(NA, 2)
factor.var.starting <- c(1, 0.25)
VPS <- simVector(factor.var, factor.var.starting)

factor.cor <- matrix(NA, 2, 2)
diag(factor.cor) <- 1
RPS <- symMatrix(factor.cor, 0.5)

VTE <- simVector(rep(NA, 4), 1.2)

RTE <- symMatrix(diag(4))

TY <- simVector(rep(0, 4))

LCA.Model <- simSetCFA(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY)

SimModel <- simModel(LCA.Model)

### Get the number sign out if you wish to run the model without misspecification
# Data.True <- simData(LCA.Model, 300)
# Output <- simResult(1000, Data.True, SimModel)
# getCutoff(Output, 0.05)
# plotCutoff(Output, 0.05)
# summaryParam(Output)

u1 <- simUnif(-0.1, 0.1)

loading.trivial <- matrix(0, 4, 2)
loading.trivial[2:3, 2] <- NA
loading.mis <- simMatrix(loading.trivial, "u1")

LCA.Mis <- simMisspecCFA(LY = loading.mis)

Data.Mis <- simData(LCA.Model, 300, misspec = LCA.Mis)

Output.Mis <- simResult(1000, Data.Mis, SimModel)
getCutoff(Output.Mis, 0.05)
plotCutoff(Output.Mis, 0.05)
summaryParam(Output.Mis)
