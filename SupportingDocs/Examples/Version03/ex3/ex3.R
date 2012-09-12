library(simsem)

factor.loading <- matrix(NA, 4, 2)
factor.loading[,1] <- 1
factor.loading[,2] <- 0:3
LY <- bind(factor.loading)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- bind(factor.mean, factor.mean.starting)

factor.var <- rep(NA, 2)
factor.var.starting <- c(1, 0.25)
VPS <- bind(factor.var, factor.var.starting)

factor.cor <- matrix(NA, 2, 2)
diag(factor.cor) <- 1
RPS <- binds(factor.cor, 0.5)

VTE <- bind(rep(NA, 4), 1.2)

RTE <- binds(diag(4))

TY <- bind(rep(0, 4))

LCA.Model <- model(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY, modelType="CFA")

### Get the number sign out if you wish to run the model without misspecification
# Output.True <- sim(1000, n=300, LCA.Model)
# getCutoff(Output, 0.05)
# plotCutoff(Output, 0.05)
# summary(Output)

loading.trivial <- matrix(0, 4, 2)
loading.trivial[2:3, 2] <- "runif(1,-0.1,0.1)"
LY.mis <- bind(factor.loading, misspec=loading.trivial)

LCA.Mis <- model(LY=LY.mis, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY, modelType="CFA")

Output.Mis <- sim(1000, n=300, model=LCA.Mis)
getCutoff(Output.Mis, 0.05)
plotCutoff(Output.Mis, 0.05)
summary(Output.Mis)
