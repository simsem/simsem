library(simsem)

# a) Noninvariance

loading1.a <- matrix(NA, 6, 1)
LY1.a <- bind(loading1.a, 0.7)
loading2.a <- matrix(0, 6, 2)
loading2.a[1:3, 1] <- NA
loading2.a[4:6, 2] <- NA
LY2.a <- bind(loading2.a, 0.7)

RPS1.a <- binds(as.matrix(1))

latentcor2.a <- matrix(NA, 2, 2)
diag(latentcor2.a) <- 1
RPS2.a <- binds(latentcor2.a, 0.5)

RTE.a <- binds(diag(6))

VTE.a <- bind(rep(NA, 6), 0.51)

noninvariance <- model(LY = list(LY1.a, LY2.a), RPS = list(RPS1.a, RPS2.a), RTE = list(RTE.a, RTE.a), VTE=list(VTE.a, VTE.a), ngroups=2, modelType = "CFA")

# dat <- generate(noninvariance, 200)
# out <- analyze(noninvariance, dat)

Output.a <- sim(1000, noninvariance, n=200) 
getCutoff(Output.a, 0.05)
plotCutoff(Output.a, 0.05)
summary(Output.a)

# b) Configural Invariance
loading.b <- matrix(0, 6, 2)
loading.b[1:3, 1] <- NA
loading.b[4:6, 2] <- NA
LY.b <- bind(loading.b, 0.7)

latentcor.b <- matrix(NA, 2, 2)
diag(latentcor.b) <- 1
RPS.b <- binds(latentcor.b, 0.5)

RTE.b <- binds(diag(6))

VTE.b <- bind(rep(NA, 6), 0.51)

configural <- model(LY = LY.b, RPS = RPS.b, RTE = RTE.b, VTE=VTE.b, ngroups=2, modelType = "CFA")

# dat <- generate(configural, 200)
# out <- analyze(configural, dat)

Output.b <- sim(1000, configural, n=200) 
getCutoff(Output.b, 0.05)
plotCutoff(Output.b, 0.05)
summary(Output.b)

# c) Weak Invariance
loading.c <- matrix(0, 6, 2)
loading.c[1:3, 1] <- c("load1", "load2", "load3")
loading.c[4:6, 2] <- c("load4", "load5", "load6")
LY.c <- bind(loading.c, 0.7)

latentcor.c <- matrix(NA, 2, 2)
diag(latentcor.c) <- 1
RPS.c <- binds(latentcor.c, 0.5)

RTE.c <- binds(diag(6))

VTE.c <- bind(rep(NA, 6), 0.51)

VPS1.c <- bind(rep(1, 2))
VPS2.c <- bind(rep(NA, 2), c(1.1, 1.2))

weak <- model(LY = LY.c, RPS = RPS.c, VPS=list(VPS1.c, VPS2.c), RTE = RTE.c, VTE=VTE.c, ngroups=2, modelType = "CFA")

# dat <- generate(weak, 200)
# out <- analyze(weak, dat)

Output.c <- sim(1000, weak, n=200) 
getCutoff(Output.c, 0.05)
plotCutoff(Output.c, 0.05)
summary(Output.c)

# d) Strong Invariance
loading.d <- matrix(0, 6, 2)
loading.d[1:3, 1] <- paste0("load", 1:3)
loading.d[4:6, 2] <- paste0("load", 4:6)
LY.d <- bind(loading.d, 0.7)

latentcor.d <- matrix(NA, 2, 2)
diag(latentcor.d) <- 1
RPS.d <- binds(latentcor.d, 0.5)

RTE.d <- binds(diag(6))

VTE.d <- bind(rep(NA, 6), 0.51)

TY.d <- bind(paste0("int", 1:6), 0)

VPS1.d <- bind(rep(1, 2))
VPS2.d <- bind(rep(NA, 2), c(1.1, 1.2))

AL1.d <- bind(rep(0, 2))
AL2.d <- bind(rep(NA, 2), c(-0.5, 0.2))

strong <- model(LY = LY.d, RPS = RPS.d, VPS=list(VPS1.d, VPS2.d), RTE = RTE.d, VTE=VTE.d, TY=TY.d, AL=list(AL1.d, AL2.d), ngroups=2, modelType = "CFA")

# dat <- generate(strong,200)
# out <- analyze(strong,dat)

Output.d <- sim(1000, strong, n=200)
getCutoff(Output.d, 0.05)
plotCutoff(Output.d, 0.05)
summary(Output.d)
