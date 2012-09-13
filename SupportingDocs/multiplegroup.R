library(simsem)

# Configural Invariance
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

configural <- model(LY = list(LY, LY), RPS = list(RPS, RPS), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")

# Data generation from strong-invariance model (with 200 subjects for each group)
dat <- generate(configural, 200) 

# Analyze data
out <- analyze(configural, dat)

# Monte Carlo simulation
Output <- sim(20, configural, n=200) 
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# Weak Invariance
loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- c("load1", "load2", "load3")
loading.in[4:6, 2] <- c("load4", "load5", "load6")
LY.in <- bind(loading.in, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

VPS1 <- bind(rep(1, 2))
VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))

weak <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, ngroups=2, modelType = "CFA")

dat <- generate(weak, 200)
out <- analyze(weak, dat)

Output <- sim(20, weak, n=200) 
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# Strong Invariance
loading.in <- matrix(0, 6, 2)
loading.in[1:3, 1] <- paste0("load", 1:3)
loading.in[4:6, 2] <- paste0("load", 4:6)
LY.in <- bind(loading.in, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

TY.in <- bind(paste0("int", 1:6), 0)

VPS1 <- bind(rep(1, 2))
VPS2 <- bind(rep(NA, 2), c(1.1, 1.2))

AL1 <- bind(rep(0, 2))
AL2 <- bind(rep(NA, 2), c(-0.5, 0.2))

strong <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, TY=TY.in, AL=list(AL1, AL2), ngroups=2, modelType = "CFA")

dat <- generate(strong,200) 
out <- analyze(strong,dat)

Output <- sim(20, strong, n=200)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)
