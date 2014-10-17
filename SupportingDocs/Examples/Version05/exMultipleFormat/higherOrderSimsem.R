library(simsem)

load1 <- matrix(0, 9, 4)
load1[1:3, 1] <- c(1, NA, NA)
load1[4:6, 2] <- c(1, NA, NA)
load1[7:9, 3] <- c(1, NA, NA)
LY <- bind(load1, 1)

RTE <- binds(diag(9))
VTE <- bind(rep(NA, 9), 0.3)
RPS <- binds(diag(4))
VPS <- bind(rep(NA, 4), 0.7)
load2 <- matrix(0, 4, 4)
load2[1:3, 4] <- c(1, NA, NA)
BE <- bind(load2, 1)
TY <- bind(rep(NA, 9), 0)
AL <- bind(rep(0, 4))

SEM.model <- model(BE=BE, LY=LY, RPS=RPS, VPS=VPS, RTE=RTE, VTE=VTE, TY=TY, AL=AL, modelType="SEM")

dat <- generate(SEM.model, n = 300)
out <- analyze(SEM.model, dat) 

Output <- sim(1000, n = 300, SEM.model) 
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

