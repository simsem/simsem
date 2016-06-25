library(simsem)

loading <- matrix(0, 48, 4)
loading[1:12, 1] <- NA
loading[13:24, 2] <- NA
loading[25:36, 3] <- NA
loading[37:48, 4] <- NA
loading.mis <- matrix("runif(1, -0.2, 0.2)", 48, 4)
loading.mis[is.na(loading)] <- 0
LY <- bind(loading, "runif(1, 0.4, 0.9)", misspec=loading.mis)

faccor <- matrix(NA, 4, 4)
diag(faccor) <- 1
RPS <- binds(faccor, "runif(1, 0.1, 0.6)")

RTE <- binds(diag(48))

CFA.model <- model(LY=LY, RPS=RPS, RTE=RTE, modelType="CFA")

setx <- c(1:3, 13:15, 25:27, 37:39)
set1 <- setx + 3
set2 <- set1 + 3
set3 <- set2 + 3
itemGroups <- list(setx, set1, set2, set3)

missModel <- miss(nforms=3, itemGroups=itemGroups, m=5)

Output <- sim(20, n=1000, CFA.model, miss=missModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
