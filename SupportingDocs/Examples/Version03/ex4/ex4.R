library(simsem)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1,0.5,0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1,-0.1,0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1,0.3,0.1)")

Path.Model <- model(RPS = RPS, BE = BE, modelType="Path")

Output <- sim(1000, n=500, Path.Model)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
