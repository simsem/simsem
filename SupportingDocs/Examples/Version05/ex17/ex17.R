library(simsem)




path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- "con1"
path[5, 2] <- path[8, 5] <- "con2"
path[6, 3] <- path[9, 6] <- "con3"
path[5, 1] <- path[8, 4] <- "con4"
path[6, 2] <- path[9, 5] <- "con5"
pathVal <- matrix(0, 9, 9)
pathVal[4, 1] <- pathVal[7, 4] <- "runif(1, 0.5, 0.7)"
pathVal[5, 2] <- pathVal[8, 5] <- "runif(1, 0.5, 0.7)"
pathVal[6, 3] <- pathVal[9, 6] <- "runif(1, 0.5, 0.7)"
pathVal[5, 1] <- pathVal[8, 4] <- "runif(1, 0.3, 0.5)"
pathVal[6, 2] <- pathVal[9, 5] <- "runif(1, 0.3, 0.5)"
BE <- bind(path, pathVal)

facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- NA
facCor[1, 3] <- facCor[3, 1] <- NA
facCor[2, 3] <- facCor[3, 2] <- NA
RPS <- binds(facCor, "runif(1, 0.3, 0.5)")

loading <- matrix(0, 27, 9)
loading[1:3, 1] <- c("con6", "con7", "con8")
loading[4:6, 2] <- c("con9", "con10", "con11")
loading[7:9, 3] <- c("con12", "con13", "con14")
loading[10:12, 4] <- c("con6", "con7", "con8")
loading[13:15, 5] <- c("con9", "con10", "con11")
loading[16:18, 6] <- c("con12", "con13", "con14")
loading[19:21, 7] <- c("con6", "con7", "con8")
loading[22:24, 8] <- c("con9", "con10", "con11")
loading[25:27, 9] <- c("con12", "con13", "con14")
LY <- bind(loading, "runif(1, 0.5, 0.7)")

errorCor <- diag(27)
errorCor[1, 10] <- errorCor[10, 19] <- NA
errorCor[2, 11] <- errorCor[11, 20] <- NA
errorCor[3, 12] <- errorCor[12, 21] <- NA
errorCor[4, 13] <- errorCor[13, 22] <- NA
errorCor[5, 14] <- errorCor[14, 23] <- NA
errorCor[6, 15] <- errorCor[15, 24] <- NA
errorCor[7, 16] <- errorCor[16, 25] <- NA
errorCor[8, 17] <- errorCor[17, 26] <- NA
errorCor[9, 18] <- errorCor[18, 27] <- NA
errorCor[1, 19] <- NA
errorCor[2, 20] <- NA
errorCor[3, 21] <- NA
errorCor[4, 22] <- NA
errorCor[5, 23] <- NA
errorCor[6, 24] <- NA
errorCor[7, 25] <- NA
errorCor[8, 26] <- NA
errorCor[9, 27] <- NA
errorCor <- errorCor + t(errorCor)
diag(errorCor) <- 1
errorCorVal <- diag(27)
errorCorVal[1, 10] <- errorCorVal[10, 19] <- 0.2
errorCorVal[2, 11] <- errorCorVal[11, 20] <- 0.2
errorCorVal[3, 12] <- errorCorVal[12, 21] <- 0.2
errorCorVal[4, 13] <- errorCorVal[13, 22] <- 0.2
errorCorVal[5, 14] <- errorCorVal[14, 23] <- 0.2
errorCorVal[6, 15] <- errorCorVal[15, 24] <- 0.2
errorCorVal[7, 16] <- errorCorVal[16, 25] <- 0.2
errorCorVal[8, 17] <- errorCorVal[17, 26] <- 0.2
errorCorVal[9, 18] <- errorCorVal[18, 27] <- 0.2
errorCorVal[1, 19] <- 0.04
errorCorVal[2, 20] <- 0.04
errorCorVal[3, 21] <- 0.04
errorCorVal[4, 22] <- 0.04
errorCorVal[5, 23] <- 0.04
errorCorVal[6, 24] <- 0.04
errorCorVal[7, 25] <- 0.04
errorCorVal[8, 26] <- 0.04
errorCorVal[9, 27] <- 0.04
errorCorVal <- errorCorVal + t(errorCorVal)
diag(errorCorVal) <- 1
RTE <- binds(errorCor, errorCorVal)

VE <- bind(c(NA, NA, NA, "con15", "con16", "con17", "con15", "con16", "con17"), 1)

longMed <- model(BE=BE, RPS=RPS, LY=LY, RTE=RTE, VE=VE, modelType="SEM")

output <- sim(1000, n=200, longMed, createOrder = c(3, 2, 1))
getCutoff(output, 0.05)
plotCutoff(output, 0.05)
summary(output)

LY2 <- matrix(0, 9, 3)
LY2[1:3, 1] <- NA
LY2[4:6, 2] <- NA
LY2[7:9, 3] <- NA
BE2 <- matrix(0, 3, 3)
BE2[2,1] <- NA
BE2[3,2] <- NA
crossMed <- estmodel(LY=LY2, BE=BE2, indLab=paste0("y", 19:27), modelType="SEM")

output2 <- sim(1000, n=200, crossMed, generate=longMed, createOrder = c(3, 2, 1))
getCutoff(output2, 0.05)
plotCutoff(output2, 0.05)
summary(output2)
