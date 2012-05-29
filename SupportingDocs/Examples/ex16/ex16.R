library(simsem)
u35 <- simUnif(0.1, 0.3)
u57 <- simUnif(0.5, 0.7)
u2 <- simUnif(-0.2, 0.2)

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- NA
path[5, 2] <- path[8, 5] <- NA
path[6, 3] <- path[9, 6] <- NA
path[5, 1] <- path[8, 4] <- NA
path[6, 2] <- path[9, 5] <- NA
pathVal <- matrix(0, 9, 9)
pathVal[4, 1] <- pathVal[7, 4] <- "u57"
pathVal[5, 2] <- pathVal[8, 5] <- "u57"
pathVal[6, 3] <- pathVal[9, 6] <- "u57"
pathVal[5, 1] <- pathVal[8, 4] <- "u35"
pathVal[6, 2] <- pathVal[9, 5] <- "u35"
BE <- simMatrix(path, pathVal)
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- NA
facCor[1, 3] <- facCor[3, 1] <- NA
facCor[2, 3] <- facCor[3, 2] <- NA
RPS <- symMatrix(facCor, "u35")
loading <- matrix(0, 27, 9)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA
loading[13:15, 5] <- NA
loading[16:18, 6] <- NA
loading[19:21, 7] <- NA
loading[22:24, 8] <- NA
loading[25:27, 9] <- NA
LY <- simMatrix(loading, "u57")
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
TE <- symMatrix(errorCor, errorCorVal)
longMed <- simSetSEM(BE=BE, RPS=RPS, LY=LY, RTE=TE)

c1 <- matrix(NA, 2, 1)
c1[,1] <- c(4, 7)
rownames(c1) <- rep("VPS", 2)

c2 <- matrix(NA, 2, 1)
c2[,1] <- c(5, 8)
rownames(c2) <- rep("VPS", 2)

c3 <- matrix(NA, 2, 1)
c3[,1] <- c(6, 9)
rownames(c3) <- rep("VPS", 2)

c4 <- matrix(NA, 2, 2)
c4[1,] <- c(4, 1)
c4[2,] <- c(7, 4)
rownames(c4) <- rep("BE", 2)

c5 <- matrix(NA, 2, 2)
c5[1,] <- c(5, 2)
c5[2,] <- c(8, 5)
rownames(c5) <- rep("BE", 2)

c6 <- matrix(NA, 2, 2)
c6[1,] <- c(6, 3)
c6[2,] <- c(9, 6)
rownames(c6) <- rep("BE", 2)

c7 <- matrix(NA, 2, 2)
c7[1,] <- c(5, 1)
c7[2,] <- c(8, 4)
rownames(c7) <- rep("BE", 2)

c8 <- matrix(NA, 2, 2)
c8[1,] <- c(6, 2)
c8[2,] <- c(9, 5)
rownames(c8) <- rep("BE", 2)

c9 <- matrix(NA, 3, 2)
c9[1,] <- c(1, 1)
c9[2,] <- c(10, 4)
c9[3,] <- c(19, 7)
rownames(c9) <- rep("LY", 3)

c10 <- matrix(NA, 3, 2)
c10[1,] <- c(2, 1)
c10[2,] <- c(11, 4)
c10[3,] <- c(20, 7)
rownames(c10) <- rep("LY", 3)

c11 <- matrix(NA, 3, 2)
c11[1,] <- c(3, 1)
c11[2,] <- c(12, 4)
c11[3,] <- c(21, 7)
rownames(c11) <- rep("LY", 3)

c12 <- matrix(NA, 3, 2)
c12[1,] <- c(4, 2)
c12[2,] <- c(13, 5)
c12[3,] <- c(22, 8)
rownames(c12) <- rep("LY", 3)

c13 <- matrix(NA, 3, 2)
c13[1,] <- c(5, 2)
c13[2,] <- c(14, 5)
c13[3,] <- c(23, 8)
rownames(c13) <- rep("LY", 3)

c14 <- matrix(NA, 3, 2)
c14[1,] <- c(6, 2)
c14[2,] <- c(15, 5)
c14[3,] <- c(24, 8)
rownames(c14) <- rep("LY", 3)

c15 <- matrix(NA, 3, 2)
c15[1,] <- c(7, 3)
c15[2,] <- c(16, 6)
c15[3,] <- c(25, 9)
rownames(c15) <- rep("LY", 3)

c16 <- matrix(NA, 3, 2)
c16[1,] <- c(8, 3)
c16[2,] <- c(17, 6)
c16[3,] <- c(26, 9)
rownames(c16) <- rep("LY", 3)

c17 <- matrix(NA, 3, 2)
c17[1,] <- c(9, 3)
c17[2,] <- c(18, 6)
c17[3,] <- c(27, 9)
rownames(c17) <- rep("LY", 3)

con <- simEqualCon(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, modelType="SEM", conBeforeFill=FALSE)

datModel <- simData(longMed, 200, equalCon=con)
SimModel <- simModel(longMed, equalCon=con)
output <- simResult(1000, datModel, SimModel)

LY2 <- matrix(0, 9, 3)
LY2[1:3, 1] <- NA
LY2[4:6, 2] <- NA
LY2[7:9, 3] <- NA
BE2 <- matrix(0, 3, 3)
BE2[2,1] <- NA
BE2[3,2] <- NA
crossMed <- simParamSEM(LY=LY2, BE=BE2)
SimModel2 <- simModel(crossMed, indLab=19:27)
output2 <- simResult(100, datModel, SimModel2)
