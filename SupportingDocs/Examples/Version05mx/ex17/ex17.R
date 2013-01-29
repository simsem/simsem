library(simsem)
library(OpenMx)

Avalues <- matrix(0, 36, 36)
Avalues[1:3, 28] <- 1
Avalues[4:6, 29] <- 1
Avalues[7:9, 30] <- 1
Avalues[10:12, 31] <- 1
Avalues[13:15, 32] <- 1
Avalues[16:18, 33] <- 1
Avalues[19:21, 34] <- 1
Avalues[22:24, 35] <- 1
Avalues[25:27, 36] <- 1
Avalues[31, 28] <- 0.6
Avalues[32, 29] <- 0.6
Avalues[33, 30] <- 0.6
Avalues[34, 31] <- 0.6
Avalues[35, 32] <- 0.6
Avalues[36, 33] <- 0.6
Avalues[32, 28] <- 0.3
Avalues[33, 29] <- 0.3
Avalues[35, 31] <- 0.3
Avalues[36, 32] <- 0.3
Afree <- matrix(FALSE, 36, 36)
Afree[1:3, 28] <- c(FALSE, TRUE, TRUE)
Afree[4:6, 29] <- c(FALSE, TRUE, TRUE)
Afree[7:9, 30] <- c(FALSE, TRUE, TRUE)
Afree[10:12, 31] <- c(FALSE, TRUE, TRUE)
Afree[13:15, 32] <- c(FALSE, TRUE, TRUE)
Afree[16:18, 33] <- c(FALSE, TRUE, TRUE)
Afree[19:21, 34] <- c(FALSE, TRUE, TRUE)
Afree[22:24, 35] <- c(FALSE, TRUE, TRUE)
Afree[25:27, 36] <- c(FALSE, TRUE, TRUE)
Afree[31, 28] <- TRUE
Afree[32, 29] <- TRUE
Afree[33, 30] <- TRUE
Afree[34, 31] <- TRUE
Afree[35, 32] <- TRUE
Afree[36, 33] <- TRUE
Afree[32, 28] <- TRUE
Afree[33, 29] <- TRUE
Afree[35, 31] <- TRUE
Afree[36, 32] <- TRUE
Alabels <- matrix(NA, 36, 36)
Alabels[1:3, 28] <- c(NA, "con1", "con2")
Alabels[4:6, 29] <- c(NA, "con3", "con4")
Alabels[7:9, 30] <- c(NA, "con5", "con6")
Alabels[10:12, 31] <- c(NA, "con1", "con2")
Alabels[13:15, 32] <- c(NA, "con3", "con4")
Alabels[16:18, 33] <- c(NA, "con5", "con6")
Alabels[19:21, 34] <- c(NA, "con1", "con2")
Alabels[22:24, 35] <- c(NA, "con3", "con4")
Alabels[25:27, 36] <- c(NA, "con5", "con6")
Alabels[31, 28] <- "con7"
Alabels[32, 29] <- "con9"
Alabels[33, 30] <- "con11"
Alabels[34, 31] <- "con7"
Alabels[35, 32] <- "con9"
Alabels[36, 33] <- "con11"
Alabels[32, 28] <- "con8"
Alabels[33, 29] <- "con10"
Alabels[35, 31] <- "con8"
Alabels[36, 32] <- "con10"

Svalues <- matrix(0, 36, 36)
Svalues[28, 29] <- 0.2
Svalues[28, 30] <- 0.2
Svalues[29, 30] <- 0.2
Svalues[1, 10] <- 0.2
Svalues[2, 11] <- 0.2
Svalues[3, 12] <- 0.2
Svalues[4, 13] <- 0.2
Svalues[5, 14] <- 0.2
Svalues[6, 15] <- 0.2
Svalues[7, 16] <- 0.2
Svalues[8, 17] <- 0.2
Svalues[9, 18] <- 0.2
Svalues[10, 19] <- 0.2
Svalues[11, 20] <- 0.2
Svalues[12, 21] <- 0.2
Svalues[13, 22] <- 0.2
Svalues[14, 23] <- 0.2
Svalues[15, 24] <- 0.2
Svalues[16, 25] <- 0.2
Svalues[17, 26] <- 0.2
Svalues[18, 27] <- 0.2
Svalues[1, 19] <- 0.04
Svalues[2, 20] <- 0.04
Svalues[3, 21] <- 0.04
Svalues[4, 22] <- 0.04
Svalues[5, 23] <- 0.04
Svalues[6, 24] <- 0.04
Svalues[7, 25] <- 0.04
Svalues[8, 26] <- 0.04
Svalues[9, 27] <- 0.04
Svalues <- Svalues + t(Svalues)
diag(Svalues) <- c(rep(0.5, 27), rep(1, 3), rep(0.6, 6))
Sfree <- matrix(FALSE, 36, 36)
Sfree[28, 29] <- TRUE
Sfree[28, 30] <- TRUE
Sfree[29, 30] <- TRUE
Sfree[1, 10] <- TRUE
Sfree[2, 11] <- TRUE
Sfree[3, 12] <- TRUE
Sfree[4, 13] <- TRUE
Sfree[5, 14] <- TRUE
Sfree[6, 15] <- TRUE
Sfree[7, 16] <- TRUE
Sfree[8, 17] <- TRUE
Sfree[9, 18] <- TRUE
Sfree[10, 19] <- TRUE
Sfree[11, 20] <- TRUE
Sfree[12, 21] <- TRUE
Sfree[13, 22] <- TRUE
Sfree[14, 23] <- TRUE
Sfree[15, 24] <- TRUE
Sfree[16, 25] <- TRUE
Sfree[17, 26] <- TRUE
Sfree[18, 27] <- TRUE
Sfree[1, 19] <- TRUE
Sfree[2, 20] <- TRUE
Sfree[3, 21] <- TRUE
Sfree[4, 22] <- TRUE
Sfree[5, 23] <- TRUE
Sfree[6, 24] <- TRUE
Sfree[7, 25] <- TRUE
Sfree[8, 26] <- TRUE
Sfree[9, 27] <- TRUE
Sfree <- Sfree | t(Sfree)
diag(Sfree) <- TRUE
Slabels <- matrix(NA, 36, 36)
diag(Slabels)[31:36] <- c("con12", "con13", "con14", "con12", "con13", "con14")
Fvalues <- cbind(diag(27), matrix(0, 27, 9))

popModel <- mxModel("Longitudinal Mediation",
    type="RAM",
    mxMatrix(type="Full", nrow=36, ncol=36, values=Avalues, free=Afree, labels=Alabels, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=36, ncol=36, values=Svalues, free=Sfree, labels=Slabels, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=27, ncol=36, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=36, values=rep(0, 36), free=c(rep(TRUE, 27), rep(FALSE, 9)), name="M"),
	mxAlgebra(expression = A[32, 28] * A[36, 32], name = "med"),
	mxCI("med"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:27), paste0("f", 1:9)))
)

outfun <- function(object) {
	object@output$confidenceIntervals
}

Output1 <- sim(1000, n=200, popModel, intervals=TRUE, outfun=outfun)
summary(Output1)
ciResult <- do.call(rbind, getExtraOutput(Output1))
# Population value of med = 0.09
coverage <- (ciResult[,1] < 0.09) & (0.09 < ciResult[,2])
mean(coverage)

Avalues2 <- matrix(0, 12, 12)
Avalues2[1:3, 10] <- 1
Avalues2[4:6, 11] <- 1
Avalues2[7:9, 12] <- 1
Avalues2[11, 10] <- 0.3
Avalues2[12, 11] <- 0.3
Afree2 <- matrix(FALSE, 12, 12)
Afree2[1:3, 10] <- c(FALSE, TRUE, TRUE)
Afree2[4:6, 11] <- c(FALSE, TRUE, TRUE)
Afree2[7:9, 12] <- c(FALSE, TRUE, TRUE)
Afree2[11, 10] <- TRUE
Afree2[12, 11] <- TRUE
Alabels2 <- matrix(NA, 12, 12)
Alabels2[1:3, 10] <- c(NA, "con1", "con2")
Alabels2[4:6, 11] <- c(NA, "con3", "con4")
Alabels2[7:9, 12] <- c(NA, "con5", "con6")
Alabels2[11, 10] <- "con8"
Alabels2[12, 11] <- "con10"

Svalues2 <- diag(c(rep(0.5, 9), 1, 0.6, 0.6))
Sfree2 <- matrix(FALSE, 12, 12)
diag(Sfree2) <- TRUE
Fvalues2 <- cbind(diag(9), matrix(0, 9, 3))

analyzeModel2 <- mxModel("Cross_sectional Mediation Model",
    type="RAM",
    mxMatrix(type="Full", nrow=12, ncol=12, values=Avalues2, free=Afree2, labels=Alabels2, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=12, ncol=12, values=Svalues2, free=Sfree2, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=9, ncol=12, free=FALSE, values=Fvalues2, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=12, values=rep(0, 12), free=c(rep(TRUE, 9), rep(FALSE, 3)), name="M"),
	mxAlgebra(expression = A[12, 11] * A[11, 10], name = "med"),
	mxCI("med"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:9), "f1", "f2", "f3"))
)

Output2 <- sim(1000, n=200, analyzeModel2, generate=popModel, intervals=TRUE, outfun=outfun)
summary(Output2)
summaryParam(Output2, matchParam = TRUE)
ciResult2 <- do.call(rbind, getExtraOutput(Output2))
# Population value of med = 0.09
coverage2 <- (ciResult2[,1] < 0.09) & (0.09 < ciResult2[,2])
mean(coverage2)
