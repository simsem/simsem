library(simsem)

# First analysis model: Model without covariate

Avalues <- matrix(0, 10, 10)
Avalues[1:3, 8] <- 0.6
Avalues[4:6, 9] <- 0.6
Avalues[1:7, 10] <- c(rep(0.4, 6), 1)
Afree <- matrix(FALSE, 10, 10)
Afree[1:3, 8] <- TRUE
Afree[4:6, 9] <- TRUE
Afree[1:6, 10] <- TRUE

Svalues <- diag(c(rep(0.64, 6), 0, 1, 1, 1))
Svalues[8, 9] <- Svalues[9, 8] <- 0.4
Sfree <- matrix(FALSE, 10, 10)
diag(Sfree)[c(1:6, 10)] <- TRUE
Sfree[8, 9] <- Sfree[9, 8] <- TRUE
Fvalues <- cbind(diag(7), matrix(0, 7, 3))

popModel <- mxModel("Covariate Population Data",
    type="RAM",
    mxMatrix(type="Full", nrow=10, ncol=10, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=10, ncol=10, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=7, ncol=10, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=10, values=rep(0, 10), free=c(rep(TRUE, 7), rep(FALSE, 3)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:7), "f1", "f2", "f3"))
)

Avalues1 <- matrix(0, 8, 8)
Avalues1[1:3, 7] <- 0.6
Avalues1[4:6, 8] <- 0.6
Afree1 <- matrix(FALSE, 8, 8)
Afree1[1:3, 7] <- TRUE
Afree1[4:6, 8] <- TRUE

Svalues1 <- diag(c(rep(0.64, 6), 1, 1))
Svalues1[7, 8] <- Svalues1[8, 7] <- 0.4
Sfree1 <- matrix(FALSE, 8, 8)
diag(Sfree1)[1:6] <- TRUE
Sfree1[7, 8] <- Sfree1[8, 7] <- TRUE
Fvalues1 <- cbind(diag(6), matrix(0, 6, 2))

analyzeModel1 <- mxModel("Model without covariate",
    type="RAM",
    mxMatrix(type="Full", nrow=8, ncol=8, values=Avalues1, free=Afree1, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=8, ncol=8, values=Svalues1, free=Sfree1, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=Fvalues1, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=8, values=rep(0, 8), free=c(rep(TRUE, 6), rep(FALSE, 2)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
)

Output1 <- sim(1000, n=200, analyzeModel1, generate=popModel)
summary(Output1)

# Second analysis model: Model accounting for covariate in the indicator level

analyzeModel2 <- popModel

Output2 <- sim(1000, n=200, analyzeModel2, generate=popModel)
summary(Output2)

# Third analysis model: Model accounting for covariate with orthogonalization

library(semTools)

datafun <- function(data) {
	residualCovariate(data, targetVar=1:6, covVar=7)
}

analyzeModel3 <- analyzeModel1

Output3 <- sim(1000, n=200, analyzeModel3, generate=popModel, datafun=datafun)
summary(Output3)

# Fourth analysis model: Model accounting for covariate in factor level

analyzeModel4 <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f2 ~ f1 + y7
f1 ~ y7
f1 ~~ 1*f1
f2 ~~ 1*f2
"

Avalues4 <- matrix(0, 10, 10)
Avalues4[1:3, 8] <- 0.6
Avalues4[4:6, 9] <- 0.6
Avalues4[7, 10] <- 1
Avalues4[8:9, 10] <- 0.4
Afree4 <- matrix(FALSE, 10, 10)
Afree4[1:3, 8] <- TRUE
Afree4[4:6, 9] <- TRUE
Afree4[8:9, 10] <- TRUE

Svalues4 <- diag(c(rep(0.64, 6), 0, 1, 1, 1))
Svalues4[8, 9] <- Svalues4[9, 8] <- 0.4
Sfree4 <- matrix(FALSE, 10, 10)
diag(Sfree4)[c(1:6, 10)] <- TRUE
Sfree4[8, 9] <- Sfree4[9, 8] <- TRUE
Fvalues4 <- cbind(diag(7), matrix(0, 7, 3))

analyzeModel4 <- mxModel("Model with covariate at factor level",
    type="RAM",
    mxMatrix(type="Full", nrow=10, ncol=10, values=Avalues4, free=Afree4, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=10, ncol=10, values=Svalues4, free=Sfree4, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=7, ncol=10, free=FALSE, values=Fvalues4, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=10, values=rep(0, 10), free=c(rep(TRUE, 7), rep(FALSE, 3)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:7), "f1", "f2", "f3"))
)

Output4 <- sim(1000, n=200, analyzeModel4, generate=popModel)
summary(Output4)
summaryParam(Output4, matchParam=TRUE) # Ignore y7 ~~ y7 which means error variance in data-generation model but total variance in analysis model
