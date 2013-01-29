library(simsem)

# a) Noninvariance

AvaluesNon1 <- matrix(0, 7, 7)
AvaluesNon1[1:6, 7] <- 0.7
AfreeNon1 <- matrix(FALSE, 7, 7)
AfreeNon1[1:6, 7] <- TRUE

SvaluesNon1 <- diag(c(rep(0.51, 6), 1))
SfreeNon1 <- matrix(FALSE, 7, 7)
diag(SfreeNon1)[1:6] <- TRUE
FvaluesNon1 <- cbind(diag(6), matrix(0, 6, 1))

MvaluesNon1 <- c(-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0)
MfreeNon1 <- c(rep(TRUE, 6), FALSE)

AvaluesNon2 <- matrix(0, 8, 8)
AvaluesNon2[1:3, 7] <- 0.7
AvaluesNon2[4:6, 8] <- 0.7
AfreeNon2 <- matrix(FALSE, 8, 8)
AfreeNon2[1:3, 7] <- TRUE
AfreeNon2[4:6, 8] <- TRUE

SvaluesNon2 <- diag(c(rep(0.51, 6), 1, 1))
SvaluesNon2[7, 8] <- SvaluesNon2[8, 7] <- 0.5
SfreeNon2 <- matrix(FALSE, 8, 8)
diag(SfreeNon2)[1:6] <- TRUE
SfreeNon2[7, 8] <- SfreeNon2[8, 7] <- TRUE
FvaluesNon2 <- cbind(diag(6), matrix(0, 6, 2))

MvaluesNon2 <- c(0.2, 0.1, 0, -0.1, -0.2, -0.3, 0, 0)
MfreeNon2 <- c(rep(TRUE, 6), FALSE, FALSE)

groupNon1 <- mxModel("group1",
		type="RAM",
		mxMatrix(type="Full", nrow=7, ncol=7, values=AvaluesNon1, free=AfreeNon1, byrow=TRUE, name="A"),
		mxMatrix(type="Symm", nrow=7, ncol=7, values=SvaluesNon1, free=SfreeNon1, byrow=TRUE, name="S"),
		mxMatrix(type="Full", nrow=6, ncol=7, free=FALSE, values=FvaluesNon1, byrow=TRUE, name="F"),
		mxMatrix(type="Full", nrow=1, ncol=7, values=MvaluesNon1, free=MfreeNon1, name="M"),
		mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1"))
	)

groupNon2 <- mxModel("group2",
		type="RAM",
		mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesNon2, free=AfreeNon2, byrow=TRUE, name="A"),
		mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesNon2, free=SfreeNon2, byrow=TRUE, name="S"),
		mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesNon2, byrow=TRUE, name="F"),
		mxMatrix(type="Full", nrow=1, ncol=8, values=MvaluesNon2, free=MfreeNon2, name="M"),
		mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	)

popModelNoninvariance <- mxModel("Noninvariance",
	groupNon1, 
	groupNon2, 
	mxAlgebra(
        group1.objective + group2.objective,
        name="minus2loglikelihood"
    ),
    mxAlgebraObjective("minus2loglikelihood")
)

Output.a <- sim(1000, popModelNoninvariance, n=list(200, 200), group = "group", mxFit=TRUE) 
getCutoff(Output.a, 0.05)
plotCutoff(Output.a, 0.05)
summary(Output.a)

# b) Configural Invariance

AvaluesCon1 <- matrix(0, 8, 8)
AvaluesCon1[1:3, 7] <- c(0.7, 0.6, 0.5)
AvaluesCon1[4:6, 8] <- c(0.7, 0.6, 0.5)
AfreeCon1 <- matrix(FALSE, 8, 8)
AfreeCon1[1:3, 7] <- TRUE
AfreeCon1[4:6, 8] <- TRUE

SvaluesCon1 <- diag(c(0.51, 0.64, 0.75, 0.51, 0.64, 0.75, 1, 1))
SvaluesCon1[7, 8] <- SvaluesCon1[8, 7] <- 0.5
SfreeCon1 <- matrix(FALSE, 8, 8)
diag(SfreeCon1)[1:6] <- TRUE
SfreeCon1[7, 8] <- SfreeCon1[8, 7] <- TRUE
FvaluesCon1 <- cbind(diag(6), matrix(0, 6, 2))

MvaluesCon1 <- c(-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0, 0)
MfreeCon1 <- c(rep(TRUE, 6), FALSE, FALSE)

AvaluesCon2 <- matrix(0, 8, 8)
AvaluesCon2[1:3, 7] <- c(0.5, 0.6, 0.7)
AvaluesCon2[4:6, 8] <- c(0.5, 0.6, 0.7)
AfreeCon2 <- matrix(FALSE, 8, 8)
AfreeCon2[1:3, 7] <- TRUE
AfreeCon2[4:6, 8] <- TRUE

SvaluesCon2 <- diag(c(0.75, 0.64, 0.51, 0.75, 0.64, 0.51, 1, 1))
SvaluesCon2[7, 8] <- SvaluesCon2[8, 7] <- 0.5
SfreeCon2 <- matrix(FALSE, 8, 8)
diag(SfreeCon2)[1:6] <- TRUE
SfreeCon2[7, 8] <- SfreeCon2[8, 7] <- TRUE
FvaluesCon2 <- cbind(diag(6), matrix(0, 6, 2))

MvaluesCon2 <- c(0.2, 0.1, 0, -0.1, -0.2, -0.3, 0, 0)
MfreeCon2 <- c(rep(TRUE, 6), FALSE, FALSE)

groupCon1 <- mxModel("group1",
		type="RAM",
		mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesCon1, free=AfreeCon1, byrow=TRUE, name="A"),
		mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesCon1, free=SfreeCon1, byrow=TRUE, name="S"),
		mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesCon1, byrow=TRUE, name="F"),
		mxMatrix(type="Full", nrow=1, ncol=8, values=MvaluesCon1, free=MfreeCon1, name="M"),
		mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	)

groupCon2 <- mxModel("group2",
		type="RAM",
		mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesCon2, free=AfreeCon2, byrow=TRUE, name="A"),
		mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesCon2, free=SfreeCon2, byrow=TRUE, name="S"),
		mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesCon2, byrow=TRUE, name="F"),
		mxMatrix(type="Full", nrow=1, ncol=8, values=MvaluesCon2, free=MfreeCon2, name="M"),
		mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	)

popModelConfigural <- mxModel("Configural Invariance",
	groupCon1, 
	groupCon2, 
	mxAlgebra(
        group1.objective + group2.objective,
        name="minus2loglikelihood"
    ),
    mxAlgebraObjective("minus2loglikelihood")
)

Output.b <- sim(1000, popModelConfigural, n=list(200, 200)) 
getCutoff(Output.b, 0.05)
plotCutoff(Output.b, 0.05)
summary(Output.b)

# c) Weak Invariance

AvaluesWeak <- matrix(0, 8, 8)
AvaluesWeak[1:3, 7] <- c(0.7, 0.6, 0.5)
AvaluesWeak[4:6, 8] <- c(0.7, 0.6, 0.5)
AfreeWeak <- matrix(FALSE, 8, 8)
AfreeWeak[1:3, 7] <- TRUE
AfreeWeak[4:6, 8] <- TRUE
AlabelsWeak <- matrix(NA, 8, 8)
AlabelsWeak[1:3, 7] <- c("l1", "l2", "l3")
AlabelsWeak[4:6, 8] <- c("l4", "l5", "l6")

SvaluesWeak1 <- diag(c(0.51, 0.64, 0.75, 0.51, 0.64, 0.75, 1, 1))
SvaluesWeak1[7, 8] <- SvaluesWeak1[8, 7] <- 0.5
SfreeWeak1 <- matrix(FALSE, 8, 8)
diag(SfreeWeak1)[1:6] <- TRUE
SfreeWeak1[7, 8] <- SfreeWeak1[8, 7] <- TRUE
FvaluesWeak1 <- cbind(diag(6), matrix(0, 6, 2))

MvaluesWeak1 <- c(-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0, 0)
MfreeWeak1 <- c(rep(TRUE, 6), FALSE, FALSE)

SvaluesWeak2 <- diag(c(0.75, 0.64, 0.51, 0.75, 0.64, 0.51, 4, 4))
SvaluesWeak2[7, 8] <- SvaluesWeak2[8, 7] <- 2
SfreeWeak2 <- matrix(FALSE, 8, 8)
diag(SfreeWeak2)[1:8] <- TRUE
SfreeWeak2[7, 8] <- SfreeWeak2[8, 7] <- TRUE
FvaluesWeak2 <- cbind(diag(6), matrix(0, 6, 2))

MvaluesWeak2 <- c(0.2, 0.1, 0, -0.1, -0.2, -0.3, 0, 0)
MfreeWeak2 <- c(rep(TRUE, 6), FALSE, FALSE)

groupWeak1 <- mxModel("group1",
		type="RAM",
		mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesWeak, free=AfreeWeak, labels=AlabelsWeak, byrow=TRUE, name="A"),
		mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesWeak1, free=SfreeWeak1, byrow=TRUE, name="S"),
		mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesWeak1, byrow=TRUE, name="F"),
		mxMatrix(type="Full", nrow=1, ncol=8, values=MvaluesWeak1, free=MfreeWeak1, name="M"),
		mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	)

groupWeak2 <- mxModel("group2",
		type="RAM",
		mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesWeak, free=AfreeWeak, labels=AlabelsWeak, byrow=TRUE, name="A"),
		mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesWeak2, free=SfreeWeak2, byrow=TRUE, name="S"),
		mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesWeak2, byrow=TRUE, name="F"),
		mxMatrix(type="Full", nrow=1, ncol=8, values=MvaluesWeak2, free=MfreeWeak2, name="M"),
		mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	)

popModelWeak <- mxModel("Weak Invariance",
	groupWeak1, 
	groupWeak2, 
	mxAlgebra(
        group1.objective + group2.objective,
        name="minus2loglikelihood"
    ),
    mxAlgebraObjective("minus2loglikelihood")
)

Output.c <- sim(1000, popModelWeak, n=list(200, 200)) 
getCutoff(Output.c, 0.05)
plotCutoff(Output.c, 0.05)
summary(Output.c)

# d) Strong Invariance

AvaluesStrong <- matrix(0, 8, 8)
AvaluesStrong[1:3, 7] <- c(0.7, 0.6, 0.5)
AvaluesStrong[4:6, 8] <- c(0.7, 0.6, 0.5)
AfreeStrong <- matrix(FALSE, 8, 8)
AfreeStrong[1:3, 7] <- TRUE
AfreeStrong[4:6, 8] <- TRUE
AlabelsStrong <- matrix(NA, 8, 8)
AlabelsStrong[1:3, 7] <- c("l1", "l2", "l3")
AlabelsStrong[4:6, 8] <- c("l4", "l5", "l6")

SvaluesStrong1 <- diag(c(0.51, 0.64, 0.75, 0.51, 0.64, 0.75, 1, 1))
SvaluesStrong1[7, 8] <- SvaluesStrong1[8, 7] <- 0.5
SfreeStrong1 <- matrix(FALSE, 8, 8)
diag(SfreeStrong1)[1:6] <- TRUE
SfreeStrong1[7, 8] <- SfreeStrong1[8, 7] <- TRUE
FvaluesStrong1 <- cbind(diag(6), matrix(0, 6, 2))

MvaluesStrong1 <- c(-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0, 0)
MfreeStrong1 <- c(rep(TRUE, 6), FALSE, FALSE)
MlabelsStrong1 <- c("i1", "i2", "i3", "i4", "i5", "i6", NA, NA)

SvaluesStrong2 <- diag(c(0.75, 0.64, 0.51, 0.75, 0.64, 0.51, 4, 4))
SvaluesStrong2[7, 8] <- SvaluesStrong2[8, 7] <- 2
SfreeStrong2 <- matrix(FALSE, 8, 8)
diag(SfreeStrong2)[1:8] <- TRUE
SfreeStrong2[7, 8] <- SfreeStrong2[8, 7] <- TRUE
FvaluesStrong2 <- cbind(diag(6), matrix(0, 6, 2))

MvaluesStrong2 <- c(-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.5, -0.5)
MfreeStrong2 <- rep(TRUE, 8)
MlabelsStrong2 <- c("i1", "i2", "i3", "i4", "i5", "i6", NA, NA)

groupStrong1 <- mxModel("group1",
		type="RAM",
		mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesStrong, free=AfreeStrong, labels=AlabelsStrong, byrow=TRUE, name="A"),
		mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesStrong1, free=SfreeStrong1, byrow=TRUE, name="S"),
		mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesStrong1, byrow=TRUE, name="F"),
		mxMatrix(type="Full", nrow=1, ncol=8, values=MvaluesStrong1, free=MfreeStrong1, labels=MlabelsStrong1, name="M"),
		mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	)

groupStrong2 <- mxModel("group2",
		type="RAM",
		mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesStrong, free=AfreeStrong, labels=AlabelsStrong, byrow=TRUE, name="A"),
		mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesStrong2, free=SfreeStrong2, byrow=TRUE, name="S"),
		mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesStrong2, byrow=TRUE, name="F"),
		mxMatrix(type="Full", nrow=1, ncol=8, values=MvaluesStrong2, free=MfreeStrong2, labels=MlabelsStrong2, name="M"),
		mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	)

popModelStrong <- mxModel("Strong Invariance",
	groupStrong1, 
	groupStrong2, 
	mxAlgebra(
        group1.objective + group2.objective,
        name="minus2loglikelihood"
    ),
    mxAlgebraObjective("minus2loglikelihood")
)

Output.d <- sim(1000, popModelStrong, n=list(200, 200)) 
getCutoff(Output.d, 0.05)
plotCutoff(Output.d, 0.05)
summary(Output.d)
