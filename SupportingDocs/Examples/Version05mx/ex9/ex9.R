library(simsem)

Avalues <- matrix(0, 52, 52)
Avalues[1:12, 49] <- 0.7
Avalues[13:24, 50] <- 0.7
Avalues[25:36, 51] <- 0.7
Avalues[37:48, 52] <- 0.7
Afree <- matrix(FALSE, 52, 52)
Afree[1:12, 49] <- TRUE
Afree[13:24, 50] <- TRUE
Afree[25:36, 51] <- TRUE
Afree[37:48, 52] <- TRUE

Svalues <- diag(c(rep(0.51, 48), rep(1, 4)))
facvalues <- matrix(0.3, 4, 4)
diag(facvalues) <- 1
Svalues[49:52, 49:52] <- facvalues
Sfree <- matrix(FALSE, 52, 52)
Sfree[49:52, 49:52] <- TRUE
diag(Sfree)[1:48] <- TRUE
diag(Sfree)[49:52] <- FALSE
Fvalues <- cbind(diag(48), matrix(0, 48, 4))

popModel <- mxModel("Four Factor Model",
    type="RAM",
    mxMatrix(type="Full", nrow=52, ncol=52, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=52, ncol=52, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=48, ncol=52, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=52, values=rep(0, 52), free=c(rep(TRUE, 48), rep(FALSE, 4)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:48), "f1", "f2", "f3", "f4"))
)

setx <- c(1:3, 13:15, 25:27, 37:39)
set1 <- setx + 3
set2 <- set1 + 3
set3 <- set2 + 3
itemGroups <- list(setx, set1, set2, set3)

missModel <- miss(nforms=3, itemGroups=itemGroups) # No multiple imputation for OpenMx now

Output <- sim(1000, n=1000, popModel, miss=missModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
