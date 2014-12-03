library(simsem)
library(OpenMx)

Avalues <- matrix(0, 6, 6)
Avalues[1:4, 5] <- 1
Avalues[1:4, 6] <- 0:3
Afree <- matrix(FALSE, 6, 6)

Svalues1 <- diag(c(rep(0.5, 4), 1, 0.25))
Svalues1[5, 6] <- Svalues1[6, 5] <- 0.05
Sfree <- matrix(FALSE, 6, 6)
diag(Sfree) <- TRUE
Sfree[5, 6] <- Sfree[6, 5] <- TRUE
Slabels <- matrix(NA, 6, 6)
diag(Slabels)[1:4] <- paste("resid", 1:4)

Svalues2 <- diag(c(rep(0.5, 4), 1, 0.25))
Svalues2[5, 6] <- Svalues2[6, 5] <- 0.05

Fvalues <- cbind(diag(4), matrix(0, 4, 2))

Mvalues1 <- c(rep(0, 4), 5, 2)
Mvalues2 <- c(rep(0, 4), 5, 0)

subpop1 <- mxModel("subpop1",
    type="RAM",
    mxMatrix(type="Full", nrow=6, ncol=6, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=6, ncol=6, values=Svalues1, free=Sfree, labels=Slabels, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=4, ncol=6, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=6, values=Mvalues1, free=c(rep(FALSE, 4), rep(TRUE, 2)), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("t", 1:4), "i", "s"))
)

subpop2 <- mxModel(subpop1, name="subpop2",
    mxMatrix(type="Full", nrow=6, ncol=6, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=6, ncol=6, values=Svalues2, free=Sfree, labels=Slabels, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=4, ncol=6, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=6, values=Mvalues2, free=c(rep(FALSE, 4), rep(TRUE, 2)), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("t", 1:4), "i", "s"))
)

# Class 1 = 20%; Class 2 = 80% --> Odds = .2/.8 and .8/.8
odds <- mxMatrix("Full", 2, 1, free=c(TRUE, FALSE), 
          values=c(0.2/0.8, 1), lbound=0.001, 
          labels = c("p1", "p2"), name="props")

props <- mxAlgebra(props%x%(1/sum(props)), name="classProbs")

algObj <- mxAlgebra(-2*sum(
          log(classProbs[1,1]%x%subpop1.objective + classProbs[2,1]%x%subpop2.objective)), 
          name="mixtureObj")

obj <- mxFitFunctionAlgebra("mixtureObj")
      
popModel <- mxModel("Growth Mixture Model",
    subpop1, subpop2,
    odds, props,
    algObj, obj
)  

Output <- sim(1000, popModel, n = list(200, 800), mxMixture = TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
