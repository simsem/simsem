pkgname <- "simsem"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('simsem')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Rnorm-class")
### * Rnorm-class

flush(stderr()); flush(stdout())

### Name: Rnorm-class
### Title: Class "Rnorm"
### Aliases: Rnorm-class run,Rnorm-method summary,Rnorm-method
### Keywords: classes

### ** Examples

showClass("Rnorm")
n2 <- rnorm.object(0, 0.2)
run(n2)
summary(n2)



cleanEx()
nameEx("Runif-class")
### * Runif-class

flush(stderr()); flush(stdout())

### Name: Runif-class
### Title: Class "Runif"
### Aliases: Runif-class run,Runif-method summary,Runif-method
### Keywords: classes

### ** Examples

showClass("Runif")
u1 <- runif.object(-0.1, 0.1)
run(u1)
summary(u1)



cleanEx()
nameEx("adjust.object")
### * adjust.object

flush(stderr()); flush(stdout())

### Name: adjust.object
### Title: Change an element in 'simMatrix', 'symMatrix', or 'simVector'.
### Aliases: adjust.object adjust.object-methods adjust.object,ANY-method
###   adjust.object,simMatrix-method adjust.object,symMatrix-method
###   adjust.object,simVector-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- matrix.object(loading, 0.7)
summary(LX)
run(LX)

u34 <- runif.object(0.3, 0.4)
LX <- adjust.object(LX, "u34", c(2, 1))
summary(LX)
run(LX)

LX <- adjust.object(LX, 0, c(2,1))
LX <- adjust.object(LX, 0.5, c(2,2), FALSE)
summary(LX)
run(LX)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- vector.object(factor.mean, factor.mean.starting)
run(AL)
summary(AL)

n01 <- rnorm.object(0, 1)
AL <- adjust.object(AL, "n01", 2)
run(AL)
summary(AL)



cleanEx()
nameEx("constraint.object")
### * constraint.object

flush(stderr()); flush(stdout())

### Name: constraint.object
### Title: Equality Constraint Object
### Aliases: constraint.object

### ** Examples

# Example 1: Single-group, one constraint
constraint <- matrix(0, 3, 2)
constraint[1,] <- c(1, 1)
constraint[2,] <- c(2, 1)
constraint[3,] <- c(3, 1)
rownames(constraint) <- rep("LY", 3)
equal.loading <- constraint.object(constraint, Tag="SEM.exo")

# Example 2: Multiple-group, one constraint
group.con <- matrix(0, 2, 3)
group.con[1,] <- c(1, 2, 1)
group.con[2,] <- c(2, 2, 1)
rownames(group.con) <- rep("BE", 2)
equal.path <- constraint.object(group.con, Tag="Path")

# Example 3: Single-group, multiple constraints
constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading2 <- constraint.object(constraint1, constraint2, constraint3, Tag="SEM")
summary(equal.loading2)



cleanEx()
nameEx("data.object")
### * data.object

flush(stderr()); flush(stdout())

### Name: data.object
### Title: Data object
### Aliases: data.object

### ** Examples


loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(200, CFA.Model)
summary(SimData)
run(SimData)

# With Misspecification Model
n01 <- rnorm.object(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- sym.matrix.object(error.cor.Mis, "n01")
CFA.Model.Mis <- misspecified.CFA.object(TD=TD.Mis)
SimData <- data.object(200, CFA.Model, simMisspecifiedSet=CFA.Model.Mis)
summary(SimData)
run(SimData)



cleanEx()
nameEx("find.cutoff")
### * find.cutoff

flush(stderr()); flush(stdout())

### Name: find.cutoff
### Title: Find cutoff given a priori alpha level
### Aliases: find.cutoff find.cutoff-methods find.cutoff,data.frame-method
###   find.cutoff,matrix-method find.cutoff,simResult-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(200, CFA.Model)
SimModel <- model.object(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- result.object(SimData, SimModel, 50)
find.cutoff(Output, 0.05)



cleanEx()
nameEx("find.power")
### * find.power

flush(stderr()); flush(stdout())

### Name: find.power
### Title: Find power in rejecting alternative models based on fit indices
###   criteria
### Aliases: find.power find.power-methods find.power,data.frame-method
###   find.power,matrix-method find.power,simResult-method

### ** Examples

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- matrix.object(loading.null, 0.7)
PH.NULL <- sym.matrix.object(diag(1))
TD <- sym.matrix.object(diag(6))
CFA.Model.NULL <- matrix.CFA.object(LY = LX.NULL, PS = PH.NULL, TE = TD)
SimData.NULL <- data.object(500, CFA.Model.NULL)
SimModel <- model.object(CFA.Model.NULL)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output.NULL <- result.object(SimData.NULL, SimModel, 50)
Cut.NULL <- find.cutoff(Output.NULL, 0.95)

u79 <- runif.object(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- matrix.object(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
PH.ALT <- sym.matrix.object(latent.cor.alt, "u79")
CFA.Model.ALT <- matrix.CFA.object(LY = LX.ALT, PS = PH.ALT, TE = TD)
SimData.ALT <- data.object(500, CFA.Model.ALT)
Output.ALT <- result.object(SimData.ALT, SimModel, 50)
find.power(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
find.power(Output.ALT, Rule.of.thumb, used.fit=c("RMSEA", "CFI", "TLI", "SRMR"))



cleanEx()
nameEx("loading.from.alpha")
### * loading.from.alpha

flush(stderr()); flush(stdout())

### Name: loading.from.alpha
### Title: Find standardized factor loading from coefficient alpha
### Aliases: loading.from.alpha

### ** Examples

    loading.from.alpha(0.8, 4)



cleanEx()
nameEx("matrix.CFA.object")
### * matrix.CFA.object

flush(stderr()); flush(stdout())

### Name: matrix.CFA.object
### Title: Create a set of matrix that belongs to CFA model.
### Aliases: matrix.CFA.object

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)

CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)



cleanEx()
nameEx("matrix.Path.object")
### * matrix.Path.object

flush(stderr()); flush(stdout())

### Name: matrix.Path.object
### Title: Create a set of matrix belongs to Path analysis model
### Aliases: matrix.Path.object

### ** Examples
 
u35 <- runif.object(0.3, 0.5)
u57 <- runif.object(0.5, 0.7)
u1 <- runif.object(-0.1, 0.1)
n31 <- rnorm.object(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- matrix.object(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
PS <- sym.matrix.object(residual.error, "n31")

Path.Model <- matrix.Path.object(PS = PS, BE = BE)

u35 <- runif.object(0.3, 0.5)
u57 <- runif.object(0.5, 0.7)
u1 <- runif.object(-0.1, 0.1)
n31 <- rnorm.object(0.3, 0.1)

path.GA <- matrix(0, 2, 2)
path.GA[1, 1:2] <- NA
GA <- matrix.object(path.GA, "u35")

path.BE <- matrix(0, 2, 2)
path.BE[2, 1] <- NA
BE <- matrix.object(path.BE, "u57")

exo.cor <- matrix(NA, 2, 2)
diag(exo.cor) <- 1
PH <- sym.matrix.object(exo.cor, "n31")

PS <- sym.matrix.object(diag(2))

Path.Exo.Model <- matrix.Path.object(PS = PS, BE = BE, PH = PH, GA = GA, exo=TRUE)



cleanEx()
nameEx("matrix.SEM.object")
### * matrix.SEM.object

flush(stderr()); flush(stdout())

### Name: matrix.SEM.object
### Title: Create a set of matrix belongs to SEM model
### Aliases: matrix.SEM.object

### ** Examples

u68 <- runif.object(0.6, 0.8)
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "u68"
LY <- matrix.object(loading, loading.start)

TE <- sym.matrix.object(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
PS <- sym.matrix.object(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "n65"
path.start[3, 2] <- "u35"
BE <- matrix.object(path, path.start)

SEM.model <- matrix.SEM.object(BE=BE, LY=LY, PS=PS, TE=TE)

loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
LX <- matrix.object(loading.X, 0.7)

loading.Y <- matrix(NA, 2, 1)
LY <- matrix.object(loading.Y, "u68")

TD <- sym.matrix.object(diag(6))

TE <- sym.matrix.object(diag(2))

factor.K.cor <- matrix(NA, 2, 2)
diag(factor.K.cor) <- 1
PH <- sym.matrix.object(factor.K.cor, 0.5)

PS <- sym.matrix.object(as.matrix(1))

path.GA <- matrix(NA, 1, 2)
path.GA.start <- matrix(c("n65", "u35"), ncol=2)
GA <- matrix.object(path.GA, path.GA.start)

BE <- matrix.object(as.matrix(0))

SEM.Exo.model <- matrix.SEM.object(GA=GA, BE=BE, LX=LX, LY=LY, PH=PH, PS=PS, TD=TD, TE=TE, exo=TRUE)



cleanEx()
nameEx("matrix.object")
### * matrix.object

flush(stderr()); flush(stdout())

### Name: matrix.object
### Title: Create matrix object that save free parameters and starting
###   values, as well as fixed values
### Aliases: matrix.object

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
summary(LX)
run(LX)

n65 <- rnorm.object(0.6, 0.05)
LY <- matrix.object(loading, "n65")
summary(LY)
run(LY)



cleanEx()
nameEx("misspecified.CFA.object")
### * misspecified.CFA.object

flush(stderr()); flush(stdout())

### Name: misspecified.CFA.object
### Title: Set of model misspecification for CFA model.
### Aliases: misspecified.CFA.object

### ** Examples

n01 <- rnorm.object(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- sym.matrix.object(error.cor.Mis, "n01")
CFA.Model.Mis <- misspecified.CFA.object(TD=TD.Mis)



cleanEx()
nameEx("misspecified.Path.object")
### * misspecified.Path.object

flush(stderr()); flush(stdout())

### Name: misspecified.Path.object
### Title: Set of model misspecification for Path analysis model.
### Aliases: misspecified.Path.object

### ** Examples

u1 <- runif.object(-0.1, 0.1)
mis.path.GA <- matrix(0, 2, 2)
mis.path.GA[2, 1:2] <- NA
mis.GA <- matrix.object(mis.path.GA, "u1")
Path.Mis.Model <- misspecified.Path.object(GA = mis.GA, exo=TRUE)



cleanEx()
nameEx("misspecified.SEM.object")
### * misspecified.SEM.object

flush(stderr()); flush(stdout())

### Name: misspecified.SEM.object
### Title: Set of model misspecification for SEM model.
### Aliases: misspecified.SEM.object

### ** Examples

u2 <- runif.object(-0.2, 0.2)
n1 <- rnorm.object(0, 0.1)
loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X.trivial)] <- 0
LX.trivial <- matrix.object(loading.X.trivial, "u2")
error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
TD.trivial <- sym.matrix.object(error.cor.X.trivial, "n1")
error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
TE.trivial <- sym.matrix.object(error.cor.Y.trivial, "n1")
TH.trivial <- matrix.object(matrix(NA, 6, 2), "n1")
SEM.Mis.Model <- misspecified.SEM.object(LX = LX.trivial, TE = TE.trivial, TD = TD.trivial, TH = TH.trivial, exo=TRUE)



cleanEx()
nameEx("model.object")
### * model.object

flush(stderr()); flush(stdout())

### Name: model.object
### Title: Create model object from model specification and be ready for
###   data analysis.
### Aliases: model.object model.object-methods model.object,ANY-method
###   model.object,simMatrixSet-method model.object,freeParamSet-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
SimModel <- model.object(CFA.Model)



cleanEx()
nameEx("plot.cutoff")
### * plot.cutoff

flush(stderr()); flush(stdout())

### Name: plot.cutoff
### Title: Plot sampling distributions of fit indices
### Aliases: plot.cutoff plot.cutoff-methods plot.cutoff,data.frame-method
###   plot.cutoff,simResult-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(200, CFA.Model)
SimModel <- model.object(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- result.object(SimData, SimModel, 50)
plot.cutoff(Output, 0.05, used.fit=c("RMSEA", "SRMR", "CFI", "TLI"))



cleanEx()
nameEx("plot.power")
### * plot.power

flush(stderr()); flush(stdout())

### Name: plot.power
### Title: Plot sampling distributions of fit indices that visualize power
### Aliases: plot.power plot.power-methods
###   plot.power,data.frame,data.frame-method
###   plot.power,data.frame,vector-method
###   plot.power,simResult,simResult-method
###   plot.power,simResult,vector-method

### ** Examples

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- matrix.object(loading.null, 0.7)
PH.NULL <- sym.matrix.object(diag(1))
TD <- sym.matrix.object(diag(6))
CFA.Model.NULL <- matrix.CFA.object(LY = LX.NULL, PS = PH.NULL, TE = TD)
SimData.NULL <- data.object(500, CFA.Model.NULL)
SimModel <- model.object(CFA.Model.NULL)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output.NULL <- result.object(SimData.NULL, SimModel, 50)
Cut.NULL <- find.cutoff(Output.NULL, 0.95)

u79 <- runif.object(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- matrix.object(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
PH.ALT <- sym.matrix.object(latent.cor.alt, "u79")
CFA.Model.ALT <- matrix.CFA.object(LY = LX.ALT, PS = PH.ALT, TE = TD)
SimData.ALT <- data.object(500, CFA.Model.ALT)
Output.ALT <- result.object(SimData.ALT, SimModel, 50)
find.power(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
plot.power(Output.ALT, Output.NULL, alpha=0.05, used.fit=c("RMSEA", "CFI", "TLI", "SRMR"))



cleanEx()
nameEx("result.object")
### * result.object

flush(stderr()); flush(stdout())

### Name: result.object
### Title: Create result object.
### Aliases: result.object

### ** Examples

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- matrix.object(loading, 0.7)
PH <- sym.matrix.object(diag(1))
TD <- sym.matrix.object(diag(6))
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(500, CFA.Model)
SimModel <- model.object(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- result.object(SimData, SimModel, 50)
#summary(Output)



cleanEx()
nameEx("rnorm.object")
### * rnorm.object

flush(stderr()); flush(stdout())

### Name: rnorm.object
### Title: Create random normal distribution object
### Aliases: rnorm.object

### ** Examples

    n02 <- rnorm.object(0, 0.2)
    run(n02)



cleanEx()
nameEx("run")
### * run

flush(stderr()); flush(stdout())

### Name: run
### Title: Run a particular object in 'simsem' package.
### Aliases: run run-methods run,ANY-method run,nullSimMatrix-method
###   run,nullSymMatrix-method run,nullSimVector-method
### Keywords: run

### ** Examples

n02 <- rnorm.object(0, 0.2)
run(n02)



cleanEx()
nameEx("runif.object")
### * runif.object

flush(stderr()); flush(stdout())

### Name: runif.object
### Title: Create random uniform distribution object
### Aliases: runif.object

### ** Examples

u1 <- runif.object(-0.1, 0.1)
run(u1)



cleanEx()
nameEx("simConstraint-class")
### * simConstraint-class

flush(stderr()); flush(stdout())

### Name: simConstraint-class
### Title: Class '"simConstraint"'
### Aliases: simConstraint-class summary,simConstraint-method
### Keywords: classes

### ** Examples

showClass("simConstraint")
constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading <- constraint.object(constraint1, constraint2, constraint3, Tag="SEM")
summary(equal.loading)



cleanEx()
nameEx("simData-class")
### * simData-class

flush(stderr()); flush(stdout())

### Name: simData-class
### Title: Class '"simData"'
### Aliases: simData-class run,simData-method summary,simData-method
### Keywords: classes

### ** Examples

showClass("simData")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(200, CFA.Model)
summary(SimData)
run(SimData)



cleanEx()
nameEx("simDist-class")
### * simDist-class

flush(stderr()); flush(stdout())

### Name: simDist-class
### Title: Class "simDist"
### Aliases: simDist-class
### Keywords: classes

### ** Examples

showClass("simDist")



cleanEx()
nameEx("simMatrix-class")
### * simMatrix-class

flush(stderr()); flush(stdout())

### Name: simMatrix-class
### Title: Class '"simMatrix"' (Random parameters matrix)
### Aliases: simMatrix-class run,simMatrix-method
###   summary.short,simMatrix-method summary,simMatrix-method
### Keywords: classes

### ** Examples

showClass("simMatrix")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
summary(LX)
run(LX)

n65 <- rnorm.object(0.6, 0.05)
LY <- matrix.object(loading, "n65")
summary(LY)
run(LY)

u34 <- runif.object(0.3, 0.4)
LY <- adjust.object(LY, "u34", c(2, 1))
summary(LY)
run(LY)
summary.short(LY)



cleanEx()
nameEx("simMatrixSet-class")
### * simMatrixSet-class

flush(stderr()); flush(stdout())

### Name: simMatrixSet-class
### Title: Class '"simMatrixSet"'
### Aliases: simMatrixSet-class run,simMatrixSet-method
###   summary,simMatrixSet-method

### ** Examples

showClass("simMatrixSet")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)

CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
summary(CFA.Model)
#run(CFA.Model)



cleanEx()
nameEx("simMisspecifiedSet-class")
### * simMisspecifiedSet-class

flush(stderr()); flush(stdout())

### Name: simMisspecifiedSet-class
### Title: Class '"simMisspecifiedSet"'
### Aliases: simMisspecifiedSet-class run,simMisspecifiedSet-method
### Keywords: classes

### ** Examples

showClass("simMisspecifiedSet")
n01 <- rnorm.object(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- sym.matrix.object(error.cor.Mis, "n01")
CFA.Model.Mis <- misspecified.CFA.object(TD=TD.Mis)



cleanEx()
nameEx("simModel-class")
### * simModel-class

flush(stderr()); flush(stdout())

### Name: simModel-class
### Title: Class '"simModel"'
### Aliases: simModel-class run,simModel-method
### Keywords: classes

### ** Examples

showClass("simModel")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
SimModel <- model.object(CFA.Model)
#summary(SimModel)



cleanEx()
nameEx("simResult-class")
### * simResult-class

flush(stderr()); flush(stdout())

### Name: simResult-class
### Title: Class '"simResult"'
### Aliases: simResult-class
### Keywords: classes

### ** Examples

showClass("simResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- matrix.object(loading, 0.7)
PH <- sym.matrix.object(diag(1))
TD <- sym.matrix.object(diag(6))
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(500, CFA.Model)
SimModel <- model.object(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- result.object(SimData, SimModel, 50)
#summary(Output)
find.cutoff(Output, 0.95)



cleanEx()
nameEx("simVector-class")
### * simVector-class

flush(stderr()); flush(stdout())

### Name: simVector-class
### Title: Class '"simVector"' (Random parameters vector)
### Aliases: simVector-class run,simVector-method
###   summary.short,simVector-method summary,simVector-method
### Keywords: classes

### ** Examples

showClass("simVector")

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- vector.object(factor.mean, factor.mean.starting)
run(AL)
summary(AL)
summary.short(AL)

n01 <- rnorm.object(0, 1)
AL <- adjust.object(AL, "n01", 2)
run(AL)
summary(AL)



cleanEx()
nameEx("summary-methods")
### * summary-methods

flush(stderr()); flush(stdout())

### Name: summary-methods
### Title: Methods for function 'summary' in package 'simsem'
### Aliases: summary,freeParamSet-method summary,labelsSet-method
###   summary,matrixSet-method summary,reducedMatrixSet-method

### ** Examples

u89 <- runif.object(0.8, 0.9)
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
LX <- matrix.object(loading, "u89")
summary(LX)



cleanEx()
nameEx("summary.short")
### * summary.short

flush(stderr()); flush(stdout())

### Name: summary.short
### Title: Provide short summary of an object.
### Aliases: summary.short summary.short-methods summary.short,ANY-method
###   summary.short,vector-method summary.short,matrix-method

### ** Examples

u89 <- runif.object(0.8, 0.9)
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
LX <- matrix.object(loading, "u89")
summary.short(LX)



cleanEx()
nameEx("sym.matrix.object")
### * sym.matrix.object

flush(stderr()); flush(stdout())

### Name: sym.matrix.object
### Title: Create symmetric matrix object that save free parameters and
###   starting values, as well as fixed values
### Aliases: sym.matrix.object

### ** Examples

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)

u46 <- runif.object(0.4, 0.6)
factor.cor <- matrix(NA, 4, 4)
diag(factor.cor) <- 1
factor.cor.start <- matrix("u46", 4, 4)
factor.cor.start[1, 2] <- factor.cor.start[2, 1] <- "0.5"
PS <- sym.matrix.object(factor.cor, factor.cor.start)



cleanEx()
nameEx("symMatrix-class")
### * symMatrix-class

flush(stderr()); flush(stdout())

### Name: symMatrix-class
### Title: Class '"symMatrix"' (Random parameters symmetric matrix)
### Aliases: symMatrix-class run,symMatrix-method summary,symMatrix-method
### Keywords: classes

### ** Examples

showClass("symMatrix")

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)

u46 <- runif.object(0.4, 0.6)
PH <- adjust.object(PH, "u46", c(3,2))
summary(PH)
summary.short(PH)
run(PH)



cleanEx()
nameEx("vector.object")
### * vector.object

flush(stderr()); flush(stdout())

### Name: vector.object
### Title: Create vector object that save free parameters and starting
###   values, as well as fixed values
### Aliases: vector.object

### ** Examples

factor.mean <- rep(NA, 4)
AL <- vector.object(factor.mean, 0)

n02 <- rnorm.object(0, 0.2)
factor.start <- rep("n02", 4)
KA <- vector.object(factor.mean, factor.start)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
