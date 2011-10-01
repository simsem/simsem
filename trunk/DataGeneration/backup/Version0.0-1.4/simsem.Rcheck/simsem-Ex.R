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
### Aliases: adjust.object

### ** Examples

#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#LX <- matrix.object(loading, 0.7)
#summary(LX)
#run(LX)

#u34 <- runif.object(0.3, 0.4)
#LX <- adjust.object(LX, "u34", c(2, 1))
#summary(LX)
#run(LX)

#LX <- adjust.object(LX, 0, c(2,1))
#LX <- adjust.object(LX, 0.5, c(2,2), FALSE)
#summary(LX)
#run(LX)

#factor.mean <- rep(NA, 2)
#factor.mean.starting <- c(5, 2)
#AL <- vector.object(factor.mean, factor.mean.starting)
#run(AL)
#summary(AL)

#n01 <- rnorm.object(0, 1)
#AL <- adjust.object(AL, "n01", 2)
#run(AL)
#summary(AL)



cleanEx()
nameEx("constant.vector")
### * constant.vector

flush(stderr()); flush(stdout())

### Name: constant.vector
### Title: Create constant 'simVector' (Internal)
### Aliases: constant.vector

### ** Examples

	#constant.vector(0, 4)



cleanEx()
nameEx("contain")
### * contain

flush(stderr()); flush(stdout())

### Name: contain
### Title: Check whether an element is in a vector (Internal)
### Aliases: contain

### ** Examples

	#contain(0, 1:3)
	#contain(1, 1:3)



cleanEx()
nameEx("create.free.parameters")
### * create.free.parameters

flush(stderr()); flush(stdout())

### Name: create.free.parameters
### Title: Create free parameters object from model specification
### Aliases: create.free.parameters

### ** Examples

#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)

#indicator.mean <- rep(NA, 6)
#MX <- vector.object(indicator.mean, 0)

#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD, MX = MX)
#free <- create.free.parameters(CFA.Model)



cleanEx()
nameEx("create.implied.MACS")
### * create.implied.MACS

flush(stderr()); flush(stdout())

### Name: create.implied.MACS
### Title: Create model implied Means and Covariance Matrix (MACS)
### Aliases: create.implied.MACS
### Keywords: ~kwd1 ~kwd2

### ** Examples


#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#summary(LX)

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)

#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
#CFA.Model.Param <- run(CFA.Model)
#create.implied.MACS(CFA.Model.Param)



cleanEx()
nameEx("find.OpenMx.values")
### * find.OpenMx.values

flush(stderr()); flush(stdout())

### Name: find.OpenMx.values
### Title: Rearrange starting values such that it is appropriate for OpenMx
###   matrix specification (Internal)
### Aliases: find.OpenMx.values

### ** Examples

#parameter <- c(NA, NA, 0, 0)
#starting.values <- c(2, 5, 0, 0)
#find.OpenMx.Values(parameter, starting.values)



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
nameEx("match.keyword")
### * match.keyword

flush(stderr()); flush(stdout())

### Name: match.keyword
### Title: Search for the keywords and check whether the specified text
###   match one in the vector (Internal)
### Aliases: match.keyword

### ** Examples

	#match.keyword("LY", c("LY", "Ly", "ly", "LX", "Lx", "lx"))



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

# Error Correlation Object
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
nameEx("model.object")
### * model.object

flush(stderr()); flush(stdout())

### Name: model.object
### Title: Create model object from model specification
### Aliases: model.object
### Keywords: ~kwd1 ~kwd2

### ** Examples

#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#summary(LX)

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)

#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)

#SimModel <- model.object(CFA.Model)



cleanEx()
nameEx("print.if.not.null")
### * print.if.not.null

flush(stderr()); flush(stdout())

### Name: print.if.not.null
### Title: Provide basic summary of each object if that object is not NULL
###   (Internal)
### Aliases: print.if.not.null

### ** Examples

#AL <- vector.object(rep(NA, 5), "0")
#print.if.not.null(AL, "Factor Mean")



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
### Title: Run a particular object in simsem package.
### Aliases: run
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
### Aliases: simMatrix-class adjust.object,simMatrix-method
###   run,simMatrix-method summary.short,simMatrix-method
###   summary,simMatrix-method
### Keywords: classes

### ** Examples

showClass("simMatrix")

#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#summary(LX)
#run(LX)

#n65 <- rnorm.object(0.6, 0.05)
#LY <- matrix.object(loading, "n65")
#summary(LY)
#run(LY)

#u34 <- runif.object(0.3, 0.4)
#LY <- adjust.object(LY, "u34", c(2, 1))
#summary(LY)
#run(LY)
#summary.short(LY)



cleanEx()
nameEx("simMatrixSet-class")
### * simMatrixSet-class

flush(stderr()); flush(stdout())

### Name: simMatrixSet-class
### Title: Class '"simMatrixSet"'
### Aliases: simMatrixSet-class summary,simMatrixSet-method

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
nameEx("simVector-class")
### * simVector-class

flush(stderr()); flush(stdout())

### Name: simVector-class
### Title: Class '"simVector"' (Random parameters vector)
### Aliases: simVector-class adjust.object,simVector-method
###   run,simVector-method summary.short,simVector-method
###   summary,simVector-method
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
nameEx("starting.values")
### * starting.values

flush(stderr()); flush(stdout())

### Name: starting.values
### Title: Find starting values of free parameters (Internal)
### Aliases: starting.values
### Keywords: ~kwd1 ~kwd2

### ** Examples

#u89 <- runif.object(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- matrix.object(loading, "u89")

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)

#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
#starting.values(LX, 10)
#result <- starting.values(CFA.Model, 10)
#summary(result)



cleanEx()
nameEx("summary.short")
### * summary.short

flush(stderr()); flush(stdout())

### Name: summary.short
### Title: Provide short summary of an object.
### Aliases: summary.short

### ** Examples

#u89 <- runif.object(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- matrix.object(loading, "u89")
#summary.short(LX)



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
### Aliases: symMatrix-class count.random.object,symMatrix-method
###   run,symMatrix-method summary,symMatrix-method
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
