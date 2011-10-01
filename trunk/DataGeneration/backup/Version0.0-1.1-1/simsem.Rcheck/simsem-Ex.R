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
nameEx("loading.from.alpha")
### * loading.from.alpha

flush(stderr()); flush(stdout())

### Name: loading.from.alpha
### Title: Find standardized factor loading from coefficient alpha
### Aliases: loading.from.alpha

### ** Examples

    loading.from.alpha(0.8, 4)



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
