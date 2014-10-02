
test.estmodel <- function() {
    loading <- matrix(0, 6, 2)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    cfa1 <- estmodel(LY = loading, ngroups = 1, modelType = "CFA")
    cfa2 <- estmodel(LY = loading, ngroups = 2, modelType = "CFA")
    
    loading <- matrix(0, 6, 2)
    loading[1:3, 1] <- c(1, "con1", "con2")
    loading[4:6, 2] <- c(1, "con3", "con4")
    latent <- matrix(NA, 2, 2)
    intcept <- paste0("int", 1:6)
    facmean <- rep(0, 2)
    error <- diag(NA, 6)
    cfa3 <- estmodel(LY = loading, PS = list(latent, latent), TE = error, AL = facmean, 
        TY = intcept, ngroups = 2, modelType = "cfa")
    
    
    path <- matrix(0, 4, 4)
    path[3, 1:2] <- NA
    path[4, 3] <- NA
    path1 <- estmodel(BE = path, ngroups = 1, modelType = "path")
    path2 <- estmodel(BE = path, ngroups = 2, modelType = "path")
    
    path <- matrix(0, 4, 4)
    path[3, 1:2] <- c("con1", "con2")
    path[4, 3] <- "con3"
    faccov <- diag(NA, 4)
    faccov[2, 1] <- faccov[1, 2] <- NA
    facmean <- rep(NA, 4)
    path3 <- estmodel(BE = path, PS = list(faccov, faccov), AL = facmean, ngroups = 2, 
        modelType = "path")
    
    path <- matrix(0, 4, 4)
    path[3, 1:2] <- NA
    path[4, 3] <- NA
    loading <- matrix(0, 12, 4)
    loading[1:3, 1] <- NA
    loading[4:6, 2] <- NA
    loading[7:9, 3] <- NA
    loading[10:12, 4] <- NA
    sem1 <- estmodel(LY = loading, BE = path, ngroups = 1, modelType = "sem")
    sem2 <- estmodel(LY = loading, BE = path, ngroups = 2, modelType = "sem")
    
    path <- matrix(0, 4, 4)
    path[3, 1:2] <- NA
    path[4, 3] <- NA
    loading <- matrix(0, 12, 4)
    loading[1:3, 1] <- paste0("con", 1:3)
    loading[4:6, 2] <- paste0("con", 4:6)
    loading[7:9, 3] <- paste0("con", 7:9)
    loading[10:12, 4] <- paste0("con", 10:12)
    faccov <- diag(1, 4)
    faccov[2, 1] <- faccov[1, 2] <- NA
    intcept <- paste0("int", 1:12)
    facmean <- rep(0, 4)
    error <- diag(NA, 12)
    sem3 <- estmodel(LY = loading, BE = path, PS = faccov, TY = intcept, AL = facmean, 
        TE = error, ngroups = 2, modelType = "sem")
    
}

test.model.lavaan <- function() {
    
    HS.model <- " visual  =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed   =~ x7 + x8 + x9 "
    
    dat <- data.frame(lavaan::HolzingerSwineford1939, z = rnorm(nrow(lavaan::HolzingerSwineford1939), 
        0, 1))
    
    fit <- lavaan::cfa(HS.model, data = dat)
    dat2 <- generate(model.lavaan(fit), n = 200)
    dat2 <- generate(model.lavaan(fit, std = TRUE), n = 200)
    
    fitgroup <- lavaan::cfa(HS.model, data = dat, group = "school")
    dat2 <- generate(model.lavaan(fitgroup), n = 200)
    dat2 <- generate(model.lavaan(fitgroup, std = TRUE), n = 200)
    
    mod <- " x5 ~ x4\nx4 ~ x3\nx3 ~ x1 + x2"
    
    fitpath <- lavaan::sem(mod, data = dat)
    dat2 <- generate(model.lavaan(fitpath), n = 200)
    dat2 <- generate(model.lavaan(fitpath, std = TRUE), n = 200)
    
    
    dat2 <- data.frame(lavaan::PoliticalDemocracy, z = rnorm(nrow(lavaan::PoliticalDemocracy), 0, 
        1))
    model1 <- "\nind60 =~ x1 + x2 + x3\ndem60 =~ y1 + a*y2 + b*y3 + c*y4\ndem65 =~ y5 + a*y6 + b*y7 + c*y8\ndem60 ~ ind60\ndem65 ~ ind60 + dem60\ny1 ~~ y5\ny2 ~~ y4 + y6\ny3 ~~ y7\ny4 ~~ y8\ny6 ~~ y8\n"
    fitsem <- lavaan::sem(model1, data = dat2, meanstructure = TRUE)
    dat3 <- generate(model.lavaan(fitsem), n = 200)
    dat3 <- generate(model.lavaan(fitsem, std = TRUE), n = 200)
} 

test.covData <- function() {

# CFA model with one covariate
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- "con1"
loading[4:6, 2] <- "con2"
loading[7:9, 3] <- "con3"
loadingVal <- matrix(0, 9, 3)
loadingVal[1:3, 1] <- 0.9
loadingVal[4:6, 2] <- 0.8
loadingVal[7:9, 3] <- 0.7
LY <- bind(loading, loadingVal)

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(9))

VTE <- bind(rep(NA, 9), 0.51)

gamma <- matrix(NA, 3, 1)
GA <- bind(gamma, 0.3)

KA <- bind(matrix(0, 9, 1), misspec = matrix("runif(1, -0.2, 0.2)", 9, 1))

con <- "abc := con1 * con2
con1 == con2"
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, VTE=VTE, GA=GA, KA = KA, modelType = "CFA", indLab=paste0("x", 1:9), facLab=c("visual", "textual", "speed"), covLab = "sex", con=con)
sex <- data.frame(sex = rep(c(0, 1), each=100))
param <- draw(CFA.Model, covData=sex)
out <- analyze(CFA.Model, generate(CFA.Model, n=200, covData=sex, params=TRUE))
Output <- sim(10, n=200, CFA.Model, covData=sex)

# Path analysis model with two covariates

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1,0.5,0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1,-0.1,0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1,0.3,0.1)")

ME <- bind(rep(NA, 4), 0)

gamma <- matrix(NA, 4, 2)
gammaVal <- matrix(rep(c(0.1, -0.05), each=4), 4, 2)
GA <- bind(gamma, gammaVal)

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, GA = GA, modelType="Path")

covData <- data.frame(z1 = c(rep(1, 100), rep(0, 100), rep(0, 100)), z2 = c(rep(0, 100), rep(1, 100), rep(0, 100)))
param <- draw(Path.Model, misfitBounds = c(0.03, 0.05), misfitType="rmsea", covData=covData)
dat <- createData(param[[1]], n = 300, covData=covData)
out <- analyze(Path.Model, dat)

Output <- sim(20, n=300, Path.Model, covData=covData)

# SEM model with two covariates

loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- "con1"
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
loading.trivial <- matrix("runif(1, -0.2, 0.2)", 8, 3)
loading.trivial[is.na(loading)] <- 0
LY <- bind(loading, loading.start, misspec=loading.trivial)

error.cor.trivial <- matrix("rnorm(1, 0, 0.1)", 8, 8)
diag(error.cor.trivial) <- 1
RTE <- binds(diag(8), misspec=error.cor.trivial)

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- binds(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"
BE <- bind(path, path.start)

gamma <- matrix(NA, 3, 2)
gammaVal <- matrix(rep(c(0.1, -0.05), each=3), 3, 2)
GA <- bind(gamma, gammaVal)

SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, GA=GA, modelType="SEM")

covData <- data.frame(z1 = c(rep(1, 100), rep(0, 100), rep(0, 100)), z2 = c(rep(0, 100), rep(1, 100), rep(0, 100)))

dat <- generate(SEM.model, n=300, covData=covData)
out <- analyze(SEM.model, dat)

Output <- sim(20, n=300, SEM.model, covData=covData, silent=TRUE) 

# model.lavaan with CFA model and one covariate

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
			  textual ~ sex
			  visual ~~ textual
			  visual ~~ speed
			  textual ~~ speed
			  x1 ~ sex
			  '

object <- lavaan::cfa(HS.model, data=lavaan::HolzingerSwineford1939, meanstructure=TRUE)
model.lavaan(object)
model.lavaan(object, std=TRUE)

# model.lavaan with path model and one covariate

HS.model2 <- ' x3 ~ x2 + x1
				x2 ~ x1
				x2 ~ sex
				x1 ~ sex
			  '

object2 <- lavaan::sem(HS.model2, data=lavaan::HolzingerSwineford1939)
model.lavaan(object2)
model.lavaan(object2, std=TRUE)

# model.lavaan with SEM model and one covariate

HS.model3 <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
			  textual ~ sex
			  visual ~~ textual
			  visual ~~ speed
			  textual ~~ speed
			  x1 ~ sex
			  '

object3 <- lavaan::cfa(HS.model, data=lavaan::HolzingerSwineford1939, meanstructure=TRUE, group="school", group.equal="loadings")
mod <- model.lavaan(object3) # Still have a problem
sexgroup <- data.frame(sex, school=rep(c(1,2,1,2), each=50))
generate(mod, list(100, 100), covData=sexgroup)
Output <- sim(10, n=list(100, 100), model=mod, covData=sexgroup)
}
