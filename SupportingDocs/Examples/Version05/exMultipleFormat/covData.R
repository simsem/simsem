library(simsem)

# Single group model with covariate effect at both factors and indicators

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
Output <- sim(1000, n=200, CFA.Model, covData=sex)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)

# Single group model with covariate effect at both factors and indicators using lavaan object

library(lavaan)

HS.model <- ' visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
textual ~ sex
visual ~~ textual
visual ~~ speed
textual ~~ speed
x1 ~ sex
'

object <- cfa(HS.model, data=HolzingerSwineford1939, meanstructure=TRUE)
template <- model.lavaan(object)
template2 <- model.lavaan(object, std=TRUE)
Output2 <- sim(1000, n=200, template, covData=sex)
getCutoff(Output2, 0.05)
plotCutoff(Output2, 0.05)
summaryParam(Output2)

# Single group model with covariate effect at both factors and indicators using lavaan object

HS.model2 <- ' x3 ~ x2 + x1
x2 ~ x1
x2 ~ sex
x1 ~ sex
'

object2 <- sem(HS.model2, data=HolzingerSwineford1939)
template3 <- model.lavaan(object2)
template4 <- model.lavaan(object2, std=TRUE)
Output3 <- sim(1000, n=200, template4, covData=sex)
getCutoff(Output3, 0.05)
plotCutoff(Output3, 0.05)
summaryParam(Output3)

# Multiple group model with covariate effect at both factors and indicators using lavaan object

HS.model3 <- ' visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
textual ~ sex
visual ~~ textual
visual ~~ speed
textual ~~ speed
x1 ~ sex
'

object3 <- cfa(HS.model, data=HolzingerSwineford1939, meanstructure=TRUE, group="school", group.equal="loadings")
mod <- model.lavaan(object3) # Still have a problem
sexgroup <- data.frame(sex, school=rep(c(1,2,1,2), each=50))
datexample <- generate(mod, list(100, 100), covData=sexgroup)
Output4 <- sim(1000, n=list(100, 100), model=mod, covData=sexgroup)
getCutoff(Output4, 0.05)
plotCutoff(Output4, 0.05)
summaryParam(Output4)

# Path analysis model with multiple covariates

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
param <- draw(Path.Model, misfitBound = c(0.03, 0.05), misfitType="rmsea", covData=covData)
dat <- createData(param[[1]], n = 300, covData=covData)

Output5 <- sim(1000, n=300, Path.Model, covData=covData)
getCutoff(Output5, 0.05)
plotCutoff(Output5, 0.05)
summaryParam(Output5)

# SEM model with multiple covariates

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
try(out <- analyze(SEM.model, dat))

Output6 <- sim(1000, n=300, SEM.model, covData=covData, silent=TRUE)
getCutoff(Output6, 0.05)
plotCutoff(Output6, 0.05)
summary(Output6)
