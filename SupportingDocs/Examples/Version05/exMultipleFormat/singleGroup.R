library(simsem)
library(OpenMx)


totalRep <- 1000
# 1. Generate Data by simsem model template

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cov <- matrix(NA, 2, 2)
diag(latent.cov) <- 1
PS <- binds(latent.cov, 0.5)

TE <- binds(diag(NA, 6), 0.51)

CFA.Model <- model.cfa(LY = LY, PS = PS, TE = TE)

# 1.1 Analyze by simsem model template

Output11 <- sim(nRep = totalRep, model = CFA.Model, n = 200, generate = CFA.Model)
summary(Output11)

# 1.2 Analyze by lavaan

script <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
"

Output12 <- sim(nRep = totalRep, model = script, n = 200, generate = CFA.Model, std.lv = TRUE, lavaanfun = "cfa")
summary(Output12)

# 1.3 Analyze by list of lavaan arguments

Output13 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE), n = 200, generate = CFA.Model, lavaanfun = "cfa")
summary(Output13)

# 1.4 Analyze by OpenMx object

mxTwoFactorModel <- mxModel("Two Factor Model",
    type="RAM",
    # asymmetric paths
    mxMatrix(
        type="Full",
        nrow=8,
        ncol=8,
        values=c(0,0,0,0,0,0,0.7,0,
                 0,0,0,0,0,0,0.7,0,
                 0,0,0,0,0,0,0.7,0,
                 0,0,0,0,0,0,0,0.7,
                 0,0,0,0,0,0,0,0.7,
                 0,0,0,0,0,0,0,0.7,
                 0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0),
        free=c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
        labels=c(NA,NA,NA,NA,NA,NA,"l1", NA,
                 NA,NA,NA,NA,NA,NA,"l2", NA,
                 NA,NA,NA,NA,NA,NA,"l3", NA,
                 NA,NA,NA,NA,NA,NA, NA,"l4",
                 NA,NA,NA,NA,NA,NA, NA,"l5",
                 NA,NA,NA,NA,NA,NA, NA,"l6",
                 NA,NA,NA,NA,NA,NA, NA, NA,
                 NA,NA,NA,NA,NA,NA, NA, NA),
        byrow=TRUE,
        name="A"
    ),
    # symmetric paths
    mxMatrix(
        type="Symm",
        nrow=8,
        ncol=8,
        values=c(0.51,0,0,0,0,0, 0, 0,
                 0,0.51,0,0,0,0, 0, 0,
                 0,0,0.51,0,0,0, 0, 0,
                 0,0,0,0.51,0,0, 0, 0,
                 0,0,0,0,0.51,0, 0, 0,
                 0,0,0,0,0,0.51, 0, 0,
                 0,0,0,0,0,0, 1,.5,
                 0,0,0,0,0,0,.5, 1),
        free=c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
               FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
               FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
               FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
               FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
               FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
        labels=c("e1", NA,   NA,   NA,   NA,   NA,    NA,    NA,
                 NA, "e2",   NA,   NA,   NA,   NA,    NA,    NA,
                 NA,   NA, "e3",   NA,   NA,   NA,    NA,    NA,
                 NA,   NA,   NA, "e4",   NA,   NA,    NA,    NA,
                 NA,   NA,   NA,   NA, "e5",   NA,    NA,    NA,
                 NA,   NA,   NA,   NA,   NA, "e6",    NA,    NA,
                 NA,   NA,   NA,   NA,   NA,   NA, "varF1", "cov",
                 NA,   NA,   NA,   NA,   NA,   NA, "cov", "varF2"),
        byrow=TRUE,
        name="S"
    ),
    # filter matrix
    mxMatrix(
        type="Full",
        nrow=6,
        ncol=8,
        free=FALSE,
        values=c(1,0,0,0,0,0,0,0,
                 0,1,0,0,0,0,0,0,
                 0,0,1,0,0,0,0,0,
                 0,0,0,1,0,0,0,0,
                 0,0,0,0,1,0,0,0,
                 0,0,0,0,0,1,0,0),
        byrow=TRUE,
        name="F"
    ),
    # means
    mxMatrix(
        type="Full",
        nrow=1,
        ncol=8,
        values=c(0,0,0,0,0,0,0,0),
        free=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE),
        labels=c("meanx1","meanx2","meanx3",
                 "meanx4","meanx5","meanx6",
                  NA,NA),
        name="M"
    ),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2")),
	mxFitFunctionML()
)

Output14 <- sim(nRep = totalRep, model = mxTwoFactorModel, n = 200, generate = CFA.Model)
summary(Output14)

# 1.5 Analyze by functions using lava

FUN <- function(data) {
	library(lava)
	m1 <- lava::lvm()
	latent(m1) <- ~f1 + f2
	regression(m1) <- c(y1, y2, y3) ~ f1
	regression(m1) <- c(y4, y5, y6) ~ f2
	covariance(m1) <- f1 ~ f2
	covariance(m1, ~f1 + f2) <- 1
	intercept(m1, ~f1 + f2) <- 0
	e <- lava::estimate(m1, data)
	coef <- coef(e)
	se <- sqrt(diag(vcov(e)))
	f <- gof(e)
	fit <- c(f$fit$statistic, f$fit$parameter, f$fit$p.value, loglik = as.numeric(f$logLik), bic = f$BIC, aic = f$AIC, rmsea = as.numeric(f$RMSEA[1]))
	converged <- !any(is.na(se))
	ci <- confint(e)
	cilower <- ci[,1]
	ciupper <- ci[,2]
	extra <- logLik(e)
	detach(package:lava) # Detach is used because the sim method from the lava package interrupting the sim function in the simsem
	list(coef = coef, se = se, fit = fit, converged = converged, cilower = cilower, ciupper = ciupper, extra = extra)
}

Output15 <- sim(nRep = totalRep, model = FUN, n = 200, generate = CFA.Model)
summary(Output15)

# 2. Provide a list of data

library(MASS)

modelImplied <- matrix(
	c(1, 0.49, 0.49, 0.245, 0.245, 0.245,
	0.49, 1, 0.49, 0.245, 0.245, 0.245,
	0.49, 0.49, 1, 0.245, 0.245, 0.245,
	0.245, 0.245, 0.245, 1, 0.49, 0.49, 
	0.245, 0.245, 0.245, 0.49, 1, 0.49, 
	0.245, 0.245, 0.245, 0.49, 0.49, 1), 6, 6, byrow=TRUE)

data.l <- list()
for(i in 1:totalRep) {
	scores <- mvrnorm(200, rep(0, 6), modelImplied)
	colnames(scores) <- paste0("y", 1:6)
	data.l[[i]] <- data.frame(scores)
}

# 2.1 Analyze by simsem model template

Output21 <- sim(model = CFA.Model, rawData = data.l)
summary(Output21)

# 2.2 Analyze by lavaan (To be fixed)

Output22 <- sim(model = script, rawData = data.l, std.lv = TRUE, lavaanfun = "cfa")
summary(Output22)

# 2.3 Analyze by list of lavaan arguments (To be fixed)

Output23 <- sim(model = list(model = script, std.lv = TRUE), rawData = data.l, lavaanfun = "cfa")
summary(Output23)

# 2.4 Analyze by OpenMx object

Output24 <- sim(model = mxTwoFactorModel, rawData = data.l)
summary(Output24)

# 2.5 Analyze by functions using lava

Output25 <- sim(model = FUN, rawData = data.l)
summary(Output25)

# 3. Provide a population data

scores <- mvrnorm(10000, rep(0, 6), modelImplied)
colnames(scores) <- paste0("y", 1:6)
popData <- data.frame(scores)

fit <- cfa(model=script, data=popData, std.lv=TRUE)
summary(fit)

# 3.1 Analyze by simsem model template (to be fixed)

Output31 <- sim(nRep = totalRep, model = CFA.Model, n = 200, rawData = popData)
summary(Output31)

# 3.2 Analyze by lavaan (Something is so wrong)

Output32 <- sim(nRep = totalRep, model = script, n = 200, rawData = popData, std.lv = TRUE, lavaanfun = "cfa")
summary(Output32)

# 3.3 Analyze by list of lavaan arguments (Something is so wrong)

Output33 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE), n = 200, rawData = popData, lavaanfun = "cfa")
summary(Output33)

# 3.4 Analyze by OpenMx object

Output34 <- sim(nRep = totalRep, model = mxTwoFactorModel, n = 200, rawData = popData)
summary(Output34)

# 3.5 Analyze by functions using lava

Output35 <- sim(nRep = totalRep, model = FUN, n = 200, rawData = popData)
summary(Output35)

# 4. lavaan script for simulate data

genscript <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3
f2 =~ 0.7*y4 + 0.7*y5 + 0.7*y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.51*y1
y2 ~~ 0.51*y2
y3 ~~ 0.51*y3
y4 ~~ 0.51*y4
y5 ~~ 0.51*y5
y6 ~~ 0.51*y6
"

# 4.1 Analyze by simsem model template

Output41 <- sim(nRep = totalRep, model = CFA.Model, n = 200, generate = genscript)
summary(Output41)

# 4.2 Analyze by lavaan

Output42 <- sim(nRep = totalRep, model = script, n = 200, generate = genscript, std.lv = TRUE, lavaanfun = "cfa")
summary(Output42)

# 4.3 Analyze by list of lavaan arguments

Output43 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE), n = 200, generate = genscript, lavaanfun = "cfa")
summary(Output43)

# 4.4 Analyze by OpenMx object

Output44 <- sim(nRep = totalRep, model = mxTwoFactorModel, n = 200, generate = genscript)
summary(Output44)

# 4.5 Analyze by functions using lava

Output45 <- sim(nRep = totalRep, model = FUN, n = 200, generate = genscript)
summary(Output45)

# 5. OpenMx Model

# The data generation and data analysis are the same models (since the parameter values are fixed as starting values)

# 5.1 Analyze by simsem model template

Output51 <- sim(nRep = totalRep, model = CFA.Model, n = 200, generate = mxTwoFactorModel)
summary(Output51)

# 5.2 Analyze by lavaan

Output52 <- sim(nRep = totalRep, model = script, n = 200, generate = mxTwoFactorModel, std.lv = TRUE, lavaanfun = "cfa")
summary(Output52)

# 5.3 Analyze by list of lavaan arguments

Output53 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE), n = 200, generate = mxTwoFactorModel, lavaanfun = "cfa")
summary(Output53)

# 5.4 Analyze by OpenMx object

Output54 <- sim(nRep = totalRep, model = mxTwoFactorModel, n = 200, generate = mxTwoFactorModel)
summary(Output54)

# 5.5 Analyze by functions using lava

Output55 <- sim(nRep = totalRep, model = FUN, n = 200, generate = mxTwoFactorModel)
summary(Output55)

# 6. Data generation by functions

datfun <- function(n) {
	modelImplied <- matrix(
		c(1, 0.49, 0.49, 0.245, 0.245, 0.245,
		0.49, 1, 0.49, 0.245, 0.245, 0.245,
		0.49, 0.49, 1, 0.245, 0.245, 0.245,
		0.245, 0.245, 0.245, 1, 0.49, 0.49, 
		0.245, 0.245, 0.245, 0.49, 1, 0.49, 
		0.245, 0.245, 0.245, 0.49, 0.49, 1), 6, 6, byrow=TRUE)
	scores <- mvrnorm(n, rep(0, 6), modelImplied)
	colnames(scores) <- paste0("y", 1:6)
	scores <- data.frame(scores)
	attr(scores, "param") <- c("load1" = 0.7, "load2" = 0.7, "load3" = 0.7)
	attr(scores, "stdparam") <- c("load1" = 0.7, "load2" = 0.7, "load3" = 0.7)
	scores
}

# 6.1 Analyze by simsem model template

Output61 <- sim(nRep = totalRep, model = CFA.Model, n = 200, generate = datfun)
summary(Output61)

# 6.2 Analyze by lavaan

Output62 <- sim(nRep = totalRep, model = script, n = 200, generate = datfun, std.lv = TRUE, lavaanfun = "cfa")
summary(Output62)

# 6.3 Analyze by list of lavaan arguments

Output63 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE), n = 200, generate = datfun, lavaanfun = "cfa")
summary(Output63)

# 6.4 Analyze by OpenMx object

Output64 <- sim(nRep = totalRep, model = mxTwoFactorModel, n = 200, generate = datfun)
summary(Output64)

# 6.5 Analyze by functions using lava

Output65 <- sim(nRep = totalRep, model = FUN, n = 200, generate = datfun)
summary(Output65)
