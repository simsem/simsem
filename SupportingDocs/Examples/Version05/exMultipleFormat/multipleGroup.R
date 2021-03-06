library(simsem)
library(OpenMx)

totalRep <- 20
# 1. Generate Data by simsem model template

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cov <- matrix(NA, 2, 2)
diag(latent.cov) <- 1
PS <- binds(latent.cov, 0.5)

TE <- binds(diag(NA, 6), 0.51)

CFA.Model <- model.cfa(LY = LY, PS = PS, TE = TE, ngroups = 2)

# 1.1 Analyze by simsem model template

Output11 <- sim(nRep = totalRep, model = CFA.Model, n = list(150, 100), generate = CFA.Model)
summary(Output11)

# 1.2 Analyze by lavaan

script <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
"

Output12 <- sim(nRep = totalRep, model = script, n = list(150, 100), generate = CFA.Model, std.lv = TRUE, group = "group", lavaanfun = "cfa")
summary(Output12)

# 1.3 Analyze by list of lavaan arguments

Output13 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE, group = "group"), n = list(150, 100), generate = CFA.Model, lavaanfun = "cfa")
summary(Output13)

# 1.4 Analyze by OpenMx

mxTwoGroupModel <- mxModel("Two Group",
	mxModel("group1",
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
			labels=c(NA,NA,NA,NA,NA,NA,"l11", NA,
					 NA,NA,NA,NA,NA,NA,"l21", NA,
					 NA,NA,NA,NA,NA,NA,"l31", NA,
					 NA,NA,NA,NA,NA,NA, NA,"l41",
					 NA,NA,NA,NA,NA,NA, NA,"l51",
					 NA,NA,NA,NA,NA,NA, NA,"l61",
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
			labels=c("e11", NA,   NA,   NA,   NA,   NA,    NA,    NA,
					 NA, "e21",   NA,   NA,   NA,   NA,    NA,    NA,
					 NA,   NA, "e31",   NA,   NA,   NA,    NA,    NA,
					 NA,   NA,   NA, "e41",   NA,   NA,    NA,    NA,
					 NA,   NA,   NA,   NA, "e51",   NA,    NA,    NA,
					 NA,   NA,   NA,   NA,   NA, "e61",    NA,    NA,
					 NA,   NA,   NA,   NA,   NA,   NA, "varF11", "cov1",
					 NA,   NA,   NA,   NA,   NA,   NA, "cov1", "varF21"),
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
			labels=c("meanx11","meanx21","meanx31",
					 "meanx41","meanx51","meanx61",
					  NA,NA),
			name="M"
		),
		mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	), 
	mxModel("group2",
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
			labels=c(NA,NA,NA,NA,NA,NA,"l12", NA,
					 NA,NA,NA,NA,NA,NA,"l22", NA,
					 NA,NA,NA,NA,NA,NA,"l32", NA,
					 NA,NA,NA,NA,NA,NA, NA,"l42",
					 NA,NA,NA,NA,NA,NA, NA,"l52",
					 NA,NA,NA,NA,NA,NA, NA,"l62",
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
			labels=c("e12", NA,   NA,   NA,   NA,   NA,    NA,    NA,
					 NA, "e22",   NA,   NA,   NA,   NA,    NA,    NA,
					 NA,   NA, "e32",   NA,   NA,   NA,    NA,    NA,
					 NA,   NA,   NA, "e42",   NA,   NA,    NA,    NA,
					 NA,   NA,   NA,   NA, "e52",   NA,    NA,    NA,
					 NA,   NA,   NA,   NA,   NA, "e62",    NA,    NA,
					 NA,   NA,   NA,   NA,   NA,   NA, "varF12", "cov2",
					 NA,   NA,   NA,   NA,   NA,   NA, "cov2", "varF22"),
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
			labels=c("meanx12","meanx22","meanx32",
					 "meanx42","meanx52","meanx62",
					  NA,NA),
			name="M"
		),
		mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
	), 
	mxAlgebra(
        group1.objective + group2.objective,
        name="minus2loglikelihood"
    ),
    mxFitFunctionAlgebra("minus2loglikelihood")
)

Output14 <- sim(nRep = totalRep, model = mxTwoGroupModel, n = list(150, 100), generate = CFA.Model, group = "group")
summary(Output14)

# 1.5 Analyze by function using lava

FUN <- function(data) {
	library(lava)
	m1 <- lvm()
	latent(m1) <- ~f1 + f2
	regression(m1) <- c(y1, y2, y3) ~ f1
	regression(m1) <- c(y4, y5, y6) ~ f2
	covariance(m1) <- f1 ~ f2
	covariance(m1, ~f1 + f2) <- 1
	intercept(m1, ~f1 + f2) <- 0
	m2 <- m1
	dataGroup <- split(data, data$group)
	e <- estimate(list(`Group 1` = m1, `Group 2` = m2), dataGroup) # No constraints across groups
	coef <- coef(e)
	se <- sqrt(diag(vcov(e)))
	f <- gof(e)
	fit <- c(f$fit$statistic, f$fit$parameter, f$fit$p.value, loglik = as.numeric(f$logLik), bic = f$BIC, aic = f$AIC, rmsea = as.numeric(f$RMSEA[1]))
	converged <- !any(is.na(se))
	detach(package:lava) # Detach is used because the sim method from the lava package interrupting the sim function in the simsem
	list(coef = coef, se = se, fit = fit, converged = converged)
}

Output15 <- sim(nRep = totalRep, model = FUN, n = list(150, 100), generate = CFA.Model, group = "group")
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
	scores1 <- cbind(mvrnorm(150, rep(0, 6), modelImplied), 1)
	scores2 <- cbind(mvrnorm(100, rep(0, 6), modelImplied), 2)
	scores <- rbind(scores1, scores2)
	colnames(scores) <- c(paste0("y", 1:6), "group")
	data.l[[i]] <- data.frame(scores)
}

# 2.1 Analyze by simsem model template

Output21 <- sim(model = CFA.Model, rawData = data.l)
summary(Output21)

# 2.2 Analyze by lavaan (To be fixed)

Output22 <- sim(model = script, rawData = data.l, std.lv = TRUE, lavaanfun = "cfa", group = "group")
summary(Output22)

# 2.3 Analyze by list of lavaan arguments (To be fixed)

Output23 <- sim(model = list(model = script, std.lv = TRUE, group = "group"), rawData = data.l, lavaanfun = "cfa")
summary(Output23)

# 2.4 Analyze by OpenMx

Output24 <- sim(model = mxTwoGroupModel, rawData = data.l)
summary(Output24)

# 2.5 Analyze by function using lava

Output25 <- sim(model = FUN, rawData = data.l)
summary(Output25)

# 3. Provide a population data

scores1 <- cbind(mvrnorm(15000, rep(0, 6), modelImplied), 1)
scores2 <- cbind(mvrnorm(10000, rep(0, 6), modelImplied), 2)
scores <- rbind(scores1, scores2)
colnames(scores) <- c(paste0("y", 1:6), "group")
popData <- data.frame(scores)
	
fit <- cfa(model=script, data=popData, std.lv=TRUE, group = "group")
summary(fit)

# 3.1 Analyze by simsem model template (to be fixed)

Output31 <- sim(nRep = totalRep, model = CFA.Model, n = list(150, 100), rawData = popData)
summary(Output31)

# 3.2 Analyze by lavaan (Something is so wrong)

Output32 <- sim(nRep = totalRep, model = script, n = list(150, 100), rawData = popData, std.lv = TRUE, lavaanfun = "cfa", group = "group")
summary(Output32)

# 3.3 Analyze by list of lavaan arguments (Something is so wrong)

Output33 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE, group = "group"), n = list(150, 100), rawData = popData, lavaanfun = "cfa")
summary(Output33)

# 3.4 Analyze by OpenMx

Output34 <- sim(nRep = totalRep, model = mxTwoGroupModel, n = list(150, 100), rawData = popData)
summary(Output34)

# 3.5 Analyze by function using lava

Output35 <- sim(nRep = totalRep, model = FUN, n = list(150, 100), rawData = popData)
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

Output41 <- sim(nRep = totalRep, model = CFA.Model, n = list(150, 100), generate = genscript)
summary(Output41)

# 4.2 Analyze by lavaan

Output42 <- sim(nRep = totalRep, model = script, n = list(150, 100), generate = genscript, std.lv = TRUE, lavaanfun = "cfa", group = "group")
summary(Output42)

# 4.3 Analyze by list of lavaan arguments

Output43 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE, group = "group"), n = list(150, 100), generate = genscript, lavaanfun = "cfa")
summary(Output43)
summaryParam(Output43, matchParam = TRUE)

# 4.4 Analyze by OpenMx

Output44 <- sim(nRep = totalRep, model = mxTwoGroupModel, n = list(150, 100), generate = genscript)
summary(Output44)

# 4.5 Analyze by function using lava

Output45 <- sim(nRep = totalRep, model = FUN, n = list(150, 100), generate = genscript)
summary(Output45)

# 5. OpenMx Model

# The data generation and data analysis are the same models (since the parameter values are fixed as starting values)

# 5.1 Analyze by simsem model template

Output51 <- sim(nRep = totalRep, model = CFA.Model, n = list(150, 100), generate = mxTwoGroupModel)
summary(Output51)

# 5.2 Analyze by lavaan

Output52 <- sim(nRep = totalRep, model = script, n = list(150, 100), generate = mxTwoGroupModel, std.lv = TRUE, lavaanfun = "cfa", group = "group")
summary(Output52)

# 5.3 Analyze by list of lavaan arguments

Output53 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE, group = "group"), n = list(150, 100), generate = mxTwoGroupModel, lavaanfun = "cfa")
summary(Output53)

# 5.4 Analyze by OpenMx

Output54 <- sim(nRep = totalRep, model = mxTwoGroupModel, n = list(150, 100), generate = mxTwoGroupModel)
summary(Output54)

# 5.5 Analyze by function using lava

Output55 <- sim(nRep = totalRep, model = FUN, n = list(150, 100), generate = mxTwoGroupModel)
summary(Output55)

# 6. Create data by a function

datfun <- function(n) {
	modelImplied1 <- matrix(
		c(1, 0.49, 0.49, 0.245, 0.245, 0.245,
		0.49, 1, 0.49, 0.245, 0.245, 0.245,
		0.49, 0.49, 1, 0.245, 0.245, 0.245,
		0.245, 0.245, 0.245, 1, 0.49, 0.49, 
		0.245, 0.245, 0.245, 0.49, 1, 0.49, 
		0.245, 0.245, 0.245, 0.49, 0.49, 1), 6, 6, byrow=TRUE)
	modelImplied2 <- matrix(
		c(1, 0.55, 0.55, 0.245, 0.245, 0.245,
		0.55, 1, 0.55, 0.245, 0.245, 0.245,
		0.55, 0.55, 1, 0.245, 0.245, 0.245,
		0.245, 0.245, 0.245, 1, 0.55, 0.55, 
		0.245, 0.245, 0.245, 0.55, 1, 0.55, 
		0.245, 0.245, 0.245, 0.55, 0.55, 1), 6, 6, byrow=TRUE)

	scores1 <- cbind(mvrnorm(n[1], rep(0, 6), modelImplied1), 1)
	scores2 <- cbind(mvrnorm(n[2], rep(0, 6), modelImplied2), 2)
	scores <- rbind(scores1, scores2)
	colnames(scores) <- c(paste0("y", 1:6), "group")
	scores <- data.frame(scores)
	attr(scores, "param") <- c("load1.g1" = 0.7, "load1.g2" = 0.75)
	attr(scores, "stdparam") <- c("load1.g1" = 0.7, "load1.g2" = 0.72)
	scores
}

# 6.1 Analyze by simsem model template

Output61 <- sim(nRep = totalRep, model = CFA.Model, n = list(150, 100), generate = datfun)
summary(Output61)

# 6.2 Analyze by lavaan

Output62 <- sim(nRep = totalRep, model = script, n = list(150, 100), generate = datfun, std.lv = TRUE, lavaanfun = "cfa", group = "group")
summary(Output62)

# 6.3 Analyze by list of lavaan arguments

Output63 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE, group = "group"), n = list(150, 100), generate = datfun, lavaanfun = "cfa")
summary(Output63)

# 6.4 Analyze by OpenMx

Output64 <- sim(nRep = totalRep, model = mxTwoGroupModel, n = list(150, 100), generate = datfun)
summary(Output64)

# 6.5 Analyze by function using lava

Output65 <- sim(nRep = totalRep, model = FUN, n = list(150, 100), generate = datfun)
summary(Output65)
