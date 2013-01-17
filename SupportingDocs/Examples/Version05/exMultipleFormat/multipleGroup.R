library(simsem)

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

Output13 <- sim(nRep = totalRep, model = list(model = script, std.lv = TRUE, group = "group"), n = 200, generate = CFA.Model, lavaanfun = "cfa")
summary(Output13)

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
