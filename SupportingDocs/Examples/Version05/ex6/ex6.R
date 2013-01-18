library(simsem)

# The simulateData function still has a bug in setting multiple-group parameters

# Thus, in this example, the population data of each group are created manually and are fed to the sim function.

# a) Noninvariance

popNoninvariance1 <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3 + 0.7*y4 + 0.7*y5 + 0.7*y6
f1 ~~ 1*f1
y1 ~~ 0.51*y1
y2 ~~ 0.51*y2
y3 ~~ 0.51*y3
y4 ~~ 0.51*y4
y5 ~~ 0.51*y5
y6 ~~ 0.51*y6
f1 ~ 0*1
y1 ~ -0.2*1
y2 ~ -0.1*1
y3 ~ 0*1
y4 ~ 0.1*1
y5 ~ 0.2*1
y6 ~ 0.3*1
"

popNoninvariance2 <- "
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
f1 ~ 0*1
f2 ~ 0*1
y1 ~ 0.2*1
y2 ~ 0.1*1
y3 ~ 0*1
y4 ~ -0.1*1
y5 ~ -0.2*1
y6 ~ -0.3*1
"

# Population data are created with 100,000 cases in each group

popDataNoninvariance1 <- simulateData(popNoninvariance1, sample.nobs = 100000)
popDataNoninvariance2 <- simulateData(popNoninvariance2, sample.nobs = 100000)
popDataNoninvariance <- rbind(popDataNoninvariance1, popDataNoninvariance2)
popDataNoninvariance <- data.frame(popDataNoninvariance, group = rep(c(1, 2), each = 100000))

analyzeNoninvariance <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ c(1, NA)*f2
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
y5 ~~ y5
y6 ~~ y6
f1 ~ 0*1
f2 ~ 0*1
y1 ~ 1
y2 ~ 1
y3 ~ 1
y4 ~ 1
y5 ~ 1
y6 ~ 1
"

outNoninvariance <- lavaan(analyzeNoninvariance, data = popDataNoninvariance, group = "group")

Output.a <- sim(1000, analyzeNoninvariance, n=list(200, 200), rawData = popDataNoninvariance, group = "group", std.lv = TRUE, lavaanfun = "lavaan") 
# Output.a <- sim(1000, list(model = analyzeNoninvariance, group = "group", std.lv = TRUE), n=list(200, 200), rawData = popDataNoninvariance)
getCutoff(Output.a, 0.05)
plotCutoff(Output.a, 0.05)
summary(Output.a)

# b) Configural Invariance

popConfigural1 <- "
f1 =~ 0.7*y1 + 0.6*y2 + 0.5*y3
f2 =~ 0.7*y4 + 0.6*y5 + 0.5*y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.51*y1
y2 ~~ 0.64*y2
y3 ~~ 0.75*y3
y4 ~~ 0.51*y4
y5 ~~ 0.64*y5
y6 ~~ 0.75*y6
f1 ~ 0*1
f2 ~ 0*1
y1 ~ -0.2*1
y2 ~ -0.1*1
y3 ~ 0*1
y4 ~ 0.1*1
y5 ~ 0.2*1
y6 ~ 0.3*1
"

popConfigural2 <- "
f1 =~ 0.5*y1 + 0.6*y2 + 0.7*y3
f2 =~ 0.5*y4 + 0.6*y5 + 0.7*y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.75*y1
y2 ~~ 0.64*y2
y3 ~~ 0.51*y3
y4 ~~ 0.75*y4
y5 ~~ 0.64*y5
y6 ~~ 0.51*y6
f1 ~ 0*1
f2 ~ 0*1
y1 ~ 0.2*1
y2 ~ 0.1*1
y3 ~ 0*1
y4 ~ -0.1*1
y5 ~ -0.2*1
y6 ~ -0.3*1
"

popDataConfigural1 <- simulateData(popConfigural1, sample.nobs = 100000)
popDataConfigural2 <- simulateData(popConfigural2, sample.nobs = 100000)
popDataConfigural <- rbind(popDataConfigural1, popDataConfigural2)
popDataConfigural <- data.frame(popDataConfigural, group = rep(c(1, 2), each = 100000))

analyzeConfigural <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ f2
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
y5 ~~ y5
y6 ~~ y6
f1 ~ 0*1
f2 ~ 0*1
y1 ~ 1
y2 ~ 1
y3 ~ 1
y4 ~ 1
y5 ~ 1
y6 ~ 1
"

outConfigural <- lavaan(analyzeConfigural, data = popDataConfigural, group = "group")

Output.b <- sim(1000, analyzeConfigural, n=list(200, 200), rawData = popDataConfigural, group = "group", std.lv = TRUE, lavaanfun = "lavaan") 
# Output.b <- sim(1000, list(model = analyzeConfigural, group = "group", std.lv = TRUE), n=list(200, 200), rawData = popDataConfigural)
getCutoff(Output.b, 0.05)
plotCutoff(Output.b, 0.05)
summary(Output.b)

# c) Weak Invariance

popWeak1 <- "
f1 =~ 0.7*y1 + 0.6*y2 + 0.5*y3
f2 =~ 0.7*y4 + 0.6*y5 + 0.5*y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.51*y1
y2 ~~ 0.64*y2
y3 ~~ 0.75*y3
y4 ~~ 0.51*y4
y5 ~~ 0.64*y5
y6 ~~ 0.75*y6
f1 ~ 0*1
f2 ~ 0*1
y1 ~ -0.2*1
y2 ~ -0.1*1
y3 ~ 0*1
y4 ~ 0.1*1
y5 ~ 0.2*1
y6 ~ 0.3*1
"

popWeak2 <- "
f1 =~ 0.7*y1 + 0.6*y2 + 0.5*y3
f2 =~ 0.7*y4 + 0.6*y5 + 0.5*y6
f1 ~~ 4*f1
f2 ~~ 4*f2
f1 ~~ 2*f2
y1 ~~ 0.51*y1
y2 ~~ 0.64*y2
y3 ~~ 0.75*y3
y4 ~~ 0.51*y4
y5 ~~ 0.64*y5
y6 ~~ 0.75*y6
f1 ~ 0*1
f2 ~ 0*1
y1 ~ 0.2*1
y2 ~ 0.1*1
y3 ~ 0*1
y4 ~ -0.1*1
y5 ~ -0.2*1
y6 ~ -0.3*1
"

popDataWeak1 <- simulateData(popWeak1, sample.nobs = 100000)
popDataWeak2 <- simulateData(popWeak2, sample.nobs = 100000)
popDataWeak <- rbind(popDataWeak1, popDataWeak2)
popDataWeak <- data.frame(popDataWeak, group = rep(c(1, 2), each = 100000))

analyzeWeak <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f1 ~~ c(1, NA)*f1
f2 ~~ c(1, NA)*f2
f1 ~~ f2
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
y5 ~~ y5
y6 ~~ y6
f1 ~ 0*1
f2 ~ 0*1
y1 ~ 1
y2 ~ 1
y3 ~ 1
y4 ~ 1
y5 ~ 1
y6 ~ 1
"

outWeak <- lavaan(analyzeWeak, data = popDataWeak, group = "group", group.equal = "loadings")

Output.c <- sim(1000, analyzeWeak, n=list(200, 200), rawData = popDataWeak, group = "group", group.equal = "loadings", lavaanfun = "lavaan") 
# Output.c <- sim(1000, list(model = analyzeWeak, group = "group", group.equal = "loadings"), n=list(200, 200), rawData = popDataWeak)
getCutoff(Output.c, 0.05)
plotCutoff(Output.c, 0.05)
summary(Output.c)

# d) Strong Invariance

popStrong1 <- "
f1 =~ 0.7*y1 + 0.6*y2 + 0.5*y3
f2 =~ 0.7*y4 + 0.6*y5 + 0.5*y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.51*y1
y2 ~~ 0.64*y2
y3 ~~ 0.75*y3
y4 ~~ 0.51*y4
y5 ~~ 0.64*y5
y6 ~~ 0.75*y6
f1 ~ 0*1
f2 ~ 0*1
y1 ~ -0.2*1
y2 ~ -0.1*1
y3 ~ 0*1
y4 ~ 0.1*1
y5 ~ 0.2*1
y6 ~ 0.3*1
"

popStrong2 <- "
f1 =~ 0.7*y1 + 0.6*y2 + 0.5*y3
f2 =~ 0.7*y4 + 0.6*y5 + 0.5*y6
f1 ~~ 4*f1
f2 ~~ 4*f2
f1 ~~ 2*f2
y1 ~~ 0.51*y1
y2 ~~ 0.64*y2
y3 ~~ 0.75*y3
y4 ~~ 0.51*y4
y5 ~~ 0.64*y5
y6 ~~ 0.75*y6
f1 ~ 0.5*1
f2 ~ -0.5*1
y1 ~ -0.2*1
y2 ~ -0.1*1
y3 ~ 0*1
y4 ~ 0.1*1
y5 ~ 0.2*1
y6 ~ 0.3*1
"

popDataStrong1 <- simulateData(popStrong1, sample.nobs = 100000)
popDataStrong2 <- simulateData(popStrong2, sample.nobs = 100000)
popDataStrong <- rbind(popDataStrong1, popDataStrong2)
popDataStrong <- data.frame(popDataStrong, group = rep(c(1, 2), each = 100000))

analyzeStrong <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f1 ~~ c(1, NA)*f1
f2 ~~ c(1, NA)*f2
f1 ~~ f2
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
y5 ~~ y5
y6 ~~ y6
f1 ~ c(0, NA)*1
f2 ~ c(0, NA)*1
y1 ~ 1
y2 ~ 1
y3 ~ 1
y4 ~ 1
y5 ~ 1
y6 ~ 1
"

outStrong <- lavaan(analyzeStrong, data = popDataStrong, group = "group", group.equal = c("loadings", "intercepts"))

Output.d <- sim(1000, analyzeStrong, n=list(200, 200), rawData = popDataStrong, group = "group", group.equal = c("loadings", "intercepts"), lavaanfun = "lavaan") 
# Output.d <- sim(1000, list(model = analyzeStrong, group = "group", group.equal = c("loadings", "intercepts")), n=list(200, 200), rawData = popDataStrong)
getCutoff(Output.d, 0.05)
plotCutoff(Output.d, 0.05)
summary(Output.d)
