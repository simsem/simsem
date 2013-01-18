library(simsem)

popModel <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3 + 0.7*y4
f2 =~ 0.7*y5 + 0.7*y6 + 0.7*y7 + 0.7*y8
f3 =~ 0.7*y9 + 0.7*y10 + 0.7*y11 + 0.7*y12
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 1*f3
f1 ~~ 0.5*f2
f1 ~~ 0.5*f3
f2 ~~ 0.5*f3
y1 ~~ 0.51*y1
y2 ~~ 0.51*y2
y3 ~~ 0.51*y3
y4 ~~ 0.51*y4
y5 ~~ 0.51*y5
y6 ~~ 0.51*y6
y7 ~~ 0.51*y7
y8 ~~ 0.51*y8
y9 ~~ 0.51*y9
y10 ~~ 0.51*y10
y11 ~~ 0.51*y11
y12 ~~ 0.51*y12
f1 ~ 1*1
f2 ~ 2*1
f3 ~ 3*1
y1 ~ 0.5*1
y2 ~ 0.4*1
y3 ~ 0.3*1
y4 ~ 0.2*1
y5 ~ 0.1*1
y6 ~ 0*1
y7 ~ 0.1*1
y8 ~ 0.2*1
y9 ~ 0.3*1
y10 ~ 0.4*1
y11 ~ 0.5*1
y12 ~ 0.6*1
"

analyzeModel <- "
f1 =~ y1 + y2 + y3 + y4
f2 =~ y5 + y6 + y7 + y8
f3 =~ y9 + y10 + y11 + y12
"

distname <- c(rep("t", 4), rep("chisq", 8))

d1 <- list(df=2)
d2 <- list(df=3)
d3 <- list(df=4)
d4 <- list(df=5)
d5 <- list(df=3)
d6 <- list(df=4)
d7 <- list(df=5)
d8 <- list(df=6)
d9 <- list(df=3)
d10 <- list(df=4)
d11 <- list(df=5)
d12 <- list(df=6)

dist <- bindDist(distname, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, reverse=c(rep(FALSE, 8), rep(TRUE, 4)))

Output <- sim(1000, n=200, analyzeModel, indDist=dist, generate=popModel, estimator="mlm", std.lv=TRUE, lavaanfun="cfa")
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

dist2 <- bindDist(skewness = seq(-5, 5, length.out=12), kurtosis = seq(2, 10, length.out=12))

Output2 <- sim(10, n=200, analyzeModel, indDist=dist2, generate=popModel, estimator="mlm", std.lv=TRUE, lavaanfun="cfa")
getCutoff(Output2, 0.05)
plotCutoff(Output2, 0.05)
summary(Output2)
