library(simsem)

popModel <- "
f1 =~ 0.8*y1 + 0.8*y2 + 0.8*y3
f2 =~ 1*y4
f3 =~ 1*y5
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 0.5*f3
f1 ~~ 0.2*f2
f3 ~ 0.5*f1 + 0.5*f2
y1 ~~ 0.36*y1
y2 ~~ 0.36*y2
y3 ~~ 0.36*y3
y4 ~~ 0*y4
y5 ~~ 0*y5
"

analyzeModel <- "
f1 =~ y1 + y2 + y3
f2 =~ 1*y4
f3 =~ 1*y5
f1 ~~ 1*f1
f2 ~~ f2
f3 ~~ f3
f1 ~~ f2
f3 ~ f1 + f2
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ 0*y4
y5 ~~ 0*y5
"

dist <- c("norm", "norm", "norm", "chisq", "norm")
n01 <- list(mean=0, sd=1)
c5 <- list(df=5)
indDist <- bindDist(dist, n01, n01, n01, c5, n01)

Output <- sim(1000, n=200, analyzeModel, indDist=indDist, generate=popModel, estimator="mlm", lavaanfun="lavaan")
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
