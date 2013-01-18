library(simsem)

popModel <- "
f1 =~ 1*y1 + 0.6*y2 + 0.7*y3
f2 =~ 1*y4 + 1.1*y5 + 0.9*y6
f3 =~ 1*y7 + 1.2*y8 + 1.1*y9
f1 ~~ 0.8*f1
f2 ~~ 0.9*f2
f3 ~~ 0.4*f3
f1 ~~ 0.4*f2
f1 ~~ 0.2*f3
f2 ~~ 0.3*f3
y1 ~~ 0.5*y1
y2 ~~ 1.1*y2
y3 ~~ 0.8*y3
y4 ~~ 0.4*y4
y5 ~~ 0.4*y5
y6 ~~ 0.8*y6
y7 ~~ 0.8*y7
y8 ~~ 0.5*y8
y9 ~~ 0.6*y9
"

analyzeModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f3 =~ y7 + y8 + y9
"

Output <- sim(1000, analyzeModel, n=200, generate=popModel, lavaanfun = "cfa")
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

