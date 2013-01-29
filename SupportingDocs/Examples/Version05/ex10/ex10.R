library(simsem)

popModel <- "
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
y7 ~~ 1*y7
y1 ~~ 0.40*y7
y2 ~~ 0.40*y7
y3 ~~ 0.40*y7
y4 ~~ 0.40*y7
y5 ~~ 0.40*y7
y6 ~~ 0.40*y7
"

analyzeModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
"

missmodel <- miss(pmMAR=0.1, cov="y7", threshold = 0.5)

Output <- sim(1000, analyzeModel, n=200, generate=popModel, std.lv=TRUE, lavaanfun = "cfa", miss = missmodel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
